// The monoid algebra R[M] (algebra:polynomial): polynomials as finitely
// supported nets over the monomials, and the normal form of a ring's terms.
// The carrier here is the word, so the coefficients wrap.
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <stdexcept>

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

namespace {

using dedekind::algebra::Monomial;
using dedekind::algebra::normal_form;
using dedekind::algebra::Polynomial;
using dedekind::sets::Arith;
using dedekind::sets::not_admitted;
using dedekind::sets::π;
using dedekind::sets::π_1;
using dedekind::sets::π_2;

using Word = unsigned int;
using M = Monomial<4>;
using P = Polynomial<Word, M>;
using T = dedekind::sets::Term<Word>;
using Pr = dedekind::sets::Pred<Word>;
using dedekind::algebra::as_term;
using dedekind::sets::Rel;
using dedekind::sets::same_shape;

/** A coefficient is its own constant: the terms here are over the word. */
struct AsConstant {
  Word operator()(Word c) const { return c; }
};

}  // namespace

TEST_CASE("Monomials: the free commutative monoid on the slots, a chain",
          "[algebra][polynomial]") {
  const M x = M::variable(1), y = M::variable(2);
  CHECK(x * y == y * x);
  CHECK((x * y).degree() == 2);
  CHECK((x * y).exponent(1) == 1);
  // The graded lexicographic order: degree first, then the slots.
  CHECK(M::one() < x);
  CHECK(y * y < x * y);
  CHECK(x * y < x * x);
  CHECK(x * x * x > M::variable(0) * M::variable(3));
  CHECK((x * x).without(1) == x);
  CHECK_THROWS_AS(M::variable(4), std::out_of_range);
  CHECK_THROWS_AS(x.exponent(4), std::out_of_range);
}

TEST_CASE("Polynomials: a net over the monomials, a ring over a ring",
          "[algebra][polynomial]") {
  const P x = P::variable(1), y = P::variable(2);

  SECTION("the finite function monomial ↦ coefficient") {
    const P p = (x + y) * (x + y);
    CHECK(p.degree() == 2);
    CHECK(p.support().size() == 3);
    CHECK(p(M::variable(1) * M::variable(2)) == 2u);
    CHECK(p(M::variable(1) * M::variable(1)) == 1u);
    CHECK(p(M::variable(0)) == 0u);  // off the support
    CHECK(p.leading_coefficient() == 1u);
    CHECK(p.support().front().first == M::variable(2) * M::variable(2));
  }
  SECTION("identities and inverses") {
    CHECK(x + P::zero() == x);
    CHECK(x * P::one() == x);
    CHECK((x - x).is_zero());
    CHECK((-x + x).is_zero());
    CHECK(P(0u).is_zero());
    CHECK_THROWS_AS(P::zero().leading_coefficient(), std::domain_error);
  }
  SECTION("the coefficients wrap with the word") {
    CHECK((x * P(4294967295u) + x).is_zero());
  }
  SECTION("the formal derivative, by slot") {
    const P p = x * x * x + P(2u) * x * x + x + P(1u);
    CHECK(p.derive(1) == P(3u) * x * x + P(4u) * x + P(1u));
    CHECK(p.derive(2).is_zero());
    CHECK((x * y).derive(1) == y);
    CHECK(P::one().derive().is_zero());
    CHECK_THROWS_AS(x.derive(4), std::out_of_range);
  }
}

TEST_CASE("The normal form of terms: equality in the free algebra",
          "[algebra][polynomial][terms]") {
  SECTION("typed terms") {
    CHECK(normal_form<Word>(π + 1u - 1u) == normal_form<Word>(π));
    CHECK(normal_form<Word>((π_1 + π_2) * (π_1 + π_2)) ==
          normal_form<Word>(π_1 * π_1 + 2u * π_1 * π_2 + π_2 * π_2));
    CHECK(normal_form<Word>(π * π) != normal_form<Word>(π));
    CHECK(normal_form<Word>(π * 4294967295u + π).is_zero());
  }
  SECTION("terms built at run time read the same") {
    const T t =
        T::bin(Arith::Sub, T::bin(Arith::Add, T::proj(0), T::constant(1u)),
               T::constant(1u));
    CHECK(normal_form<Word>(t) == normal_form<Word>(π));
    CHECK(normal_form<Word>(T::bin(Arith::Mul, T::proj(1), T::proj(2))) ==
          normal_form<Word>(π_1 * π_2));
  }
  SECTION("outside the catalogue: refused, never rewritten") {
    CHECK_THROWS_AS(
        normal_form<Word>(T::bin(Arith::Mod, T::proj(0), T::constant(2u))),
        not_admitted);
    CHECK_THROWS_AS(
        normal_form<Word>(T::bin(Arith::FloorDiv, T::proj(0), T::constant(2u))),
        not_admitted);
    // The word has no field division.
    CHECK_THROWS_AS(
        normal_form<Word>(T::bin(Arith::Div, T::proj(0), T::constant(2u))),
        not_admitted);
    // A division by a term has no polynomial on any carrier.
    CHECK_THROWS_AS(
        normal_form<Word>(T::bin(Arith::Div, T::proj(1), T::proj(2))),
        not_admitted);
  }
}

TEST_CASE("The normal form of predicates: atom by atom, written back as terms",
          "[algebra][polynomial][terms]") {
  const AsConstant lift;
  const T x = T::proj(0), x1 = T::proj(1), x2 = T::proj(2);
  const T one = T::constant(1u), zero = T::constant(0u);
  const auto nf = [&](const Pr& p) { return normal_form<Word>(p, lift); };
  SECTION("a polynomial written back reads as itself") {
    const P p = normal_form<Word>((π_1 + π_2) * (π_1 + π_2) + 3u);
    CHECK(normal_form<Word>(as_term<Word>(p, lift)) == p);
    CHECK(normal_form<Word>(as_term<Word>(P::zero(), lift)).is_zero());
  }
  SECTION("an equation moves to a difference equal to 0") {
    CHECK(same_shape(nf(Pr::cmp(Rel::Eq, T::bin(Arith::Add, x, one), one)),
                     nf(Pr::cmp(Rel::Eq, x, zero))));
    CHECK(same_shape(nf(Pr::cmp(Rel::Eq, zero, x)),
                     nf(Pr::cmp(Rel::Eq, x, zero))));
    const T square = T::bin(Arith::Mul, T::bin(Arith::Add, x1, x2),
                            T::bin(Arith::Add, x1, x2));
    const T expanded = T::bin(
        Arith::Add,
        T::bin(Arith::Add, T::bin(Arith::Mul, x1, x1),
               T::bin(Arith::Mul, T::constant(2u), T::bin(Arith::Mul, x1, x2))),
        T::bin(Arith::Mul, x2, x2));
    CHECK(same_shape(nf(Pr::cmp(Rel::Ne, square, x2)),
                     nf(Pr::cmp(Rel::Ne, expanded, x2))));
    // The word is no field: 2π = 0 is not made monic (it is not π = 0).
    CHECK_FALSE(same_shape(
        nf(Pr::cmp(Rel::Eq, T::bin(Arith::Mul, T::constant(2u), x), zero)),
        nf(Pr::cmp(Rel::Eq, x, zero))));
  }
  SECTION("an order atom keeps its comparison, its sides normalised") {
    CHECK(same_shape(
        nf(Pr::cmp(Rel::Lt, T::bin(Arith::Add, x, zero), T::constant(5u))),
        Pr::cmp(Rel::Lt, x, T::constant(5u))));
  }
  SECTION("the connectives are kept, and a remainder refused") {
    const Pr a = Pr::cmp(Rel::Eq, T::bin(Arith::Sub, x, x), zero);
    CHECK(same_shape(
        nf(Pr::conj(a, a)),
        Pr::conj(Pr::cmp(Rel::Eq, zero, zero), Pr::cmp(Rel::Eq, zero, zero))));
    CHECK_THROWS_AS(
        nf(Pr::cmp(Rel::Eq, T::bin(Arith::Mod, x, T::constant(2u)), zero)),
        not_admitted);
  }
}
