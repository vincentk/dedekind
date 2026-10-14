// The monoid algebra R[M] (algebra:polynomial): polynomials as finitely
// supported nets over the monomials, and the normal form of a ring's terms.
// The carrier here is the word, so the coefficients wrap.
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <stdexcept>

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;

namespace {

using dedekind::algebra::Monomial;
using dedekind::algebra::normal_form;
using dedekind::algebra::Polynomial;
using dedekind::category::IsArrow;
using dedekind::category::IsCommutativeMonoid;
using dedekind::category::IsCommutativeRing;
using dedekind::category::IsFreeAlgebra;
using dedekind::order::IsDirectedSet;
using dedekind::order::IsTotallyOrdered;
using dedekind::sequences::IsNet;
using dedekind::sets::Arith;
using dedekind::sets::not_admitted;
using dedekind::sets::π;
using dedekind::sets::π1;
using dedekind::sets::π2;

using Word = unsigned int;
using M = Monomial<4>;
using P = Polynomial<Word, M>;
using T = dedekind::sets::Term<Word>;

}  // namespace

TEST_CASE("Monomials: the free commutative monoid on the slots, a chain",
          "[algebra][polynomial]") {
  STATIC_CHECK(IsCommutativeMonoid<M, std::multiplies<M>>);
  STATIC_CHECK(IsTotallyOrdered<M>);
  STATIC_CHECK(IsDirectedSet<M>);
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
}

TEST_CASE("Polynomials: a net over the monomials, a ring over a ring",
          "[algebra][polynomial]") {
  STATIC_CHECK(IsArrow<P>);
  STATIC_CHECK(IsNet<P>);
  STATIC_CHECK(IsCommutativeRing<P, std::plus<P>, std::multiplies<P>>);
  STATIC_CHECK(IsFreeAlgebra<P>);
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
  }
}

TEST_CASE("The normal form of terms: equality in the free algebra",
          "[algebra][polynomial][terms]") {
  SECTION("typed terms") {
    CHECK(normal_form<Word>(π + 1u - 1u) == normal_form<Word>(π));
    CHECK(normal_form<Word>((π1 + π2) * (π1 + π2)) ==
          normal_form<Word>(π1 * π1 + 2u * π1 * π2 + π2 * π2));
    CHECK(normal_form<Word>(π * π) != normal_form<Word>(π));
    CHECK(normal_form<Word>(π * 4294967295u + π).is_zero());
  }
  SECTION("terms built at run time read the same") {
    const T t =
        T::bin(Arith::Sub, T::bin(Arith::Add, T::proj(0), T::constant(1u)),
               T::constant(1u));
    CHECK(normal_form<Word>(t) == normal_form<Word>(π));
    CHECK(normal_form<Word>(T::bin(Arith::Mul, T::proj(1), T::proj(2))) ==
          normal_form<Word>(π1 * π2));
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
