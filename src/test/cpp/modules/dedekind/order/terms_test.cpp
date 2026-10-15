/** @file dedekind/order/terms_test.cpp
 *
 * The term grammar (sets:terms) with the rules order states beside its
 * certificates: the comparisons on a partially ordered carrier (:poset),
 * Euclidean division and remainder on a dividable chain (:completeness).
 * Terms compose at any nesting, a constant meets the carrier of the term beside
 * it, the checks on a constant's value raise where they cannot be compile
 * errors, and a term built at run time is read by the same algebras.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <stdexcept>
#include <type_traits>
#include <utility>

import dedekind.category;   // Ternary, the K₃ carrier
import dedekind.order;      // the comparison and division rules
import dedekind.relational; // IsFunctional: a graph atom over two carriers
import dedekind.sets;

using namespace dedekind::sets;
using P = std::pair<unsigned, unsigned>;

TEST_CASE("order:terms: terms compose and evaluate at a point",
          "[sets][terms]") {
  const auto step = cond(π_1 % 2 == 0, π_1 / 2, π_1 * 3 + 1) == π_2;
  CHECK(step(P{6, 3}));
  CHECK(step(P{7, 22}));
  CHECK_FALSE(step(P{7, 21}));
  CHECK(((π_1 + 1) * 3 == π_2)(P{1, 6}));
  CHECK((π % 3 == 0)(9));
  CHECK_FALSE((π % 3 == 0)(10));
  CHECK(slots_v<decltype(π_1 * 3 + 1)> == 0b10u);
}

// The witnesses of sets:terms that need order's rules.
static_assert(!(π_1 < 5)(P{5, 0}) && (!(π_1 < 5))(P{5, 0}),
              "!atom flips the comparison");
static_assert((π % 3 == 0)(9) && !(π % 3 == 0)(10),
              "an atom on the unary π evaluates at a carrier value");
static_assert((cond(π_1 % 2 == 0, π_1 / 2, π_1 * 3 + 1) == π_2)(P{6, 3}) &&
                  (cond(π_1 % 2 == 0, π_1 / 2, π_1 * 3 + 1) == π_2)(P{7, 22}),
              "cond on terms: the Collatz step as one atom");
static_assert(is_total_term_v<P, decltype(π_1 % 3)> &&
                  !is_total_term_v<P, decltype(π_1 % π_2)> &&
                  !is_total_term_v<P, decltype((π_1 % π_2) + 1)>,
              "a remainder by a nonzero constant is total, by a term it is not "
              "certified (the divisor could be 0), and the verdict propagates "
              "up without asking the value algebra");
static_assert(slots_v<decltype(cond(π_1 < 1, π_1, π_2))> == 0b110u &&
                  is_total_term_v<P, decltype(cond(π_1 < 1, π_1, π_2))>,
              "a conditional mentions the slots of all three legs");
// The rules, read directly: order's and not a field's on the word.
static_assert(admits(On<unsigned>{}, std::less<>{}, false) &&
                  admits(On<unsigned>{}, EuclideanDivision{}, true) &&
                  !admits(On<unsigned>{}, EuclideanDivision{}, false) &&
                  admits(On<unsigned>{}, std::modulus<>{}, true) &&
                  !admits(On<unsigned>{}, FieldDivision{}, true) &&
                  admits(On<unsigned>{}, std::divides<>{}, true),
              "on a word: the order comparisons, // and % by a constant, and "
              "its own division; a field's division is not its rule.");
// ℕ's proxy is a rig and a dividable chain, not a group: + · % are in its
// signature, monus is not until its variety is named.
static_assert(
    admits(On<Cardinality>{}, std::plus<>{}, false) &&
        admits(On<Cardinality>{}, std::multiplies<>{}, false) &&
        admits(On<Cardinality>{}, std::modulus<>{}, true) &&
        admits(On<Cardinality>{}, std::less<>{}, false) &&
        !admits(On<Cardinality>{}, std::minus<>{}, false) &&
        !admits(On<Cardinality>{}, FieldDivision{}, true),
    "ℕ: + · % and the order are in the signature; monus and a field's "
    "division are not.");

TEST_CASE("order:terms: a constant meets the carrier beside it",
          "[sets][terms][constants]") {
  const auto succ = π_1 + 1 == π_2;
  CHECK(succ(std::pair{finite_cardinality(4), finite_cardinality(5)}));
  CHECK(carrier_cast<Cardinality>(3) == finite_cardinality(3));
  CHECK_THROWS_AS(carrier_cast<Cardinality>(-1), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<unsigned char>(300), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<bool>(2), std::domain_error);
}

TEST_CASE("order:terms: a constant is checked when the term is bound",
          "[sets][terms][constants]") {
  CHECK_THROWS_AS(𝔸<Cardinality>{} | (π + -1 == 3), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<int>(1.5), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<int>(1e12), std::domain_error);
  CHECK(carrier_cast<int>(2.0) == 2);
  // The unselected branch is never evaluated: its constant −1 would raise when
  // it met the unsigned carrier.
  CHECK_NOTHROW((cond(π_1 >= 0, π_1, π_1 + -1) == π_2)(P{3, 3}));
  const auto both = !(π_1 < 3 && π_2 > 5);
  CHECK(both(P{3, 0}));
  CHECK_FALSE(both(P{2, 6}));
}

namespace {
// A predicate the grammar does not know, which must not be reached.
struct Unreachable {
  using is_rel_predicate = void;
  bool operator()(const P&) const { throw std::logic_error("reached"); }
};
}  // namespace

TEST_CASE("order:terms: the connectives short-circuit like the built-ins",
          "[sets][terms][connectives]") {
  CHECK_FALSE((π_1 < 3 && Unreachable{})(P{3, 0}));  // left false: right unread
  CHECK((π_1 < 3 || Unreachable{})(P{2, 0}));        // left true: right unread
  CHECK_THROWS_AS((π_1 < 3 && Unreachable{})(P{2, 0}), std::logic_error);
}

namespace {
/** @brief A carrier with equality and no order. */
struct Unordered {
  int v;
  constexpr Unordered(int x = 0)
      : v(x) {}  // NOLINT(google-explicit-constructor)
  friend bool operator==(const Unordered&, const Unordered&) = default;
};
using T = Term<int>;
using Q = Pred<int>;
}  // namespace

TEST_CASE("order:terms: a term built at run time is read by the same algebras",
          "[sets][terms][runtime]") {
  // π_2 == π_1 · 3 + 1, typed and built at run time: one grammar, two shapes.
  const auto typed = π_1 * 3 + 1 == π_2;
  const Q built =
      Q::cmp(Rel::Eq,
             T::bin(Arith::Add, T::bin(Arith::Mul, T::proj(1), T::constant(3)),
                    T::constant(1)),
             T::proj(2));
  for (unsigned x = 0; x < 5; ++x)
    for (unsigned y = 0; y < 20; ++y) CHECK(typed(P{x, y}) == built(P{x, y}));
  // An atom on the unary π, the former, and the connectives in negation
  // normal form.
  const Q even = Q::cmp(Rel::Eq, T::bin(Arith::Mod, T::proj(0), T::constant(2)),
                        T::constant(0));
  const Q gt2 = Q::cmp(Rel::Gt, T::proj(0), T::constant(2));
  const auto evens = 𝔸<unsigned>{} | even;
  CHECK(evens(4u));
  CHECK_FALSE(evens(5u));
  CHECK((𝔸<unsigned>{} | even.negated())(5u));
  CHECK(Q::conj(even, gt2)(4u));
  CHECK_FALSE(Q::conj(even, gt2)(2u));
  CHECK(Q::disj(even, gt2)(3u));
  const Q neither = Q::conj(even, gt2).negated();  // !even || !gt2
  CHECK(neither(3u));
  CHECK(neither(2u));
  CHECK_FALSE(neither(4u));
}

TEST_CASE("order:terms: a term built at run time is checked when bound",
          "[sets][terms][runtime][bind]") {
  // A division by the constant 0 is refused when built; by a term, when bound.
  CHECK_THROWS_AS(T::bin(Arith::Mod, T::proj(0), T::constant(0)),
                  std::domain_error);
  CHECK_THROWS_AS(bind_check<P>(Q::cmp(
                      Rel::Eq, T::bin(Arith::FloorDiv, T::proj(2), T::proj(1)),
                      T::proj(2))),
                  not_admitted);
  // A predicate called directly, without the former, checks each node it
  // visits: the same refusal, at evaluation.
  CHECK_THROWS_AS(
      Q::cmp(Rel::Eq, T::bin(Arith::Div, T::proj(0), T::constant(2)),
             T::constant(1))(4u),
      not_admitted);
  // / is a field's: refused on the word; // and % by a constant are admitted.
  CHECK_THROWS_AS(bind_check<unsigned>(Q::cmp(
                      Rel::Eq, T::bin(Arith::Div, T::proj(0), T::constant(2)),
                      T::constant(1))),
                  not_admitted);
  CHECK_NOTHROW(bind_check<unsigned>(
      Q::cmp(Rel::Eq, T::bin(Arith::FloorDiv, T::proj(0), T::constant(2)),
             T::constant(1))));
  // − is in the word's signature (an abelian group: it wraps).  𝔹's rig is
  // spelled (∨, ∧), so std::plus<bool> is in no signature: + is refused.
  CHECK_NOTHROW(bind_check<P>(Q::cmp(
      Rel::Eq, T::proj(2), T::bin(Arith::Sub, T::proj(1), T::constant(1)))));
  CHECK_THROWS_AS(bind_check<bool>(Q::cmp(
                      Rel::Eq, T::bin(Arith::Add, T::proj(0), T::constant(1)),
                      T::constant(1))),
                  not_admitted);
  // A constant the carrier cannot hold.
  CHECK_THROWS_AS(
      bind_check<unsigned>(Q::cmp(Rel::Gt, T::proj(0), T::constant(-1))),
      std::domain_error);
  // A slot the points have not.
  CHECK_THROWS_AS(
      bind_check<unsigned>(Q::cmp(Rel::Gt, T::proj(1), T::constant(1))),
      not_admitted);
  CHECK_THROWS_AS(bind_check<P>(Q::cmp(Rel::Gt, T::proj(0), T::constant(1))),
                  not_admitted);
  CHECK_THROWS_AS(bind_check<P>(Q::cmp(Rel::Gt, T::proj(3), T::constant(1))),
                  not_admitted);
  // An unordered carrier takes == and != only, read at its values.
  CHECK_THROWS_AS(
      bind_check<Unordered>(Q::cmp(Rel::Lt, T::proj(0), T::constant(1))),
      not_admitted);
  const Q is_one = Q::cmp(Rel::Eq, T::proj(0), T::constant(1));
  CHECK_NOTHROW(bind_check<Unordered>(is_one));
  CHECK(is_one(Unordered{1}));
  CHECK_FALSE(is_one(Unordered{2}));
}

// ── Two carriers in one term ────────────────────────────────────────────────
// The typed grammar is many-sorted: a product's slots each have their own
// carrier, and a term is read per node in that node's carrier.  The word and
// K₃, each slot compared in its own order; and an arrow between the two
// carriers applied to a term, the graph of a function word → K₃.
namespace {
inline constexpr auto 𝕎 = 𝔸<unsigned>{};
inline constexpr auto 𝕂3 = 𝔸<dedekind::category::Ternary>{};
/** The verdict "above five", read in K₃: ⊤ past 5, U at or below. */
struct AboveFive {
  using Domain = unsigned;
  using Codomain = dedekind::category::Ternary;
  constexpr Codomain operator()(const unsigned& x) const {
    return x > 5u ? Codomain::True : Codomain::Unknown;
  }
};
}  // namespace

TEST_CASE("order:terms: a term over two carriers, each slot its own sort",
          "[order][terms][sorts]") {
  using dedekind::category::Ternary;
  using NK = std::pair<unsigned, Ternary>;
  // Each slot compared in its own order: the word's, and K₃'s.
  constexpr auto both = 𝕎 * 𝕂3 | ((π_1 < 5u) && (π_2 >= Ternary::Unknown));
  STATIC_CHECK(both(NK{3u, Ternary::True}));
  STATIC_CHECK(both(NK{3u, Ternary::Unknown}));
  STATIC_CHECK(!both(NK{3u, Ternary::False}));
  STATIC_CHECK(!both(NK{7u, Ternary::True}));
  // An arrow between the carriers, applied: the graph of a function 𝕎 → K₃.
  constexpr auto graph = 𝕎 * 𝕂3 | (π_2 == ap(AboveFive{}, π_1));
  STATIC_CHECK(graph(NK{7u, Ternary::True}));
  STATIC_CHECK(graph(NK{2u, Ternary::Unknown}));
  STATIC_CHECK(!graph(NK{7u, Ternary::Unknown}));
  // A constexpr variable's decltype is const; the certificate is on the type.
  using Graph = std::remove_cvref_t<decltype(graph)>;
  STATIC_CHECK(dedekind::relational::IsFunctional<Graph>);
  STATIC_CHECK(dedekind::relational::IsEntire<Graph>);  // f total, into K₃
}
