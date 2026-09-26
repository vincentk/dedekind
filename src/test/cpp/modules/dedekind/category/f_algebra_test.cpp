/**
 * @file f_algebra_test.cpp
 * @brief Test suite for the @c category:f_algebra partition (closes
 *        the universal-property layer for #449).
 *
 * @section Scope
 * Exercises the universal-property reification of F-algebras:
 *
 *   1. @c IsInitialFAlgebra<F, A, Alpha> — opt-in trait defaults to
 *      false; carriers must explicitly register the universal-
 *      property witness.  The structural-shape clause @c IsFAlgebra
 *      (in @c :functor) is mechanically checked; the universal-
 *      property uniqueness clause is the engineer's honesty
 *      obligation.
 *
 *   2. @c IsTerminalFCoalgebra<F, A, Alpha> — dual.
 *
 * The Cardinality witness ( @c Cardinality is the initial F-algebra
 * for @c F(X) @c = @c 1 @c + @c X) requires the @c 1 @c + @c X
 * endofunctor encoding and is filed as a sibling concern.  This
 * suite pins the concept-layer behaviour: opt-in, defaults to false,
 * fires on explicit registration.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.category;

using namespace dedekind::category;

// ===========================================================================
// (1) Default: opt-in trait is false; concepts do not fire without
//     explicit registration.
// ===========================================================================

namespace {
// Use @c unsigned to keep the toy successor / halve shape aligned
// with the textbook @c F(X) @c = @c 1 @c + @c X reading on naturals
// (@c unsigned wraps under overflow, which is well-defined; @c int
// would have signed-overflow UB).  The successor map @c x @c ↦ @c
// x @c + @c 1 is the structural Peano-style step the universal-
// property concept ranges over.
using NatCat = DiscreteCategory<unsigned>;
using IdF = identity_functor<NatCat>;

constexpr auto successor_arrow = arrow([](unsigned x) { return x + 1u; });
constexpr auto halve_arrow = arrow([](unsigned x) { return x / 2u; });
using SuccArrow = std::decay_t<decltype(successor_arrow)>;
using HalveArrow = std::decay_t<decltype(halve_arrow)>;
}  // namespace

TEST_CASE(
    "f_algebra: IsInitialFAlgebra opt-in trait defaults to false (no "
    "universal-property witness without explicit registration)",
    "[category][f_algebra][initial][negative]") {
  // Structural shape fires ( @c IsFAlgebra in @c :functor) — but the
  // universal-property concept @c IsInitialFAlgebra does not, since
  // initiality is the engineer's honesty obligation and must be
  // explicitly registered.
  STATIC_CHECK(IsFAlgebra<unsigned, SuccArrow, IdF>);
  STATIC_CHECK(!is_initial_f_algebra_v<IdF, unsigned, SuccArrow>);
  STATIC_CHECK(!IsInitialFAlgebra<IdF, unsigned, SuccArrow>);
}

TEST_CASE("f_algebra: IsTerminalFCoalgebra opt-in trait defaults to false",
          "[category][f_algebra][terminal][negative]") {
  STATIC_CHECK(IsFCoalgebra<unsigned, HalveArrow, IdF>);
  STATIC_CHECK(!is_terminal_f_coalgebra_v<IdF, unsigned, HalveArrow>);
  STATIC_CHECK(!IsTerminalFCoalgebra<IdF, unsigned, HalveArrow>);
}

// ===========================================================================
// (2) Explicit registration fires the concept.
// ===========================================================================
//
// A test-local toy carrier that opts in to both universal properties
// for a fixed structure map.  The opt-in is what asserts the honesty
// clause; the test simply pins that the concept fires once the
// trait is registered.

namespace test_local {
struct ToyCarrier {
  int value{};
  constexpr bool operator==(ToyCarrier const&) const = default;
};
using ToyCat = DiscreteCategory<ToyCarrier>;
using ToyIdF = identity_functor<ToyCat>;
constexpr auto toy_alpha =
    arrow([](ToyCarrier const& x) { return ToyCarrier{x.value + 1}; });
using ToyAlpha = std::decay_t<decltype(toy_alpha)>;
}  // namespace test_local

namespace dedekind::category {
template <>
inline constexpr bool is_initial_f_algebra_v<
    test_local::ToyIdF, test_local::ToyCarrier, test_local::ToyAlpha> = true;
template <>
inline constexpr bool is_terminal_f_coalgebra_v<
    test_local::ToyIdF, test_local::ToyCarrier, test_local::ToyAlpha> = true;
}  // namespace dedekind::category

TEST_CASE("f_algebra: IsInitialFAlgebra fires on explicit opt-in registration",
          "[category][f_algebra][initial][witness]") {
  using namespace test_local;
  STATIC_CHECK(IsFAlgebra<ToyCarrier, ToyAlpha, ToyIdF>);
  STATIC_CHECK(is_initial_f_algebra_v<ToyIdF, ToyCarrier, ToyAlpha>);
  STATIC_CHECK(IsInitialFAlgebra<ToyIdF, ToyCarrier, ToyAlpha>);
}

TEST_CASE(
    "f_algebra: IsTerminalFCoalgebra fires on explicit opt-in registration",
    "[category][f_algebra][terminal][witness]") {
  using namespace test_local;
  STATIC_CHECK(IsFCoalgebra<ToyCarrier, ToyAlpha, ToyIdF>);
  STATIC_CHECK(is_terminal_f_coalgebra_v<ToyIdF, ToyCarrier, ToyAlpha>);
  STATIC_CHECK(IsTerminalFCoalgebra<ToyIdF, ToyCarrier, ToyAlpha>);
}

// ===========================================================================
// (3) cata ⦇β⦈ over the composition term functor (#961).
//
// The initial-algebra fold applied to a Compose<F,G> node: F(cata) folds
// the two legs, then β = reduce_step (in :morphism) applies the monoid
// unit law id∘f = f = f∘id.  The engine (cata) is law-free; the law rides
// in :morphism next to composition, found by ADL.  simplify == cata.
// ===========================================================================

namespace {
// Two DISTINCT non-identity endo-arrows on int, and the identity arrow.
// endo<int>(λ) yields a Morphism leaf (no π_1 / π_2), so cata treats it as
// an atom; id<int>() yields Identity<int>, likewise a leaf but the unit β
// pattern-matches structurally.
constexpr auto inc = endo<int>([](int x) { return x + 1; });
constexpr auto dbl = endo<int>([](int x) { return x * 2; });

using Inc = std::remove_cvref_t<decltype(inc)>;
using Dbl = std::remove_cvref_t<decltype(dbl)>;
using Id = Identity<int>;

// Reduced-form helper: the normal form cata is expected to produce.
template <typename T, typename Term>
constexpr bool cata_reduces_to = std::same_as<std::remove_cvref_t<T>, Term>;
}  // namespace

TEST_CASE(
    "f_algebra: cata leaves a composite of two non-identity arrows inert "
    "(cata(f >> g) == f >> g)",
    "[category][f_algebra][cata][monoid][961]") {
  const auto fg = inc >> dbl;  // Compose<Inc, Dbl>: apply inc, then dbl
  const auto reduced = cata(fg);

  // Neither leg is the unit, so β rebuilds the composite: same node type.
  STATIC_CHECK(cata_reduces_to<decltype(reduced), Compose<Inc, Dbl>>);
  STATIC_CHECK(cata_reduces_to<decltype(fg), Compose<Inc, Dbl>>);

  // ...and it is still the same map: (dbl ∘ inc)(3) = (3 + 1) * 2 = 8.
  CHECK(reduced(3) == 8);
  CHECK(reduced(3) == fg(3));
}

TEST_CASE("f_algebra: cata drops a right-identity leg (cata(f >> id) == f)",
          "[category][f_algebra][cata][monoid][unit][961]") {
  const auto reduced = cata(inc >> id<int>());  // f ∘ id = f

  STATIC_CHECK(cata_reduces_to<decltype(reduced), Inc>);
  CHECK(reduced(3) == 4);
}

TEST_CASE("f_algebra: cata drops a left-identity leg (cata(id >> g) == g)",
          "[category][f_algebra][cata][monoid][unit][961]") {
  const auto reduced = cata(id<int>() >> dbl);  // id ∘ g = g

  STATIC_CHECK(cata_reduces_to<decltype(reduced), Dbl>);
  CHECK(reduced(3) == 6);
}

TEST_CASE(
    "f_algebra: cata collapses id >> id to the identity, tying back to the "
    ":involution unit witness (#961)",
    "[category][f_algebra][cata][monoid][involution][961]") {
  const auto reduced = cata(id<int>() >> id<int>());  // id ∘ id = id

  STATIC_CHECK(cata_reduces_to<decltype(reduced), Id>);
  CHECK(reduced(3) == 3);

  // The collapsed unit is exactly the arrow :involution certifies as the
  // trivial involution (id⁻¹ = id), so cata's fixed point of the unit law
  // lands on the witnessed atom.
  STATIC_CHECK(is_involutive_v<std::remove_cvref_t<decltype(reduced)>, int>);
  STATIC_CHECK(IsInvolution<Id, int>);
}
