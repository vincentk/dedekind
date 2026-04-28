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

import dedekind.category;

using namespace dedekind::category;

// ===========================================================================
// (1) Default: opt-in trait is false; concepts do not fire without
//     explicit registration.
// ===========================================================================

namespace {
// Reuse the existing IdF setup from functor_test.cpp so this suite
// stands on its own without leaking helpers.
using IntCat = DiscreteCategory<int>;
using IdF = identity_functor<IntCat>;

constexpr auto successor_arrow = arrow([](int x) { return x + 1; });
constexpr auto halve_arrow = arrow([](int x) { return x / 2; });
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
  STATIC_CHECK(IsFAlgebra<int, SuccArrow, IdF>);
  STATIC_CHECK(!is_initial_f_algebra_v<IdF, int, SuccArrow>);
  STATIC_CHECK(!IsInitialFAlgebra<IdF, int, SuccArrow>);
}

TEST_CASE("f_algebra: IsTerminalFCoalgebra opt-in trait defaults to false",
          "[category][f_algebra][terminal][negative]") {
  STATIC_CHECK(IsFCoalgebra<int, HalveArrow, IdF>);
  STATIC_CHECK(!is_terminal_f_coalgebra_v<IdF, int, HalveArrow>);
  STATIC_CHECK(!IsTerminalFCoalgebra<IdF, int, HalveArrow>);
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
