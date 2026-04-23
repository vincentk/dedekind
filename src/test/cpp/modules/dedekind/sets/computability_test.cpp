/** @file dedekind/sets/computability_test.cpp
 *
 * Unit coverage for the three-tier computability hierarchy introduced in
 * PR #361: `HasDecidableMembership`, `IsFiniteSet`, `IsCompileTimeEnumerable`.
 *
 * Tests in this file use ONLY `dedekind.sets` + `dedekind.category` so the
 * sets-test target respects the module DAG (sets is upstream of order).
 * Downstream concept-conformance for order-level types (`Singleton`,
 * `OrderInterval`) lives in `modules/dedekind/order/halfspace_test.cpp`;
 * reduction-boundary coverage via the halfspace DSL lives in
 * `modules/dedekind/analysis/pruning_showcases_test.cpp`.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("sets:computability — HasDecidableMembership on Ø",
          "[sets][computability]") {
  SECTION("ClassicalLogic Ø satisfies the concept") {
    STATIC_CHECK(HasDecidableMembership<Ø<int>>);
    STATIC_CHECK(HasDecidableMembership<Ø<int, ClassicalLogic>>);
  }

  SECTION("TernaryLogic Ø fails the concept") {
    STATIC_CHECK_FALSE(HasDecidableMembership<Ø<int, TernaryLogic>>);
  }

  SECTION("Intensional Set over a transfinite carrier fails the concept") {
    constexpr auto x = var<Ω<int>>;
    constexpr auto s = Set{x % N | [](int v) { return v > 5; }};
    // ℕ is transfinite → NaturalLogic picks TernaryLogic.
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
  }
}

TEST_CASE("sets:computability — IsFiniteSet on Ø", "[sets][computability]") {
  SECTION("Ø is finite regardless of logic species") {
    STATIC_CHECK(IsFiniteSet<Ø<int>>);
    STATIC_CHECK(IsFiniteSet<Ø<int, TernaryLogic>>);
  }

  SECTION("Intensional Set over a transfinite carrier is not finite") {
    constexpr auto x = var<Ω<int>>;
    constexpr auto s = Set{x % N | [](int v) { return v > 5; }};
    STATIC_CHECK_FALSE(IsFiniteSet<decltype(s)>);
  }
}

TEST_CASE("sets:computability — IsCompileTimeEnumerable on Ø",
          "[sets][computability]") {
  SECTION("Ø exposes (vacuously) elements at the type level") {
    STATIC_CHECK(IsCompileTimeEnumerable<Ø<int>>);
    STATIC_CHECK(IsCompileTimeEnumerable<Ø<int, TernaryLogic>>);
  }

  SECTION("An intensional Set is neither finite nor compile-time-enumerable") {
    constexpr auto x = var<Ω<int>>;
    constexpr auto s = Set{x % N | [](int v) { return v > 5; }};
    STATIC_CHECK_FALSE(IsFiniteSet<decltype(s)>);
    STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(s)>);
  }
}

TEST_CASE("sets:computability — refinement order of the three tiers",
          "[sets][computability]") {
  // IsCompileTimeEnumerable is definitionally `IsFiniteSet<S> && requires {
  // is_compile_time_extensional_tag }`, so the implication holds
  // structurally. Verified here against concrete witnesses.
  STATIC_CHECK(IsCompileTimeEnumerable<Ø<int>> && IsFiniteSet<Ø<int>>);

  // Finite extensional witnesses in dedekind.sets also happen to satisfy
  // HasDecidableMembership (via ClassicalLogic default); tested as a
  // practical invariant on the shipped types, not a theorem of the concepts.
  STATIC_CHECK(IsFiniteSet<Ø<int>> && HasDecidableMembership<Ø<int>>);
}
