/** @file dedekind/sets/computability_test.cpp
 *
 * Unit coverage for the consolidated computability surface
 * (post-2026-05-09): @c HasDecidableMembership in @c :sets:computability
 * and @c IsExtensional in @c :sets:cardinality.  The previous tag-based
 * @c IsCompileTimeEnumerable / @c IsFiniteSet concepts were retired in
 * favour of the @c IsExtensional gate; this file's tests collapse the
 * two prior tier tests into one accordingly.
 *
 * Tests in this file use ONLY @c dedekind.sets + @c dedekind.category so
 * the sets-test target respects the module DAG (sets is upstream of order).
 * Downstream concept-conformance for order-level types (@c Singleton,
 * @c OrderInterval) lives in
 * @c modules/dedekind/order/halfspace_test.cpp; reduction-boundary
 * coverage via the halfspace DSL lives in
 * @c modules/dedekind/analysis/pruning_showcases_test.cpp.
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
    constexpr auto x = element<ℕ>;
    constexpr auto s = Set{x | [](const auto& v) { return v > 5u; }};
    // ℕ is transfinite → NaturalLogic picks TernaryLogic.
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
  }
}

TEST_CASE("sets:cardinality — IsExtensional on Ø",
          "[sets][cardinality][computability]") {
  SECTION("Ø is extensional regardless of logic species") {
    STATIC_CHECK(IsExtensional<Ø<int>>);
    STATIC_CHECK(IsExtensional<Ø<int, TernaryLogic>>);
  }

  SECTION("Intensional Set over a transfinite carrier is not extensional") {
    constexpr auto x = element<ℕ>;
    constexpr auto s = Set{x | [](const auto& v) { return v > 5u; }};
    STATIC_CHECK_FALSE(IsExtensional<decltype(s)>);
  }
}

TEST_CASE("sets:computability — extensionality and decidability are orthogonal",
          "[sets][computability]") {
  // The two surviving tiers are along independent axes
  // (extensionality lives in :sets:cardinality; decidability here).
  // This test exhibits the orthogonality on a concrete witness.
  STATIC_CHECK(IsExtensional<Ø<int>> && HasDecidableMembership<Ø<int>>);
  STATIC_CHECK(IsExtensional<Ø<int, TernaryLogic>> &&
               !HasDecidableMembership<Ø<int, TernaryLogic>>);
}
