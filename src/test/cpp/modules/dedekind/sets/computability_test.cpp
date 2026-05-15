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

  SECTION("Intensional Set over a countable carrier satisfies the concept") {
    constexpr auto x = element<ℕ>;
    constexpr auto s = Set{x | [](const auto& v) { return v > 5u; }};
    // ℕ is countably infinite (ℵ_0) → NaturalLogic picks ClassicalLogic on
    // the carrier axis (#622).  Rice's theorem caps further promotion of
    // the opaque λ predicate, but the carrier-axis witness is sufficient
    // here: the resolver trusts the carrier and lets the λ run.
    STATIC_CHECK(HasDecidableMembership<decltype(s)>);
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

TEST_CASE("sets:computability — NaturalLogic carrier-axis cut (#622)",
          "[sets][computability][resolver][622]") {
  // Positive witnesses: countable carriers route to ClassicalLogic on the
  // carrier axis (Rice's theorem caps further promotion of opaque-λ
  // predicates; the carrier-axis verdict is the cheap structural witness).
  SECTION("Countable carriers → ClassicalLogic") {
    STATIC_CHECK(std::same_as<typename NaturalLogic<UniversalSet<int>>::type,
                              ClassicalLogic>);
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<UniversalSet<unsigned>>::type,
                     ClassicalLogic>);
    STATIC_CHECK(std::same_as<typename NaturalLogic<UniversalSet<bool>>::type,
                              ClassicalLogic>);
  }

  // Negative witness: Mandelbrot-shaped Sets — uncountable carrier (ℶ_1)
  // + structurally-Π⁰₁ predicate + no set-level shortcut → no axis fires
  // → TernaryLogic.  This is the canonical witness that the resolver
  // doesn't over-promise on structurally-undecidable sets.  Two
  // independent ceilings stack on uncountable carriers:
  //   (a) Rice forbids recognising opaque predicates as Δ⁰₁;
  //   (b) the float↔ℝ gap makes @c double-typed witnesses denote ℝ values
  //       only approximately, so even structurally-Δ⁰₁ comparisons land
  //       exact-as-@c double but unknown-as-ℝ.
  SECTION("Uncountable carriers → TernaryLogic (Mandelbrot-shape witness)") {
    // ℶ_1-tagged UniversalSet models the "carrier with ℝ-shaped
    // cardinality" — the Mandelbrot canonical case is @c
    // UniversalSet<Complex<...>, _, ℶ_1>, mechanically equivalent here.
    STATIC_CHECK(
        std::same_as<
            typename NaturalLogic<UniversalSet<int, ClassicalLogic, ℶ_1>>::type,
            TernaryLogic>);
  }

  // SFINAE fallback: types without @c cardinality_type degrade to the
  // honest default @c TernaryLogic.  Required so @c NaturalLogic-probing
  // @c requires-clauses (e.g.\ the cartesian-product operator gate in
  // @c :expressions) substitute cleanly on non-Set carriers.
  SECTION("No cardinality_type → TernaryLogic fallback") {
    struct NoCardinality {};
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<NoCardinality>::type, TernaryLogic>);
  }
}
