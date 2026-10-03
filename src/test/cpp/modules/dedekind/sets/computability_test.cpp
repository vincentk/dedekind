/** @file dedekind/sets/computability_test.cpp
 *
 * Unit coverage for the computability surface: @c HasDecidableMembership in
 * @c :sets:computability, @c IsExtensional in @c :sets:cardinality, the
 * carrier-axis resolver @c NaturalLogic, and the species rule of
 * @c Comprehension (join of base and answer).
 *
 * Tests in this file use ONLY @c dedekind.sets + @c dedekind.category so
 * the sets-test target respects the module DAG (sets is upstream of order).
 * Downstream concept-conformance for order-level types (@c Singleton,
 * @c Interval) lives in
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
  SECTION("Boole Ø satisfies the concept") {
    STATIC_CHECK(HasDecidableMembership<Ø<int>>);
    STATIC_CHECK(HasDecidableMembership<Ø<int, Boole>>);
  }

  SECTION("Kleene Ø fails the concept") {
    STATIC_CHECK_FALSE(HasDecidableMembership<Ø<int, Kleene>>);
  }

  SECTION("Intensional Set over a countable carrier satisfies the concept") {
    // An @b opaque named predicate (the point of this case: decidability comes
    // from the carrier axis, not the predicate shape --- so NOT a structured
    // point-free halfspace).  @c :sets tests may not import @c :order, so
    // @c π/fix is unavailable here anyway.
    constexpr auto gt_five = [](const auto& v) { return v > 5u; };
    constexpr auto s = Comprehension{ℕ, gt_five};
    // ℕ is countably infinite (ℵ_0) → NaturalLogic picks Boole on
    // the carrier axis (#622).  Rice's theorem caps further promotion of
    // the opaque predicate, but the carrier-axis witness is sufficient
    // here: the resolver trusts the carrier and lets the predicate run.
    STATIC_CHECK(HasDecidableMembership<decltype(s)>);
  }
}

TEST_CASE("sets:cardinality — IsExtensional on Ø",
          "[sets][cardinality][computability]") {
  SECTION("Ø is extensional regardless of logic species") {
    STATIC_CHECK(IsExtensional<Ø<int>>);
    STATIC_CHECK(IsExtensional<Ø<int, Kleene>>);
  }

  SECTION("Intensional Set over a transfinite carrier is not extensional") {
    constexpr auto gt_five = [](const auto& v) { return v > 5u; };
    constexpr auto s = Comprehension{ℕ, gt_five};
    STATIC_CHECK_FALSE(IsExtensional<decltype(s)>);
  }
}

TEST_CASE("sets:computability — extensionality and decidability are orthogonal",
          "[sets][computability]") {
  // The two surviving tiers are along independent axes
  // (extensionality lives in :sets:cardinality; decidability here).
  // This test exhibits the orthogonality on a concrete witness.
  STATIC_CHECK(IsExtensional<Ø<int>> && HasDecidableMembership<Ø<int>>);
  STATIC_CHECK(IsExtensional<Ø<int, Kleene>> &&
               !HasDecidableMembership<Ø<int, Kleene>>);
}

TEST_CASE("sets:computability — IsDecidableSet: Σ-set vs Ω-set (#846)",
          "[sets][computability][846]") {
  // IsDecidableSet = IsLSet && HasDecidableMembership: the Boolean reading
  // of an L-set (χ factors through the dominance Σ ↪ Ω).  IsLSet is the
  // general noun and does NOT imply it — a Kleene-valued L-set has Ω-valued
  // (possibly Unknown) membership; nor is it an ETCS set (IsSet needs Ω = 𝔹).
  SECTION("Σ-sets: ETCS sets over Boole are decidable") {
    STATIC_CHECK(IsDecidableSet<decltype(ℕ)>);
    STATIC_CHECK(IsDecidableSet<𝔸<bool>>);
  }
  SECTION(
      "Ω-set: same carrier, Ternary ambient — an L-set, neither decidable "
      "nor ETCS") {
    STATIC_CHECK(IsLSet<𝔸<bool, Kleene>>);  // an L-set, not ETCS (Ω ≠ 𝔹)
    STATIC_CHECK_FALSE(IsSet<𝔸<bool, Kleene>>);
    STATIC_CHECK_FALSE(HasDecidableMembership<𝔸<bool, Kleene>>);
    STATIC_CHECK_FALSE(IsDecidableSet<𝔸<bool, Kleene>>);
  }
}

TEST_CASE("sets:computability — NaturalLogic carrier-axis cut (#622)",
          "[sets][computability][resolver][622]") {
  // Positive witnesses: countable carriers route to Boole on the
  // carrier axis (Rice's theorem caps further promotion of opaque-λ
  // predicates; the carrier-axis verdict is the cheap structural witness).
  SECTION("Countable carriers → Boole") {
    STATIC_CHECK(std::same_as<typename NaturalLogic<𝔸<int>>::type, Boole>);
    STATIC_CHECK(std::same_as<typename NaturalLogic<𝔸<unsigned>>::type, Boole>);
    STATIC_CHECK(std::same_as<typename NaturalLogic<𝔸<bool>>::type, Boole>);
  }

  // Negative witness: Mandelbrot-shaped Sets — uncountable carrier (ℶ_1)
  // + structurally-Π⁰₁ predicate + no set-level shortcut → no axis fires
  // → Kleene.  This is the canonical witness that the resolver
  // doesn't over-promise on structurally-undecidable sets.  Two
  // independent ceilings stack on uncountable carriers:
  //   (a) Rice forbids recognising opaque predicates as Δ⁰₁;
  //   (b) the float↔ℝ gap makes @c double-typed witnesses denote ℝ values
  //       only approximately, so even structurally-Δ⁰₁ comparisons land
  //       exact-as-@c double but unknown-as-ℝ.
  SECTION("Uncountable carriers → Kleene (Mandelbrot-shape witness)") {
    // ℶ_1-tagged Universe models the "carrier with ℝ-shaped
    // cardinality" — the Mandelbrot canonical case is @c
    // 𝔸<Complex<...>, _, ℶ_1>, mechanically equivalent here.
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<𝔸<int, Boole, ℶ_1>>::type, Kleene>);
  }

  // SFINAE fallback: types without @c cardinality_type degrade to the
  // honest default @c Kleene.  Required so @c NaturalLogic-probing
  // @c requires-clauses (e.g.\ the cartesian-product operator gate in
  // @c :expressions) substitute cleanly on non-Set carriers.
  SECTION("No cardinality_type → Kleene fallback") {
    struct NoCardinality {};
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<NoCardinality>::type, Kleene>);
  }
}

TEST_CASE(
    "sets:computability — a comprehension's species is the JOIN of its base's "
    "and its answer's",
    "[sets][computability][species]") {
  // One rule, stated on Comprehension itself (no wrapper re-derives it): the
  // species is join(base species, species of the predicate's answer), with
  // both answers lifted into it before the conjunction.
  SECTION("Boole base, bool answer: Boole, an ETCS set, decidable") {
    struct GtFive {
      constexpr bool operator()(const int& n) const { return n > 5; }
    };
    constexpr auto s = Comprehension{𝔸<int>{}, GtFive{}};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Boole>);
    STATIC_CHECK(IsSet<decltype(s)>);
    STATIC_CHECK(HasDecidableMembership<decltype(s)>);
    CHECK(s(6) == true);
    CHECK(s(5) == false);
  }
  SECTION("Kleene base, bool answer: the base's Kleene wins (an L-set)") {
    struct GtFive {
      constexpr bool operator()(const int& n) const { return n > 5; }
    };
    constexpr auto s = Comprehension{𝔸<int, Kleene>{}, GtFive{}};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    STATIC_CHECK(IsLSet<decltype(s)> && !IsSet<decltype(s)>);
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(6) == Kleene::True);
    CHECK(s(5) == Kleene::False);
  }
  SECTION(
      "Boole base, Ternary answer: the answer's Kleene wins (Unknown "
      "survives)") {
    struct Partial {
      constexpr Ternary operator()(const int& n) const {
        return n > 0 ? Ternary::True : Ternary::Unknown;
      }
    };
    constexpr auto s = Comprehension{𝔸<int>{}, Partial{}};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == Kleene::True);
    CHECK(s(-1) == Kleene::Unknown);
  }
  SECTION("the universes over the continuum carry Kleene themselves") {
    constexpr auto s = 𝔸<int, Kleene, ℶ_1>{};  // the ℝ shape, without numbers
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == Kleene::True);
  }
}
