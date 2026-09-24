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
  SECTION("Boole Ø satisfies the concept") {
    STATIC_CHECK(HasDecidableMembership<Ø<int>>);
    STATIC_CHECK(HasDecidableMembership<Ø<int, Boole>>);
  }

  SECTION("Kleene Ø fails the concept") {
    STATIC_CHECK_FALSE(HasDecidableMembership<Ø<int, Kleene>>);
  }

  SECTION("Intensional Set over a countable carrier satisfies the concept") {
    constexpr auto x = element<ℕ>;
    constexpr auto s = Set{x | [](const auto& v) { return v > 5u; }};
    // ℕ is countably infinite (ℵ_0) → NaturalLogic picks Boole on
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
    STATIC_CHECK(IsExtensional<Ø<int, Kleene>>);
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
  STATIC_CHECK(IsExtensional<Ø<int, Kleene>> &&
               !HasDecidableMembership<Ø<int, Kleene>>);
}

TEST_CASE("sets:computability — IsDecidableSet: Σ-set vs Ω-set (#846)",
          "[sets][computability][846]") {
  // IsDecidableSet = IsSet && HasDecidableMembership: the strict, Boolean
  // reading (χ factors through the dominance Σ ↪ Ω; Σ = Ω only in strict
  // ETCS).  IsSet is the honest
  // base and does NOT imply it — an ETCS set over Kleene is IsSet but
  // has non-decidable (Ω-valued) membership.
  SECTION("Σ-sets: ETCS sets over Boole are decidable") {
    STATIC_CHECK(IsDecidableSet<decltype(ℕ)>);
    STATIC_CHECK(IsDecidableSet<decltype(𝔸<bool>)>);
  }
  SECTION("Ω-set: same carrier, Ternary ambient — IsSet but not decidable") {
    STATIC_CHECK(IsSet<decltype(𝔸<bool, Kleene>)>);
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(𝔸<bool, Kleene>)>);
    STATIC_CHECK_FALSE(IsDecidableSet<decltype(𝔸<bool, Kleene>)>);
  }
}

TEST_CASE("sets:computability — NaturalLogic carrier-axis cut (#622)",
          "[sets][computability][resolver][622]") {
  // Positive witnesses: countable carriers route to Boole on the
  // carrier axis (Rice's theorem caps further promotion of opaque-λ
  // predicates; the carrier-axis verdict is the cheap structural witness).
  SECTION("Countable carriers → Boole") {
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<UniversalSet<int>>::type, Boole>);
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<UniversalSet<unsigned>>::type,
                     Boole>);
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<UniversalSet<bool>>::type, Boole>);
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
    // ℶ_1-tagged UniversalSet models the "carrier with ℝ-shaped
    // cardinality" — the Mandelbrot canonical case is @c
    // UniversalSet<Complex<...>, _, ℶ_1>, mechanically equivalent here.
    STATIC_CHECK(
        std::same_as<typename NaturalLogic<UniversalSet<int, Boole, ℶ_1>>::type,
                     Kleene>);
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
    "sets:computability - Set(Species) codomain tracks the predicate's own "
    "logic species (#928)",
    "[sets][computability][928]") {
  // The @c Set(Species) identity CTAD used to derive the wrapping logic from
  // the carrier axis (@c NaturalLogic) alone, which can DEMOTE a predicate that
  // carries its own @c logic_species: a countable carrier (@c int → ℵ_0 →
  // Boole) tagged with pessimistic @c Kleene logic.  The species' @c operator()
  // returns @c Kleene::Ω (Ternary), which @c lift_logic passes through
  // unchanged, so a @c Boole-declared Set advertised @c Codomain = @c bool
  // while its membership returned @c Ternary; the wrapper then failed
  // @c IsSet.  #928 wraps at the @c join of the carrier-axis verdict and the
  // predicate's own species, so the codomain is expressive enough for both:
  // Kleene wins the countable-Kleene case, while the continuum promotion (ℝ,
  // below) is preserved.

  SECTION("Coherent ambient (𝔸<int>) is unchanged: Boole, decidable") {
    constexpr auto s = Set{𝔸<int>};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Boole>);
    STATIC_CHECK(std::same_as<typename decltype(s)::Codomain, bool>);
    STATIC_CHECK(IsSet<decltype(s)>);
    STATIC_CHECK(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == true);  // runtime observable (Codecov-visible)
  }

  SECTION(
      "Incoherent ambient (𝔸<int,Kleene>): Kleene species wins the "
      "codomain, wrapper stays a coherent Ω-set") {
    // Countable carrier + pessimistic Kleene logic: NaturalLogic's carrier
    // axis says Boole, but the predicate carries Kleene.  Post-#928 the Set
    // adopts Kleene, so Codomain = Kleene::Ω (Ternary) matches operator().
    constexpr auto s = Set{𝔸<int, Kleene>};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    // Now coherent: IsSet holds (a partial / Ω-set), and it is HONESTLY
    // non-decidable: the carrier axis no longer over-promotes it to Boole.
    STATIC_CHECK(IsSet<decltype(s)>);
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    // Runtime observable (Codecov-visible): membership answers in Ternary,
    // matching the declared codomain rather than a mis-typed bool.
    CHECK(s(3) == Kleene::True);
  }

  SECTION(
      "Continuum ambient tagged Boole (ℝ-shape 𝔸<int,Boole,ℶ_1>) stays "
      "Kleene: the carrier axis is NOT demoted by the predicate's Boole") {
    // The dual guard: the fix is the JOIN of the carrier axis and the
    // predicate's species, not the predicate's species verbatim.  The
    // canonical continuum ambient ℝ = 𝔸<QuadraticReal,Boole,ℶ_1> declares Boole
    // logic but is semi-decidable via its ℶ_1 cardinality; NaturalLogic reads
    // Kleene off the carrier axis, and the join with the Boole tag must stay
    // Kleene (a verbatim-logic_species fix would wrongly make ℝ decidable).
    // 𝔸<int,Boole,ℶ_1> is the same shape (the Mandelbrot stand-in), reachable
    // without importing dedekind.numbers.
    constexpr auto s = Set{𝔸<int, Boole, ℶ_1>};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    STATIC_CHECK(IsSet<decltype(s)>);
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == Kleene::True);
  }

  SECTION(
      "Species outside the Boole/Kleene join lattice (Percent, Chain) are "
      "REJECTED, not silently mis-typed to Boole (#945 Sollbruchstelle)") {
    // join_logic_t only models 𝔹 ⊑ K₃, so a Percent-/Chain-tagged ambient would
    // wrap to Boole while membership returns Percentage / a chain value ---
    // exactly the #928 mismatch.  The Set(Species) CTAD is gated on the
    // CoherentSetWrap contract, so it rejects those ambients; generalising the
    // join to deduce them coherently is FIXME(#945).  Witness the gate CONCEPT
    // directly: a `requires { Set{...}; }` form is unreliable because GCC leaks
    // CTAD "no viable deduction guide" as a hard error rather than absorbing
    // it.
    STATIC_CHECK_FALSE(CoherentSetWrap<UniversalSet<int, Percent>>);
    STATIC_CHECK_FALSE(CoherentSetWrap<UniversalSet<int, Chain<int>>>);
    // Control: a Kleene ambient DOES lift into the wrapped codomain, so the
    // gate admits it and the CTAD wraps coherently (as the sections above
    // verify).
    STATIC_CHECK(CoherentSetWrap<UniversalSet<int, Kleene>>);
  }

  SECTION(
      "Untagged species with a Ternary-returning operator() over a "
      "countable carrier deduces Kleene (the RETURN type is the "
      "authority, not the tag)") {
    // The closed hole: no logic_species tag, and NaturalLogic reads Boole off
    // the countable carrier, yet operator() returns Ternary.  A tag-based
    // derivation would demote the codomain to Boole and truncate the Ternary
    // answer; deriving from the RETURN type (GetLogic<Ternary> = Kleene, joined
    // with the carrier axis) yields Kleene, so the wrapper is coherent.
    struct UntaggedTernaryOverN {
      using Domain = int;
      using cardinality_type = ℵ_0;  // countable → carrier axis says Boole
      constexpr Ternary operator()(const int& n) const {
        return n > 0 ? Ternary::True : Ternary::Unknown;
      }
    };
    STATIC_CHECK(std::same_as<typename NaturalLogic<UntaggedTernaryOverN>::type,
                              Boole>);  // carrier axis alone would demote
    STATIC_CHECK(CoherentSetWrap<UntaggedTernaryOverN>);
    constexpr auto s = Set{UntaggedTernaryOverN{}};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    STATIC_CHECK(IsSet<decltype(s)>);
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == Kleene::True);
    CHECK(s(-1) == Kleene::Unknown);
  }
}
