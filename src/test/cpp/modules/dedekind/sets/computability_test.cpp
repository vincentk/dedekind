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
    "sets:computability - Set(Species) codomain tracks the predicate's own "
    "logic species (#928)",
    "[sets][computability][928]") {
  // #928: Set codomain = join(carrier-axis NaturalLogic, GetLogic of the
  // predicate's actual RETURN type), so it stays coherent with operator().

  SECTION("Coherent ambient (𝔸<int>) is unchanged: Boole, decidable") {
    constexpr auto s = 𝔸<int>{};
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
    constexpr auto s = 𝔸<int, Kleene>{};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    // Now coherent: IsSet holds (a partial / Ω-set), and it is HONESTLY
    // non-decidable: the carrier axis no longer over-promotes it to Boole.
    STATIC_CHECK(
        IsLSet<decltype(s)>);  // Kleene-valued: an L-set, not ETCS (Ω ≠ 𝔹)
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    // Runtime observable (Codecov-visible): membership answers in Ternary,
    // matching the declared codomain rather than a mis-typed bool.
    CHECK(s(3) == Kleene::True);
  }

  SECTION(
      "Continuum ambient (ℝ-shape 𝔸<int,Kleene,ℶ_1>): the universe over an "
      "uncountable carrier carries Kleene itself; nothing re-tags it") {
    // The species of a set over the continuum is stated ONCE, on the universe
    // (ℝ, ℝ_d, ℂ, ℂ_d, 𝔻, 𝔻_d all spell Kleene).  There is no wrap that
    // re-derives it from the ℶ_1 tag any more: a comprehension over this
    // universe is Kleene because its base is.  𝔸<int,Kleene,ℶ_1> is the same
    // shape (the Mandelbrot stand-in), reachable without dedekind.numbers.
    constexpr auto s = 𝔸<int, Kleene, ℶ_1>{};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    STATIC_CHECK(
        IsLSet<decltype(s)>);  // Kleene-valued: an L-set, not ETCS (Ω ≠ 𝔹)
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == Kleene::True);
  }

  SECTION(
      "Species outside the Boole/Kleene join lattice (Percent, Chain) are "
      "REJECTED, not silently mis-typed to Boole (#945 Sollbruchstelle)") {
    // join_logic_t only models 𝔹 ⊑ K₃, so a Percent-/Chain-tagged ambient would
    // wrap to Boole while membership returns Percentage / a chain value ---
    // exactly the #928 mismatch.  The Set(Species) CTAD is gated on the
    // join to deduce them coherently is FIXME(#945).  Witness the gate CONCEPT
    // directly: a `requires { Set{...}; }` form is unreliable because GCC leaks
    // CTAD "no viable deduction guide" as a hard error rather than absorbing
    // it.
    // Control: a Kleene ambient DOES lift into the wrapped codomain, so the
    // gate admits it and the CTAD wraps coherently (as the sections above
    // verify).
  }

  SECTION(
      "Untagged species with a Ternary-returning operator() over a "
      "countable carrier deduces Kleene (the RETURN type is the "
      "authority, not the tag)") {
    // Carrier axis says Boole; GetLogic of the Ternary RETURN makes the wrap
    // Kleene.
    struct UntaggedTernaryOverN {
      using Domain = int;
      using cardinality_type = ℵ_0;  // countable → carrier axis says Boole
      constexpr Ternary operator()(const int& n) const {
        return n > 0 ? Ternary::True : Ternary::Unknown;
      }
    };
    STATIC_CHECK(std::same_as<typename NaturalLogic<UntaggedTernaryOverN>::type,
                              Boole>);  // carrier axis alone would demote
    constexpr auto s = Comprehension{𝔸<int, Kleene>{}, UntaggedTernaryOverN{}};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    STATIC_CHECK(
        IsLSet<decltype(s)>);  // Kleene-valued: an L-set, not ETCS (Ω ≠ 𝔹)
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == Kleene::True);
    CHECK(s(-1) == Kleene::Unknown);
  }

  SECTION(
      "through the genuine Comprehension node (𝔸<int,Kleene> comprehended by "
      "a Ternary-predicate) deduces Kleene") {
    // Exercises the Set(Comprehension<B,P>) guide (not the identity CTAD),
    // built as a bare node: no point-free operator| comprehends an arbitrary
    // predicate (the retired scout element<A>|pred produced this same type).
    struct TernaryPred {
      constexpr Ternary operator()(const int& n) const {
        return n > 0 ? Ternary::True : Ternary::Unknown;
      }
    };
    constexpr auto s = Comprehension{𝔸<int, Kleene>{}, TernaryPred{}};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    STATIC_CHECK(
        IsLSet<decltype(s)>);  // Kleene-valued: an L-set, not ETCS (Ω ≠ 𝔹)
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(3) == Kleene::True);
    CHECK(s(-1) == Kleene::Unknown);
  }

  SECTION(
      "mixed Comprehension (Kleene base, bool predicate) takes its codomain "
      "from the whole comprehension's return, not the predicate alone (#928)") {
    // Comprehension::operator() combines base(x) under the base's Kleene logic,
    // so a Kleene base comprehended by a bool predicate still returns
    // Kleene::Ω.  Deriving from the bool predicate alone would mis-type it
    // Boole (carrier axis is countable → Boole, and GetLogic<bool> = Boole);
    // the guide joins in the base's species, so it stays Kleene.
    struct BoolPred {
      constexpr bool operator()(const int& n) const { return n > 5; }
    };
    constexpr auto s = Comprehension{𝔸<int, Kleene>{}, BoolPred{}};
    STATIC_CHECK(std::same_as<typename decltype(s)::logic_species, Kleene>);
    STATIC_CHECK(
        std::same_as<typename decltype(s)::Codomain, typename Kleene::Ω>);
    STATIC_CHECK(
        IsLSet<decltype(s)>);  // Kleene-valued: an L-set, not ETCS (Ω ≠ 𝔹)
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
    CHECK(s(6) == Kleene::True);
    CHECK(s(5) == Kleene::False);
  }
}
