/** @file dedekind/numbers/quotient_meeting_point_test.cpp
 *
 * The quotient categorification meeting-point test (#718 Slice 6 —
 * final slice of the quotient Form-chain epic).  Mirrors #698 Slice 10's
 * lattice meeting-point: four canonical carriers exercise the rows
 * they honestly support, jointly populating the row tower from
 * @c IsEquivalenceRelation through @c IsExactCategory.
 *
 * The four canonical carriers:
 *
 *   1. @c bool                — the smallest Boolean algebra / @c 𝔽₂ / ℤ/2ℤ.
 *   2. @c Modular<6>          — the canonical finite cyclic ring.
 *   3. @c Rational<int>       — ℚ as @c Frac(ℤ), an HSP-H carrier.
 *   4. parity-quotient @c mod_2 — the @c int @c → @c bool homomorphism
 *      from Slice 4's First-Iso crown.
 *
 * Each carrier fires the rows it honestly supports.  No carrier fires
 * every row — the Form-chain is genuinely populated by the family
 * rather than dominated by any single witness.  Category-level concepts
 * (@c IsFactorisationSystem, @c IsRegularCategory, @c IsExactCategory)
 * fire on @c CanonicalSetCCC<X> for @c X in the family; per-carrier
 * concepts (@c IsCongruence, @c IsQuotientAlgebra) fire on the
 * carriers themselves.
 */

#include <catch2/catch_test_macros.hpp>

#include <functional>

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.numbers;

using namespace dedekind::category;
using dedekind::morphologies::Modular;
using dedekind::numbers::Rational;

TEST_CASE("quotient meeting-point — bool fires equivalence + congruence rows",
          "[quotient][meeting-point][bool]") {
  /** @brief @c bool as the smallest Boolean algebra / @c 𝔽₂.
   *
   *  Fires Form-chain rows 1 (equivalence) and 2 (congruence) directly.
   *  Indirectly @c bool inhabits the First-Iso crown (Slice 4) as
   *  @c Modular<2> ≅ bool. */
  STATIC_CHECK(IsBinaryRelation<std::equal_to<bool>, bool, bool>);
  STATIC_CHECK(IsEquivalenceRelation<std::equal_to<bool>, bool>);
  STATIC_CHECK(IsCongruence<std::equal_to<bool>, bool, std::plus<bool>>);
  STATIC_CHECK(IsCongruence<std::equal_to<bool>, bool, std::multiplies<bool>>);
  STATIC_CHECK(IsCongruence<std::equal_to<bool>, bool, std::bit_xor<bool>>);
  STATIC_CHECK(IsCongruence<std::equal_to<bool>, bool, std::bit_and<bool>>);
}

TEST_CASE(
    "quotient meeting-point — Modular<6> fires HSP propagation (Slice 5)",
    "[quotient][meeting-point][modular][HSP]") {
  /** @brief @c Modular<6> is the canonical cyclic-ring inhabitant
   *         of the associative-commutative @c + variety.  Slice 5's
   *         HSP-closed crown uses @c Modular<6> as the base, exercised
   *         here directly. */
  using Z6 = Modular<6>;
  STATIC_CHECK(is_associative_v<Z6, std::plus<Z6>>);
  STATIC_CHECK(is_commutative_v<Z6, std::plus<Z6>>);
  STATIC_CHECK(is_distributive_v<Z6, std::multiplies<Z6>, std::plus<Z6>>);
}

TEST_CASE(
    "quotient meeting-point — Rational<int> fires IsQuotientAlgebra (HSP-H)",
    "[quotient][meeting-point][rational][quotient]") {
  /** @brief @c Rational<int> @c = @c Frac(int) is the canonical
   *         quotient-algebra witness (carrier-side declaration of
   *         @c quotient_algebra_base<Rational<I>>::type @c = @c I).
   *         Fires @c IsQuotientAlgebra; the HSP-H propagation lifts
   *         species traits from @c int to @c Rational<int> wherever
   *         the base has them. */
  using Q = Rational<int>;
  STATIC_CHECK(IsQuotientAlgebra<Q>);
}

TEST_CASE(
    "quotient meeting-point — mod_2 parity homomorphism (First-Iso, Slice 4)",
    "[quotient][meeting-point][parity][first-iso]") {
  /** @brief The @c mod_2 homomorphism @c int @c → @c bool from
   *         Slice 4's First-Iso crown.  Witnesses
   *         @c Modular<2> @c ≅ @c bool typed via
   *         @c WitnessesFirstIso<mod_2_arrow, connector>.  The
   *         parity quotient is the cross-cutter tying the
   *         carrier-axis (bool, Modular<2>) to the homomorphism-axis
   *         (mod_2_arrow). */
  using dedekind::morphologies::first_iso_mod_2::connector;
  using dedekind::morphologies::first_iso_mod_2::mod_2_arrow;

  STATIC_CHECK(IsArrow<mod_2_arrow>);
  STATIC_CHECK(IsIsomorphism<connector>);
  STATIC_CHECK(WitnessesFirstIso<mod_2_arrow, connector>);

  // Runtime witness of the parity homomorphism — keeps codecov honest
  // about the Form-chain's per-carrier exercise (mod_2 already covered
  // in first_iso_test.cpp; this is the meeting-point's cross-reference).
  mod_2_arrow f{};
  CHECK_FALSE(f(0));  // even
  CHECK(f(1));        // odd
}

TEST_CASE(
    "quotient meeting-point — Set as the categorical context (rows 6–8)",
    "[quotient][meeting-point][set][category-level]") {
  /** @brief Category-level rows 6 (@c IsFactorisationSystem), 7
   *         (@c IsRegularCategory), 8 (@c IsExactCategory) fire on
   *         @c CanonicalSetCCC<A> for @c A in the canonical-carrier
   *         family.  Slice 2 registered the @c IsExactCategory witness
   *         for @c CanonicalSetCCC<A> uniformly; the upgrade chain
   *         lifts to @c IsRegularCategory and @c IsFactorisationSystem
   *         automatically. */
  STATIC_CHECK(IsExactCategory<CanonicalSetCCC<int>>);
  STATIC_CHECK(IsExactCategory<CanonicalSetCCC<bool>>);
  STATIC_CHECK(IsExactCategory<CanonicalSetCCC<Modular<6>>>);
  STATIC_CHECK(IsExactCategory<CanonicalSetCCC<Rational<int>>>);

  STATIC_CHECK(IsRegularCategory<CanonicalSetCCC<int>>);
  STATIC_CHECK(IsRegularCategory<CanonicalSetCCC<Modular<6>>>);

  STATIC_CHECK(IsFactorisationSystem<CanonicalSetCCC<bool>>);
  STATIC_CHECK(IsFactorisationSystem<CanonicalSetCCC<Rational<int>>>);
}

TEST_CASE(
    "quotient meeting-point — the four carriers jointly populate "
    "the quotient Form-chain rows 1–8",
    "[quotient][meeting-point][form-chain][crown]") {
  /** @brief The meeting-point crown: four canonical carriers
   *         exercising the quotient Form-chain together.  No carrier
   *         fires every row, but the family jointly populates rows
   *         1 through 8 — equivalence relations, congruences, HSP
   *         propagation, the First-Iso theorem, the (regular epi,
   *         mono) factorisation system on Set, regularity, and
   *         exactness.
   *
   *         Per #698 Slice 10 precedent: the meeting-point bundles
   *         the row tower's coverage so a regression in any concept
   *         body or trait registration fails one of the four legs
   *         here. */
  using dedekind::morphologies::first_iso_mod_2::connector;
  using dedekind::morphologies::first_iso_mod_2::mod_2_arrow;
  using Z6 = Modular<6>;
  using Q = Rational<int>;

  // Row 1 (IsEquivalenceRelation) — bool leg
  STATIC_CHECK(IsEquivalenceRelation<std::equal_to<bool>, bool>);

  // Row 2 (IsCongruence) — bool leg
  STATIC_CHECK(IsCongruence<std::equal_to<bool>, bool, std::bit_xor<bool>>);

  // Row 5 (HSP propagation) — Modular<6> leg
  STATIC_CHECK(is_associative_v<Z6, std::plus<Z6>>);

  // Row 5 / IsQuotientAlgebra — Rational leg
  STATIC_CHECK(IsQuotientAlgebra<Q>);

  // WitnessesFirstIso crown (Slice 4 cross-ref) — parity-quotient leg
  STATIC_CHECK(WitnessesFirstIso<mod_2_arrow, connector>);

  // Rows 6, 7, 8 (factorisation system / regular / exact) — Set leg
  STATIC_CHECK(IsExactCategory<CanonicalSetCCC<int>>);
  STATIC_CHECK(IsRegularCategory<CanonicalSetCCC<bool>>);
  STATIC_CHECK(IsFactorisationSystem<CanonicalSetCCC<Modular<6>>>);
}
