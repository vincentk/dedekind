/**
 * @file dedekind/numbers/integer.cppm
 * @partition :integer
 * @brief Minimal number taxonomy concepts for reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist
 * Menschenwerk."
 *       ("God made the integers; all else is the work of man.")
 *       -- Leopold Kronecker, Jahresbericht der DMV 2 (1891, reported)
 */
module;

#include <concepts>
#include <numeric>
#include <type_traits>

export module dedekind.numbers:integer;

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;
import :natural;
export import :cardinality;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

/** @brief Default signed-integer carrier used by downstream numeric
 *  layers (Rational<I>, embeddings).  Post-#670-sibling (ℚ retarget):
 *  anchored on @c sets::SignedCardinality (saturating ℤ proxy with
 *  ±ℵ_0 / NaZ escalation), mirroring how @c ℤ = @c Ω<SignedCardinality>
 *  in @c :integer.  This is the discipline-consistent canonical ℤ
 *  carrier --- @c ℚ = @c Ω<Rational<default_integer>> now uses the
 *  saturating variant uniformly.
 *
 *  Pre-retarget value: @c SignedExtensionalCardinal<> (cyclic finite
 *  fragment).  The retarget is what the user asked for: "re-target
 *  rationals / ℚ in the same way as ℤ, ℕ, 𝔹".
 */
export using default_integer = dedekind::sets::SignedCardinality;

/** @section integer__Saturating_ℤ (#670)
 *
 * @c ℤ is the universe @c Ω<SignedCardinality>, using the @b saturating
 * variant @c sets::SignedCardinality as the carrier (rather than the
 * @b cyclic finite fragment @c SignedExtensionalCardinal<>).  This
 * mirrors the @c ℕ pattern (@c ℕ @c = @c Ω<Cardinality>, where
 * @c Cardinality is the saturating @f$\mathbb{N} \cup \{\aleph_0\}@f$
 * variant) --- the project's stance is now consistent across @c ℕ and
 * @c ℤ: bounded representations @b escalate (saturate to
 * @f$\pm \aleph_0@f$) rather than wrap.
 *
 * The carrier @c sets::SignedCardinality is the project's documented
 * bona-fide proxy for @f$\mathbb{Z}@f$ modulo physical limits
 * (cf.\ @c cardinality.cppm:923-924, "the library's bona-fide proxy
 * for ℤ modulo physical limits") --- arithmetic saturates to
 * @f$\pm \aleph_0@f$ on overflow rather than wrapping modulo
 * @f$2^{N \cdot 64}@f$.  This is what the in-line scout-algebra
 * surface (#664) requires for translation-invariant halfspace-pivot
 * transport: the @c IsOrderedAdditiveGroup marker in
 * @c :algebra:scout_algebra is specialised to @c true for
 * @c SignedCardinality and to @c false (default) for the cyclic
 * finite fragment, exactly because the saturating discipline
 * preserves order under translation and the cyclic one does not at
 * the wrap boundary.
 */
export inline constexpr auto ℤ =
    dedekind::sets::Ω<dedekind::sets::SignedCardinality>;

static_assert(
    std::same_as<std::remove_cvref_t<decltype(ℤ)>,
                 dedekind::sets::UniversalSet<dedekind::sets::SignedCardinality,
                                              ClassicalLogic, ℵ_0>>,
    "ℤ is the universe Ω<SignedCardinality>, mirroring "
    "ℕ = Ω<Cardinality> (#670).");
static_assert(std::same_as<typename std::remove_cvref_t<decltype(ℤ)>::Domain,
                           dedekind::sets::SignedCardinality>,
              "ℤ's underlying carrier IS SignedCardinality — the project's "
              "bona-fide saturating ℤ proxy (per cardinality.cppm:923-924). "
              "Mirrors ℕ's underlying carrier being Cardinality.");

// The saturating ℤ proxy inhabits the algebraic concept chain the
// in-line scout-algebra surface uses (#664).  Pinning the witness:
// IsOrderedAdditiveGroup<SignedCardinality> holds via the
// is_translation_invariant_ordered marker specialised in
// :algebra:scout_algebra.
static_assert(dedekind::algebra::IsOrderedAdditiveGroup<
                  dedekind::sets::SignedCardinality>,
              "ℤ's carrier SignedCardinality must satisfy "
              "IsOrderedAdditiveGroup --- the structural binding "
              "between ℤ and the in-line scout-algebra halfspace "
              "pipe (#664 / #670).");

// ===========================================================================
// Initial Ring + Grothendieck Group witnesses on @c SignedCardinality
// (closes part of #446).
//
// Two universal-property witnesses anchoring @c SignedCardinality
// simultaneously:
//   * @c IsInitialRing<SignedCardinality> — for every ring @c R there
//     exists a unique ring homomorphism @c SignedCardinality @c → @c R
//     (e.g.\ @c χ_{Modular<n>} as the mod-n reduction).
//   * @c IsGrothendieckGroup<SignedCardinality, Cardinality> —
//     @c SignedCardinality is the free abelian group on the
//     commutative monoid @c Cardinality; the closure-forcing operator
//     @c Cardinality @c - @c Cardinality @c → @c SignedCardinality
//     realises the construction at the operator level.
//
// Universal-property content (existence + uniqueness of the canonical
// homomorphisms) is the engineer's honesty obligation; the test
// suite exercises the operational behaviour at concrete targets.
// ===========================================================================

namespace dedekind::numbers {

static_assert(
    dedekind::algebra::IsInitialRing<dedekind::sets::SignedCardinality>,
    "SignedCardinality is the canonical Initial Ring witness: for every "
    "ring R there exists a unique ring homomorphism SignedCardinality → R "
    "(e.g. χ_{Modular<n>} = mod-n reduction).  Universal-property content "
    "is the engineer's honesty obligation.");

static_assert(
    dedekind::algebra::IsGrothendieckGroup<dedekind::sets::SignedCardinality,
                                           dedekind::sets::Cardinality>,
    "SignedCardinality is the canonical Grothendieck group of Cardinality: "
    "the free abelian group on the commutative monoid (Cardinality, +, 0).  "
    "The closure-forcing operator Cardinality - Cardinality → "
    "SignedCardinality realises the Grothendieck construction at the "
    "operator level.");

}  // namespace dedekind::numbers

// ---------------------------------------------------------------------------
// Carrier-lattice lift unification (#455): existential-proof
// specialisation of @c category::lift for the central variant-layer
// pair @c (Cardinality, SignedCardinality).  Demonstrates that the
// discoverability-alias dispatch works on a real lattice arrow;
// remaining six specialisations land as follow-up.
// ---------------------------------------------------------------------------

namespace dedekind::category {
template <>
constexpr dedekind::sets::SignedCardinality
lift<dedekind::sets::Cardinality, dedekind::sets::SignedCardinality>(
    dedekind::sets::Cardinality const& n) {
  return dedekind::numbers::lift_ℕ_ℤ_(n);
}
}  // namespace dedekind::category
