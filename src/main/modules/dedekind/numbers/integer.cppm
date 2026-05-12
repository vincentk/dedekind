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

/** @section integer__Saturating_ℤ_Companion (#670)
 *
 * Companion to @c ℤ above, using the @b saturating variant
 * @c sets::SignedCardinality as the carrier rather than the @b cyclic
 * finite fragment @c SignedExtensionalCardinal<>.  This mirrors the
 * @c ℕ pattern (@c ℕ @c = @c Ω<Cardinality> where @c Cardinality is
 * the saturating @f$\mathbb{N} \cup \{\aleph_0\}@f$ variant) and
 * gives the project a discipline-consistent ℤ proxy at the value
 * level.
 *
 * Why a companion alias rather than retargeting @c ℤ itself: the
 * existing @c ℤ is anchored across ~400 references (witnesses in
 * @c :rational, @c Rational<default_integer>, embeddings, paper
 * listings); retargeting it would cascade and touch many sites
 * outside @c :numbers.  The additive companion is a Sollbruchstelle:
 * downstream code that wants the saturating discipline can opt in via
 * @c ℤ_aleph today, and a future cleanup can migrate the full
 * @c ℤ alias once all downstream sites have been verified.
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
    std::same_as<std::remove_cvref_t<decltype(ℤ_aleph)>,
                 dedekind::sets::UniversalSet<dedekind::sets::SignedCardinality,
                                              ClassicalLogic, ℵ_0>>,
    "ℤ_aleph is the universe Ω<SignedCardinality>, mirroring "
    "ℕ = Ω<Cardinality>.");
static_assert(
    std::same_as<typename std::remove_cvref_t<decltype(ℤ_aleph)>::Domain,
                 dedekind::sets::SignedCardinality>,
    "ℤ_aleph's underlying carrier IS SignedCardinality — the project's "
    "bona-fide saturating ℤ proxy (per cardinality.cppm:923-924). "
    "Mirrors ℕ's underlying carrier being Cardinality, the saturating "
    "ℕ proxy.");

// The saturating ℤ proxy inhabits the algebraic concept chain the
// in-line scout-algebra surface uses (#664).  Pinning the witness:
// IsOrderedAdditiveGroup<SignedCardinality> holds via the
// is_translation_invariant_ordered marker specialised in
// :algebra:scout_algebra.
static_assert(dedekind::algebra::IsOrderedAdditiveGroup<
                  dedekind::sets::SignedCardinality>,
              "ℤ_aleph's carrier SignedCardinality must satisfy "
              "IsOrderedAdditiveGroup --- this is the structural binding "
              "between the new saturating-ℤ alias and the in-line "
              "scout-algebra halfspace pipe (#664).");

/**
 * @brief Canonical embedding K3 ↪ ℤ: Ternary → SignedCardinality.
 * @details Maps False → -1, Unknown → 0, True → 1.  Lands on the
 *          variant ℤ-proxy carrier @c SignedCardinality (closes #430,
 *          a #402 prerequisite); the previous @c int codomain made
 *          the K3 → variant-ℤ chain require an explicit machine-side
 *          extraction step.  The unreachable default returns @c NaZ
 *          for IEEE-NaN-style propagation if a non-canonical
 *          @c Ternary value were ever constructed (cannot happen in
 *          practice — @c Ternary is a closed enum).
 */
export inline constexpr auto embed_𝕂3_ℤ_ =
    arrow<Ternary, dedekind::sets::SignedCardinality>(
        [](const Ternary& t) noexcept -> dedekind::sets::SignedCardinality {
          switch (t) {
            case Ternary::False:
              return dedekind::sets::finite_signed_cardinality(-1);
            case Ternary::Unknown:
              return dedekind::sets::finite_signed_cardinality(0);
            case Ternary::True:
              return dedekind::sets::finite_signed_cardinality(1);
          }
          return dedekind::sets::SignedCardinality{dedekind::sets::NaZ{}};
        });

/**
 * @brief Canonical variant-layer embedding @c ℕ @c ↪ @c ℤ:
 *        @c Cardinality @c → @c SignedCardinality, exposed as a
 *        first-class @c arrow object for the carrier-lattice diagram.
 *
 * @details Wraps @c dedekind::sets::lift_cardinality_to_signed (the
 *          public function definition; lives in @c sets:cardinality
 *          to remain reachable from cross-variant comparison
 *          operators without crossing the @c sets @c → @c numbers
 *          module boundary).  This @c arrow form is the named monic
 *          morphism the carrier-lattice Figure 1 labels at the
 *          variant-layer top row; structurally @b distinct from the
 *          machine-layer @c embed_uint_sint_ above (an
 *          @c arrow<unsigned, @c int> sign reinterpretation).
 *          Registered as monic below.
 */
export inline constexpr auto lift_ℕ_ℤ_ =
    arrow<dedekind::sets::Cardinality, dedekind::sets::SignedCardinality>(
        [](const dedekind::sets::Cardinality& c) noexcept {
          return dedekind::sets::lift_cardinality_to_signed(c);
        });

/**
 * @brief Canonical embedding of any std::signed_integral into ℤ.
 *
 * @details The extensional integer carrier (extensional_integer = int) is the
 * default target. For injecting into a general IsInteger domain Z, use
 * embed_signed_integral<Z>(v).
 *
 * @tparam S Any std::signed_integral source type.
 */
export template <std::signed_integral S>
constexpr extensional_integer embed_signed_to_ℤ(S v) {
  return static_cast<extensional_integer>(v);
}

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool is_monic_arrow_v<
    std::decay_t<decltype(dedekind::numbers::embed_uint_sint_)>> = true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_uint_sint_)>>,
    "embed_uint_sint_ (machine-layer ℕ → ℤ) is registered injective.");

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝕂3_ℤ_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_𝕂3_ℤ_)>>,
    "embed_𝕂3_ℤ_ (𝕂3 → ℤ) is registered injective.");

template <>
inline constexpr bool is_monic_arrow_v<
    std::decay_t<decltype(dedekind::numbers::embed_unsigned_ℕ)>> = true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_unsigned_ℕ)>>,
    "embed_unsigned_ℕ (unsigned → ExtensionalCardinal) is registered "
    "injective.");
// Monicity of @c embed_uint_ℕ_ is registered in @c :uint.

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::lift_ℕ_ℤ_)>> =
        true;
static_assert(IsInjective<std::decay_t<decltype(dedekind::numbers::lift_ℕ_ℤ_)>>,
              "lift_ℕ_ℤ_ (variant-layer ℕ ↪ ℤ; Grothendieck-construction unit) "
              "is registered injective.");
}  // namespace dedekind::category

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

namespace dedekind::algebra {

template <>
inline constexpr bool is_initial_ring_v<dedekind::sets::SignedCardinality> =
    true;

template <>
inline constexpr bool is_grothendieck_group_v<dedekind::sets::SignedCardinality,
                                              dedekind::sets::Cardinality> =
    true;

}  // namespace dedekind::algebra

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
