/**
 * @file dedekind/category/factorisation.cppm
 * @partition :factorisation
 * @brief (Regular epi, mono) factorisation system theory — the
 *        categorical context for the image-as-coequalizer-of-kernel-pair
 *        theorem narrated in @c :image (#718 Slice 1).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section factorisation__Motivation
 *
 * In a category @b C with finite limits, an arrow @c e is a
 * @b regular @b epi when it is the coequalizer of @b some parallel
 * pair; dually, @c m is a @b regular @b mono when it is the
 * equalizer of @b some parallel pair.  When every arrow of @b C
 * factors uniquely (up to iso) as @c m @c ∘ @c e with @c e a regular
 * epi and @c m a mono, the pair (regular epis, monos) constitutes a
 * @b factorisation @b system on @b C, and @b C is called a
 * @b regular @b category.  When in addition every equivalence
 * relation is the kernel pair of some arrow (i.e.\ every equivalence
 * relation is @b effective), @b C is called @b exact.
 *
 * @b Set is exact, and every (elementary) topos is exact, so every
 * topos is regular and admits the canonical (regular epi, mono)
 * factorisation — this is the categorical context that makes the
 * image-as-coequalizer-of-kernel-pair theorem (narrated in
 * @c :image) the right structural payoff for the quotient
 * categorification.
 *
 * @section factorisation__Form_chain_position
 *
 * Row 4–8 of the quotient Form-chain (#718):
 *
 *   @c IsKernelPair (row 3, in @c :pullback)
 *       ↓ + coequalizer-of-parallel-pair role
 *   @c IsRegularEpi (row 4)
 *       ↓ + dual on the mono side
 *   @c IsRegularMono (row 5)
 *       ↓ + paired into a factorisation
 *   @c IsFactorisationSystem (row 6)
 *       ↓ + universally in a category
 *   @c IsRegularCategory (row 7 — @b Set, every topos)
 *       ↓ + every equivalence relation is effective
 *   @c IsExactCategory (row 8 — @b Set is exact; every topos is exact)
 *
 * @section factorisation__Structural_vs_Universal
 *
 * The concepts here pin the @b operational shape (an arrow is a
 * regular epi when it is the coequalizer of some parallel pair) but
 * not the universal property's @c $\forall$ — existence and
 * uniqueness of the factorisation map.  C++ concepts cannot quantify
 * over the universal property; downstream witnesses opt-in via the
 * variable-trait registry to record the @b intent that their carrier
 * realises the structural role.  This is the @c IsNNO honesty pattern
 * the project uses for every universal-property concept.
 *
 * @section factorisation__Layering
 *
 * Upstream-of-everything-numeric.  Lives in @c :category alongside
 * @c :image and @c :pullback.  Carrier witnesses register
 * downstream where the carriers are available.  Sister to
 * @c :morphism (which already names @c IsEpicArrow / @c IsMonicArrow
 * at the structural-shape level); @c :factorisation refines those by
 * naming the @b regular variants (coequalizer / equalizer of a
 * parallel pair, not just any epi / mono).
 *
 * @note "In a regular category, the regular-epimorphisms and the
 *        monomorphisms form a factorization system."
 *       — Borceux, @em Handbook @em of @em Categorical @em Algebra,
 *         vol 2, §2.1.  (See also Mac Lane, @em CWM, Ex §IV.6; the
 *         (regular epi, mono) factorisation is the canonical
 *         factorisation system in @b Set and in any topos.)
 */
module;

#include <concepts>

export module dedekind.category:factorisation;

import :morphism;  // IsArrow, IsEpicArrow, IsMonicArrow, Identity
import :pullback;  // IsParallelPair (the input shape for coequalizer /
                   // equalizer)

namespace dedekind::category {

// ---------------------------------------------------------------------------
// Regular epi / regular mono — refinements of IsEpicArrow / IsMonicArrow.
//
// Wikipedia regular category: "A regular epimorphism is one that is the
// coequalizer of some parallel pair of morphisms."  Dually for regular
// monomorphisms.  C++ concepts cannot quantify over the existential
// "some parallel pair", so the structural shape is pinned by the
// upstream epi / mono predicates plus an opt-in variable trait
// recording the @b intent.
// ---------------------------------------------------------------------------

/**
 * @brief User-declared witness: @c E is a regular epi (the coequalizer
 *        of some parallel pair).
 *
 * @details Cannot be checked at compile time in general — the existence
 * of a parallel pair @c (f, g) such that @c E is @c coeq(f, g) is the
 * engineer's honesty obligation, opted-in at the carrier registration
 * site.  In Sets and any topos, every epi is regular; in a general
 * category, regular epis are a strict subclass of epis.
 */
export template <typename E>
inline constexpr bool is_regular_epi_v = false;

/**
 * @concept IsRegularEpi
 * @brief An arrow @c E is a @b regular @b epi when it is an epi
 *        @b and registered as the coequalizer of some parallel pair.
 *
 * @details Refines @c IsEpicArrow by the opt-in regularity witness.
 *          Every regular epi is an epi, but not every epi is regular
 *          (a textbook foil: in @b Top, the inclusion of @c ℚ into
 *          @c ℝ is an epi but not a regular epi).  In @b Set and any
 *          topos, every epi is regular.
 *
 *          Form-chain row 4 in the quotient categorification (#718).
 */
export template <typename E>
concept IsRegularEpi = IsEpicArrow<E> && is_regular_epi_v<E>;

/**
 * @brief User-declared witness: @c M is a regular mono (the equalizer
 *        of some parallel pair).  Dual to @c is_regular_epi_v.
 */
export template <typename M>
inline constexpr bool is_regular_mono_v = false;

/**
 * @concept IsRegularMono
 * @brief Dual of @c IsRegularEpi: an arrow @c M is a @b regular @b mono
 *        when it is a mono @b and the equalizer of some parallel pair.
 *
 * @details In @b Set and any topos, every mono is regular.  Form-chain
 *          row 5 in the quotient categorification (#718).
 */
export template <typename M>
concept IsRegularMono = IsMonicArrow<M> && is_regular_mono_v<M>;

// ---------------------------------------------------------------------------
// Factorisation system — the (E, M) pair on a category C.
//
// Borceux vol 2 §2.1: "A factorization system on a category C is a pair
// (E, M) of classes of morphisms such that every morphism factors as
// m ∘ e with e ∈ E and m ∈ M, uniquely up to iso, with E and M closed
// under composition with isomorphisms and orthogonal to each other."
// The canonical example in @b Set / any topos is (regular epi, mono).
// ---------------------------------------------------------------------------

/**
 * @brief User-declared witness: category @c C admits a (regular epi,
 *        mono) factorisation system — every arrow @c f of @c C factors
 *        as @c f @c = @c m @c ∘ @c e with @c e a regular epi and @c m
 *        a mono, uniquely up to iso.
 *
 * @details The categorical anchor for the image-as-coequalizer reading
 * (per @c :image's narration): the factorisation @c A @c → @c Im(F)
 * @c → @c B is exactly the (regular epi, mono) factorisation of @c F.
 */
export template <typename C>
inline constexpr bool is_factorisation_system_v = false;

/**
 * @concept IsFactorisationSystem
 * @brief Category @c C admits the canonical (regular epi, mono)
 *        factorisation system.  Form-chain row 6 (#718).
 */
export template <typename C>
concept IsFactorisationSystem = is_factorisation_system_v<C>;

// ---------------------------------------------------------------------------
// Regular and exact categories — Sollbruchstellen named here; downstream
// carriers register the witnesses for @b Set, the @b Cartesian closed
// power, and any topos.
// ---------------------------------------------------------------------------

/**
 * @brief User-declared witness: @c C is a @b regular @b category — has
 *        finite limits, kernel pairs and coequalizers of kernel pairs,
 *        and regular epis are stable under pullback.
 *
 * @details In any regular category, the (regular epi, mono) pair is a
 *          factorisation system (Wikipedia, regular category).  The
 *          full property's $\forall$-quantifiers are the engineer's
 *          honesty obligation; this trait is the opt-in witness.
 *          Form-chain row 7 (#718).
 */
export template <typename C>
inline constexpr bool is_regular_category_v = false;

/**
 * @concept IsRegularCategory
 * @brief Category @c C satisfies the regular-category axioms.  Pinned
 *        for @b Set and (transitively) every topos.
 */
export template <typename C>
concept IsRegularCategory = is_regular_category_v<C>;

/**
 * @brief Every regular category admits the (regular epi, mono)
 *        factorisation system (Wikipedia: regular category).  The
 *        upgrade fires automatically; downstream callers need only
 *        register @c is_regular_category_v.
 */
template <typename C>
  requires IsRegularCategory<C>
inline constexpr bool is_factorisation_system_v<C> = true;

/**
 * @brief User-declared witness: @c C is an @b exact @b category — a
 *        regular category in which every equivalence relation is the
 *        kernel pair of some arrow (every equivalence relation is
 *        @b effective).
 *
 * @details @b Set is exact, and every elementary topos is exact
 *          (Borceux vol 2 §2; "In Set and more generally any topos,
 *          every epimorphism is the coequalizer of its kernel pair").
 *          Form-chain row 8 (#718).
 */
export template <typename C>
inline constexpr bool is_exact_category_v = false;

/**
 * @concept IsExactCategory
 * @brief Category @c C satisfies the exact-category axioms.  Sollbruchstelle
 *        landing site; the @b Set witness ships now, individual topos
 *        witnesses follow when they're needed.
 */
export template <typename C>
concept IsExactCategory = is_exact_category_v<C>;

/**
 * @brief Every exact category is a regular category (Borceux vol 2 §2.6);
 *        the upgrade fires automatically.
 */
template <typename C>
  requires IsExactCategory<C>
inline constexpr bool is_regular_category_v<C> = true;

// ---------------------------------------------------------------------------
// Trivial canonical witnesses: Identity<T> is both a regular epi and a
// regular mono on every carrier T.  Coequalizer of (id, id) is id; the
// degenerate parallel pair witnesses regularity at both ends.
// ---------------------------------------------------------------------------

template <typename T>
inline constexpr bool is_regular_epi_v<Identity<T>> = true;

template <typename T>
inline constexpr bool is_regular_mono_v<Identity<T>> = true;

// Identity<T> is the canonical regular epi and regular mono witness;
// the (id ∘ id) factorisation is the trivial-factorisation row.
static_assert(IsRegularEpi<Identity<int>>,
              "Identity<int> is the trivial regular epi — coequalizer "
              "of the degenerate parallel pair (id, id).");
static_assert(IsRegularMono<Identity<int>>,
              "Identity<int> is the trivial regular mono — equalizer "
              "of the degenerate parallel pair (id, id).");

}  // namespace dedekind::category
