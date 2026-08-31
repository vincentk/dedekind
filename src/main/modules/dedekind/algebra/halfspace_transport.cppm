/**
 * @file dedekind/algebra/halfspace_transport.cppm
 * @partition :halfspace_transport
 * @brief Transport of halfspaces along the additive group: the @b ordered-group
 *        slice of the point-free relational DSL (@c image / @c inverse /
 *        @c argmax and the entireness inference).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section halfspace_transport__Why_Here
 * The DSL surface (the @c image / @c inverse / @c argmax / @c is_function /
 * @c is_entire spellings) lives in @c namespace @c dedekind::order so that the
 * point-free calls resolve by ADL on their order/relation argument types.  Its
 * @b implementation, however, lives here in @c dedekind.algebra, because these
 * operations are @b ordered-group operations: they slide a @f$\le@f$-defined
 * halfspace by @c +K, which is sound exactly when the carrier's order is
 * translation-invariant.  That property is the canonical @c
 * algebra::IsOrderedAdditiveGroup (its marker @c
 * is_translation_invariant_ordered naming precisely "a non-cyclic ordered
 * additive group"), and @c algebra sits downstream of @c order, so this is
 * where the gate is in scope --- no order-layer reconstruction.  This is the
 * deliberate front-loading: an order-facing DSL whose transport slice is backed
 * by algebra.
 *
 * @section halfspace_transport__Functional_vs_Entire
 * The relation-property split follows the same logic.  @b Functionality
 * (@c is_right_unique_v, single-valuedness) is @b structural and stays in
 * @c order.  @b Entireness (@c is_left_total_v) is @b algebraic --- a
 * translation
 * @f$x\mapsto x+K@f$ is total iff @c x+K stays in the carrier, an ordered-group
 * fact --- so every @c is_left_total_v specialisation for the DSL's graph
 * relations lives here.  @c IsFunctional on a graph is therefore an order-level
 * query; @c IsEntire (and @c IsFunction) is an algebra-level one.
 *
 * @build_order after :scout_algebra
 * @dependency dedekind.order
 */
module;

#include <concepts>     // std::same_as
#include <functional>   // std::plus
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::pair

export module dedekind.algebra:halfspace_transport;

import dedekind.category; // IsAbelianGroup, is_left_total_v (the trait primary)
import dedekind.sets;     // Set, Ω, Singleton, Cardinality, SignedCardinality
import dedekind.order; // Halfspace, ProjAddConstProj, Rel, dir_of/strict_of/flip
import :scout_algebra;  // IsOrderedAdditiveGroup — the canonical gate

// Mirror order/halfspace.cppm's directives so the DSL names (Set, Ω, Singleton
// from sets; the trait primaries from category) resolve unqualified inside the
// re-opened namespaces.  using-directives are TU-local (never exported).
using namespace dedekind::sets;
using namespace dedekind::category;

namespace dedekind::order {

/**
 * @concept IsEntireTranslationCarrier
 * @brief The carriers on which the translation @f$x \mapsto x+K@f$ is a
 *        @b total (entire) function: @c x+K stays in the carrier for every
 *        @c x.
 *
 * @details Two disjoint witnesses: an @c algebra::IsOrderedAdditiveGroup
 * carrier
 * (@f$\mathbb{Z}@f$-like, closed under @c +K for @b any shift @c K), or the
 * bounded-below @c ℕ = @c Cardinality @b when @c K≥0 (the successor @c x+1 is
 * total on @c ℕ; the predecessor @c x−1 is not).  Excludes the cyclic
 * (wrapping) carriers (@c unsigned, @c bool): @c x+K folds modulo capacity, so
 * neither is a bona-fide total translation with an order-shaped range.
 */
export template <typename T, auto K>
concept IsEntireTranslationCarrier =
    dedekind::algebra::IsOrderedAdditiveGroup<T> ||
    (std::same_as<std::remove_cvref_t<T>, dedekind::sets::Cardinality> &&
     K >= 0);

/** @brief @c inverse of a translation-graph relation = its CONVERSE
 *  @c B*A|P⁻¹: the same graph read backwards, @c x↦x−K, again a GRAPH (a
 *  relation, not an arrow), so it stays on the surface and composes.
 *
 *  @details Functions ARE graphs here, so @c inverse is a relational operation,
 *  the converse with the shift negated.  Gated on @c IsOrderedAdditiveGroup:
 *  negating the shift (@c K→−K) is the predecessor relation only where the
 *  carrier has genuine, order-respecting additive inverses.  On ℕ (@c
 *  Cardinality) there is no inverse below 0; on a wrapping group (@c unsigned)
 *  @c −K folds modulo capacity rather than computing the predecessor, so the
 *  inverse of the successor is NOT its converse there; the overload is
 *  withheld. */
export template <typename T, auto K, typename L>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
constexpr auto inverse(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return Set<std::pair<T, T>, L, ProjAddConstProj<1, -K, Rel::Eq, 2>>{
      ProjAddConstProj<1, -K, Rel::Eq, 2>{}};
}

// image = the RANGE (π_B projection) of a functional graph, read structurally.
// A translation is surjective on ANY additive group (@c IsAbelianGroup under
// +): on ℤ the range of the unbounded graph is the whole line; on a cyclic
// group
// (@c unsigned) the modular translation is still a bijection, hence onto, so Ω
// is the correct range regardless of wrap.  (On ℕ = @c Cardinality, NOT a
// group, x↦x+K misses {0,…,K−1}, so this overload is gated to the group case.)
// Bounded by a π1-halfspace the range is that halfspace pushed forward by K ---
// an affine pushforward that, unlike bare onto-ness, DOES need
// order-preservation (below).
export template <typename T, auto K, typename L>
  requires dedekind::category::IsAbelianGroup<T, std::plus<T>>
constexpr auto image(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return Ω<T, L>;  // preserve the relation's logic species
}

/** @brief image of a translation graph restricted to a halfspace @c {x⋈P}: the
 *  affine pushforward @c {y⋈P+K}, a halfspace of the same shape.
 *
 *  @details Constrained to ORDER relations (Lt/Le/Gt/Ge): @c dir_of / @c
 * strict_of only model a halfspace bound.  An EQUALITY restriction (@c
 * π1==fix(p)) is a singleton domain, not a halfspace, so it must NOT match here
 * (that would give
 *  @c {y≤p+K} instead of the singleton @c {p+K}); it is left to a separate
 *  singleton path.  Gated on @c IsEntireTranslationCarrier: the pushforward
 * assumes translation PRESERVES the order.  On a wrapping carrier (@c unsigned)
 *  it does not --- the image of @c {x≥5} under @c x+1 wraps @c UINT_MAX to @c
 * 0, which @c {y≥6} would miss --- so the modular groups are declined; the
 *  saturating ℕ (K≥0) and the ordered groups are admitted. */
export template <typename T, auto K, Rel R, auto P, typename L>
  requires((R == Rel::Lt || R == Rel::Le || R == Rel::Gt || R == Rel::Ge) &&
           IsEntireTranslationCarrier<T, K>)
constexpr auto image(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              ProjBound<1, R, P>>>&) {
  return Halfspace<T, P + K, dir_of(R), strict_of(R), L>{};  // keep L
}

/** @brief image of a restricted REFLECTION @c x↦c·x (@c c=±1) on @c {x⋈P}: the
 *  domain halfspace scaled by @c c (pivot @c c·P, sense FLIPPED when @c c<0).
 *
 *  @details These are the branches of the sign-fold epi @c abs = @c (x↦x on
 * x≥0)
 *  @c ⊔ @c (x↦−x on x<0): each is a mono reflection, so its image is a plain
 *  halfspace pushed forward, no search.  (@c |c|>1 would also induce the
 * residue
 *  @c {y≡0 mod c}, a downstream @c :numbers concern; the sign-fold is @c c=±1,
 * so the range stays a bare halfspace here.)  Constrained to ORDER relations
 *  (equality is a singleton, not a halfspace), and the NEGATE branch (@c C=−1)
 *  additionally requires @c IsOrderedAdditiveGroup: a genuine, ORDER-REVERSING
 *  additive inverse.  On a bounded-below non-group carrier (@c Cardinality)
 *  @c x↦−x has no image; on a wrapping group (@c unsigned) modular negation
 * does NOT reverse the order (@c −x of @c {x<5} would admit @c 0 via @c
 * UINT_MAX), so both are declined. */
export template <typename T, auto C, Rel R, auto P, typename L>
  requires((R == Rel::Lt || R == Rel::Le || R == Rel::Gt || R == Rel::Ge) &&
           (C == 1 ||
            (C == -1 && dedekind::algebra::IsOrderedAdditiveGroup<T>)))
constexpr auto image(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjMulConstProj<1, C, Rel::Eq, 2>,
                              ProjBound<1, R, P>>>&) {
  constexpr Direction d = (C < 0) ? flip(dir_of(R)) : dir_of(R);
  return Halfspace<T, C * P, d, strict_of(R), L>{};  // keep L
}

/** @brief @c is_function(R) --- the bracket-free query: @c R is a bona fide
 *  function.  A graph @f$\pi_2 = \pi_1 + K@f$ is single-valued in @f$\pi_2@f$
 *  (functional) and total (entire, a translation is defined everywhere), so it
 *  meets both bounds of Table~3's @f$\pi_A@f$ column.  Gated on @c
 *  IsEntireTranslationCarrier: on @c bool the graph is not entire (@c true+K
 *  leaves the carrier), so the query is withheld there rather than claiming a
 *  spurious total function. */
export template <typename T, auto K, typename L>
  requires IsEntireTranslationCarrier<T, K>
consteval bool is_function(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return true;
}

/** @brief @c is_entire(R): does @c R cover its whole declared domain?  The bare
 *  translation graph is total; ANY restriction --- here a codomain constraint
 *  on @f$\pi_2@f$ --- pulls its domain back to a proper subset, so it drops to
 *  a @b partial function (functional, not entire; Table~3).  Gated on @c
 *  IsEntireTranslationCarrier so the bare graph is certified total only where
 *  @c x+K genuinely stays in the carrier (ℤ for any @c K, ℕ for @c K≥0). */
export template <typename T, auto K, typename L>
  requires IsEntireTranslationCarrier<T, K>
consteval bool is_entire(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return true;
}
// A CODOMAIN constraint on π2 (an upper/lower bound, or its meet with a
// residue) pulls the domain back through the graph.  Gated on @c
// IsOrderedAdditiveGroup<T>: on the unbounded ℤ-like carrier the codomain is
// unbounded both ways, so ANY finite half-bound @c {π2⋈P} provably cuts a
// PROPER sub-domain @c {x⋈P−K} and the graph drops to a partial function.  ℕ =
// @c Cardinality is NOT matched here, precisely because a lower bound there can
// be VACUOUS (e.g. @c π2≥0 on the successor removes nothing): declining to
// match keeps @c is_entire from making a false non-entire claim on ℕ.
export template <typename T, auto K, Rel R, auto P, typename L>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
consteval bool is_entire(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              ProjBound<2, R, P>>>&) {
  return false;
}
export template <typename T, auto K, Rel R, auto P, auto V, auto W, typename L>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
consteval bool is_entire(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              RelAnd<ProjBound<2, R, P>,
                                     ProjModConstBound<2, V, Rel::Eq, W>>>>&) {
  return false;
}

/** @brief @c argmax over a partial function: the translation @c x↦x+K into a
 *  codomain bounded above (@c π2≤P) and restricted to a residue class
 *  (@c π2≡W mod V), read off structurally (a compile-time constrained optimum).
 *
 *  @details Gated on @c IsOrderedAdditiveGroup: the arithmetic assumes a domain
 *  unbounded below, so @c {x≤P−K ∧ x≡r mod V} is always non-empty and @c m is a
 *  valid optimum.  Additive inverses are what make the carrier unbounded below
 *  (ℤ certifies it; ℕ = @c Cardinality is a rig, no negation, bounded below by
 *  0), and the ordered (non-cyclic) requirement excludes the wrapping groups
 *  (@c unsigned), where @c −K would fold modulo capacity.  So this excludes ℕ
 *  --- where a codomain bound @c P<K would pull the feasible domain empty while
 *  the formula still returned a negative singleton --- and @c unsigned alike.
 */
export template <typename T, auto K, auto P, auto V, auto W, typename L>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
constexpr auto argmax(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              RelAnd<ProjBound<2, Rel::Le, P>,
                                     ProjModConstBound<2, V, Rel::Eq, W>>>>&) {
  constexpr auto p = P - K;                      // domain bound {x ≤ P−K}
  constexpr auto r = ((W - K) % V + V) % V;      // residue x ≡ (W−K) mod V
  constexpr auto m = p - ((p - r) % V + V) % V;  // largest x ≤ p with x ≡ r
  return Singleton<m, L>{};
}

}  // namespace dedekind::order

// ── Entireness inference (Table 3): the ALGEBRAIC half of the DSL's relation
// properties.  Functionality (is_right_unique_v) stays structural in @c order;
// entireness (is_left_total_v) lands here because a translation is total
// exactly where the carrier is an ordered additive group.  A LEAF carries the
// property by its carrier; a NODE (a relative product @c >>) inherits it from
// both factors.
namespace dedekind::category {

// LEAF: a translation graph x ↦ x+K is ENTIRE exactly where x+K stays in the
// carrier --- @c IsEntireTranslationCarrier: an ordered additive group (ℤ-like,
// any K) or ℕ = Cardinality with K ≥ 0.  Bare @c IsAbelianGroup was the trap:
// unsigned/bool are (cyclic) groups, but true+K leaves bool and x+K wraps on
// unsigned, so neither is a total translation.
template <typename T, auto K, typename L>
inline constexpr bool is_left_total_v<dedekind::sets::Set<
    std::pair<T, T>, L,
    dedekind::order::ProjAddConstProj<1, K, dedekind::order::Rel::Eq, 2>>> =
    dedekind::order::IsEntireTranslationCarrier<T, K>;

// LEAF: the diagonal π1==π2 (the identity relation) is entire on any carrier --
// a ↦ a is total.
template <typename T, typename L>
inline constexpr bool is_left_total_v<dedekind::sets::Set<
    std::pair<T, T>, L,
    dedekind::order::ProjProj<1, dedekind::order::Rel::Eq, 2>>> = true;

// NODE (the compositional closure): the relative product R>>S is entire iff
// BOTH factors are -- the property propagates through >> (Table 3's
// containments composing).  The retained intermediate B reconstructs the two
// factor relations.  (Functionality's NODE rule is the structural sibling in @c
// order.)
template <typename A, typename C, typename L, typename PR, typename PS,
          typename B>
inline constexpr bool is_left_total_v<dedekind::sets::Set<
    std::pair<A, C>, L, dedekind::order::ComposePred<PR, PS, B>>> =
    is_left_total_v<dedekind::sets::Set<std::pair<A, B>, L, PR>> &&
    is_left_total_v<dedekind::sets::Set<std::pair<B, C>, L, PS>>;

}  // namespace dedekind::category
