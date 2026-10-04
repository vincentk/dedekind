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
 * @build_order after :ordered_algebra
 * @dependency dedekind.order
 */
module;

#include <concepts>     // std::same_as
#include <functional>   // std::plus
#include <limits>       // std::numeric_limits (overflow-safe pivot guards)
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::pair

export module dedekind.algebra:halfspace_transport;

import dedekind.category; // IsAbelianGroup, is_left_total_v (the trait primary)
import dedekind.sets;     // Set, 𝔸, Singleton, Cardinality, SignedCardinality
import dedekind.order; // Halfspace, ProjAddConstProj, Rel, dir_of/strict_of/flip
import dedekind.relational; // ComposePred (Tarski :dyadic) — the >> trait NODE (#792)
import :ordered_algebra;  // IsOrderedAdditiveGroup (the canonical gate)

// Mirror order/halfspace.cppm's directives so the DSL names (Set, 𝔸, Singleton
// from sets; the trait primaries from category) resolve unqualified inside the
// re-opened namespaces.  using-directives are TU-local (never exported).
using namespace dedekind::sets;
using namespace dedekind::relational;  // RelAnd (moved from :sets, #792)
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
 *  @c B*A|P⁻¹: the same graph read backwards, @c x↦x+(−K), again a GRAPH (a
 *  relation, not an arrow), so it stays on the surface and composes.
 *
 *  @details Functions ARE graphs here, so @c inverse is the relational converse
 *  with the shift replaced by its GROUP inverse.  A right translation
 *  @f$x\mapsto x+K@f$ is a bijection on @b any group, and a bijection's
 * converse is its inverse, so the converse is @f$x\mapsto x+(-K)@f$ where
 * @f$-K@f$ is the group's @b unary inverse of the shift.  In a group @c (G,+)
 * the primitive is that unary inverse and the binary @c y−K unfolds as @c
 * y+(−K); this overload builds the @c +(−K) converse from the same @c std::plus
 * the forward graph applies.
 *
 *  We take the unary inverse from the group-inverse registry,
 *  @c category::inverse_v, @b not the carrier's @c operator- on the NTTP.
 *  @c IsGroup guarantees a group inverse @b exists; it does @b not guarantee a
 *  carrier @c operator-, and the two disagree in general: on a
 *  characteristic-two field such as @c 𝔽64 the additive inverse is @c K itself
 *  (@f$-x=x@f$), where a plain negation would demand an @c operator- the group
 *  axioms never promised.  So the gate pairs the structural claim
 *  @c IsGroup<T,Op> with the operational one that @c inverse_v is @b
 * computable: the @c is_invertible_v @b marker can be a bare opt-in (as it is
 * for @c 𝔽64 via its @c GaloisFieldRegistration atlas) without a matching
 * computable
 *  @c inverse free function, and forming the converse needs the value, not just
 *  the claim.
 *
 *  The inverse is computed at the shift's own type @c decltype(K), not at the
 *  carrier @c T, so the graph's NTTP type-identity is preserved (the DSL spells
 *  a shift over @c ℤ = @c 𝔸<SignedCardinality> with an @c int NTTP, @c
 * fix(3_c); promoting it to @c T would change the graph type and break converse
 *  equality).  The graph's @c std::plus<T> then interprets the constant in the
 *  carrier.  On a wrapping group such as @c unsigned (@f$\mathbb{Z}/2^w@f$) the
 *  registry inverse folds modulo capacity, the correct converse even though it
 *  is @b not the order-predecessor the old @c IsOrderedAdditiveGroup gate
 *  conflated it with.  The gate is @c IsGroup<T, std::plus<T>>: @c std::plus<T>
 *  is @b the operation @c ProjAddConstProj actually evaluates, so the group
 *  structure is asserted for @b that operation, not a free @c Op parameter.  A
 *  free @c Op would be unsound here (it is not deducible from the argument, so
 *  a caller could certify an unrelated group op, e.g.\ @c bool under
 *  @c std::bit_xor, and expose an @c inverse for a non-bijective
 *  @c std::plus<bool> graph); the operator-generic form waits until the graph
 *  carries its operation in its type (#882).  This is the #875 generalization
 *  from @f$\mathbb{Z}@f$ to an arbitrary @c IsGroup under @c +.  (The
 *  ORDER-preserving affine pushforward @c image(Halfspace, +K) below genuinely
 *  needs the order and stays gated on @c IsOrderedAdditiveGroup; the two facts
 *  are gated independently.  ℕ = @c Cardinality is not a group, so it is not
 *  matched here either way.) */
export template <typename T, auto K, typename L, typename C>
  requires dedekind::category::IsGroup<T, std::plus<T>> && requires(
                                                               decltype(K) k) {
    {
      dedekind::category::inverse_v<decltype(K), std::plus<decltype(K)>>(k)
    } -> std::same_as<decltype(K)>;
  }
constexpr auto inverse(
    const Comprehension<𝔸<std::pair<T, T>, L, C>,
                        ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  // −K is the group's UNARY inverse of the shift (category::inverse_v), NOT
  // carrier operator- on the NTTP; the converse graph is x ↦ x + (−K).  It is
  // computed at decltype(K) to preserve the graph's NTTP type-identity.
  constexpr decltype(K) neg_K =
      dedekind::category::inverse_v<decltype(K), std::plus<decltype(K)>>(K);
  return Comprehension<𝔸<std::pair<T, T>, L>,
                       ProjAddConstProj<1, neg_K, Rel::Eq, 2>>{
      ProjAddConstProj<1, neg_K, Rel::Eq, 2>{}};
}

// ── #875 witness: retractability generalizes ℤ → arbitrary IsGroup ───────────
// The OLD gate (@c IsOrderedAdditiveGroup) withheld @c inverse on the cyclic
// group @c unsigned (@f$\mathbb{Z}/2^w@f$), conflating the group inverse with
// the order-predecessor.  With the gate relaxed to @c IsGroup the
// converse-with-negated-shift now resolves there too, and it IS the modular
// group inverse: @c inverse of the successor graph @c π1+1==π2 over @c unsigned
// equals the converse graph @c π1+(−1)==π2 (i.e. @c x↦x+UINT_MAX, the modular
// predecessor).  A bijection's converse is its inverse, so this is correct.
static_assert(
    std::same_as<
        decltype(inverse(Comprehension<𝔸<std::pair<unsigned, unsigned>, Boole>,
                                       ProjAddConstProj<1, 1u, Rel::Eq, 2>>{
            ProjAddConstProj<1, 1u, Rel::Eq, 2>{}})),
        Comprehension<𝔸<std::pair<unsigned, unsigned>, Boole>,
                      ProjAddConstProj<1, -1u, Rel::Eq, 2>>>,
    "inverse over unsigned (a cyclic group the old IsOrderedAdditiveGroup gate "
    "withheld) is now the converse graph with the negated (modular) shift: the "
    "group inverse, #875 (retractability generalizes ℤ → arbitrary IsGroup).");

// image = the RANGE (π_B projection) of a functional graph, read structurally.
// A translation is surjective on ANY additive group (@c IsAbelianGroup under
// +): on ℤ the range of the unbounded graph is the whole line; on a cyclic
// group
// (@c unsigned) the modular translation is still a bijection, hence onto, so 𝔸
// is the correct range regardless of wrap.  (On ℕ = @c Cardinality, NOT a
// group, x↦x+K misses {0,…,K−1}, so this overload is gated to the group case.)
// Bounded by a π1-halfspace the range is that halfspace pushed forward by K ---
// an affine pushforward that, unlike bare onto-ness, DOES need
// order-preservation (below).
export template <typename T, auto K, typename L, typename C>
  requires dedekind::category::IsAbelianGroup<T, std::plus<T>>
constexpr auto image(const Comprehension<𝔸<std::pair<T, T>, L, C>,
                                         ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return 𝔸<T, L>{};  // preserve the relation's logic species
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
export template <typename T, auto K, Rel R, typename VT, typename L, typename C>
  requires((R == Rel::Lt || R == Rel::Le || R == Rel::Gt || R == Rel::Ge) &&
           IsEntireTranslationCarrier<T, K>)
constexpr auto image(
    const Comprehension<𝔸<std::pair<T, T>, L, C>,
                        ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                                        ProjBound<1, R, VT>>>& s) {
  // The pivot rides in the ProjBound VALUE now, so the shifted pivot P+K
  // is computed at constexpr (folds when the argument is constexpr) rather than
  // in the NTTPs.  Direction / strictness stay type-level (R is NTTP), so the
  // meet's complement-pair collapse remains type-sensitive.
  return Halfspace<T, dir_of(R), strict_of(R), L>{
      static_cast<T>(s.predicate.rp.value + K)};  // keep L
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
export template <typename T, auto C, Rel R, typename VT, typename L,
                 typename CU>
  requires((R == Rel::Lt || R == Rel::Le || R == Rel::Gt || R == Rel::Ge) &&
           (C == 1 ||
            (C == -1 && dedekind::algebra::IsOrderedAdditiveGroup<T>)))
constexpr auto image(
    const Comprehension<𝔸<std::pair<T, T>, L, CU>,
                        ProductRestrict<ProjMulConstProj<1, C, Rel::Eq, 2>,
                                        ProjBound<1, R, VT>>>& s) {
  constexpr Direction d = (C < 0) ? flip(dir_of(R)) : dir_of(R);
  return Halfspace<T, d, strict_of(R), L>{
      static_cast<T>(C * s.predicate.rp.value)};  // keep L
}

/** @brief @c is_function(R) --- the bracket-free query: @c R is a bona fide
 *  function.  A graph @f$\pi_2 = \pi_1 + K@f$ is single-valued in @f$\pi_2@f$
 *  (functional) and total (entire, a translation is defined everywhere), so it
 *  meets both bounds of Table~3's @f$\pi_A@f$ column.  Gated on @c
 *  IsEntireTranslationCarrier: on @c bool the graph is not entire (@c true+K
 *  leaves the carrier), so the query is withheld there rather than claiming a
 *  spurious total function. */
export template <typename T, auto K, typename L, typename C>
  requires IsEntireTranslationCarrier<T, K>
consteval bool is_function(
    const Comprehension<𝔸<std::pair<T, T>, L, C>,
                        ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return true;
}

/** @brief @c is_entire(R): does @c R cover its whole declared domain?  The bare
 *  translation graph is total; ANY restriction --- here a codomain constraint
 *  on @f$\pi_2@f$ --- pulls its domain back to a proper subset, so it drops to
 *  a @b partial function (functional, not entire; Table~3).  Gated on @c
 *  IsEntireTranslationCarrier so the bare graph is certified total only where
 *  @c x+K genuinely stays in the carrier (ℤ for any @c K, ℕ for @c K≥0). */
export template <typename T, auto K, typename L, typename C>
  requires IsEntireTranslationCarrier<T, K>
consteval bool is_entire(
    const Comprehension<𝔸<std::pair<T, T>, L, C>,
                        ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
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
export template <typename T, auto K, Rel R, typename VT, typename L, typename C>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
consteval bool is_entire(
    const Comprehension<𝔸<std::pair<T, T>, L, C>,
                        ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                                        ProjBound<2, R, VT>>>&) {
  return false;
}
export template <typename T, auto K, Rel R, typename VT, auto V, auto W,
                 typename L, typename C>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
consteval bool is_entire(
    const Comprehension<
        𝔸<std::pair<T, T>, L, C>,
        ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                        RelAnd<ProjBound<2, R, VT>,
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
export template <typename T, auto K, typename VT, auto V, auto W, typename L,
                 typename C>
  requires(dedekind::algebra::IsOrderedAdditiveGroup<T> &&
           dedekind::sets::IsRingIntegral<T>)
constexpr auto argmax(
    const Comprehension<
        𝔸<std::pair<T, T>, L, C>,
        ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                        RelAnd<ProjBound<2, Rel::Le, VT>,
                               ProjModConstBound<2, V, Rel::Eq, W>>>>& s) {
  // The codomain bound P rides in the ProjBound VALUE now; the residue
  // modulus/shift @c K/@c W/@c V stay compile-time NTTPs.  The optimum is read
  // off with wider-type headroom (a wider signed type, not the pivot's own),
  // so @c P−K and the residue folds cannot overflow.  Residue normalisation
  // adds @c V only when the remainder is negative (cf. the ℤ/N materialisation
  // in :numbers).  Folds at compile time when the argument is constexpr.
  using W_ = long long;  // wider bound: no pivot overflow
  const W_ p = W_(s.predicate.rp.a.value) - W_(K);  // domain bound {x ≤ P−K}
  constexpr W_ r0 = (W_(W) - W_(K)) % W_(V);
  constexpr W_ r = r0 < 0 ? r0 + W_(V) : r0;  // residue x ≡ (W−K) mod V
  const W_ d0 = (p - r) % W_(V);
  const W_ m = p - (d0 < 0 ? d0 + W_(V) : d0);  // largest x ≤ p with x ≡ r
  return Singleton<W_, L>{m};
}

// ── preimage: the CONTRAVARIANT inverse of @c image ────────────────────────
// Where @c image pushes a domain halfspace FORWARD along an affine map, @c
// preimage pulls a CODOMAIN halfspace BACK to the domain, in closed form and
// the same shape.  It is the missing inverse leg of the transport surface: the
// point-free realisation of the by-hand "compose Φ, certify the reduced native
// predicate" pattern (numbers/strength_reduction_test).  @c preimage is
// contravariant --- @f$(f;g)^{*} = g^{*}\circ f^{*}@f$ --- so pulling a bound
// back through a composite is the fold @c preimage(f, preimage(g, ...)); each
// leg is a closed form, so the composite is too.  The defining property
// @c preimage(f,P)(a) == P(f(a)) is witnessed per map in the exhibit.

/** @brief preimage of a codomain halfspace @c {y⋈P} under the translation
 *  @f$x\mapsto x+K@f$: the domain halfspace @c {x⋈(P−K)}, same direction and
 *  strictness.  Exact inverse of the forward @c image (which sends @c {x⋈P} to
 *  @c {y⋈P+K}).
 *
 *  @details Gated on @c IsOrderedAdditiveGroup: the equivalence
 *  @f$(x+K)\bowtie P \iff x \bowtie (P-K)@f$ needs a translation-invariant
 * order. On a wrapping @c unsigned the shifted bound would admit wrapped
 * values, so the modular groups are declined (the same gate the forward
 * pushforward carries). */
export template <typename T, auto K, typename LG, typename C, typename LH,
                 typename CH, IsSide Lo, IsSide Hi>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T> &&
           (is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr auto preimage(
    const Comprehension<𝔸<std::pair<T, T>, LG, C>,
                        ProjAddConstProj<1, K, Rel::Eq, 2>>&,
    const Comprehension<𝔸<T, LH, CH>, Bounds<Lo, Hi, T>>& h) {
  // The pivot P rides in the Halfspace VALUE, so the pulled-back bound P−K is
  // computed at constexpr in the carrier's own arithmetic (folds when the
  // argument is constexpr); K is lifted into the carrier first, since a
  // variant carrier (ℤ = SignedCardinality) has no mixed int arithmetic.  The
  // graph's logic (LG) and the target's logic (LH) are deduced SEPARATELY: the
  // pullback inherits the target set's logic, so a Classical translation graph
  // can pull back a Ternary halfspace (mirrors the general :graph preimage).
  // Direction / strictness stay type-level (load-bearing for structured_and's
  // complement detection).
  using B = Bounds<Lo, Hi, T>;
  return Comprehension<𝔸<T, LH>, B>{
      B{pivot(h) - static_cast<T>(K)}};  // target logic LH
}

/** @brief preimage of a codomain halfspace @c {y⋈P} under the reflection/scale
 *  @f$x\mapsto C\cdot x@f$ (@c C=±1): @c {x⋈'(C·P)}, sense FLIPPED when @c C<0.
 *  Self-inverse for @c C=±1, so it agrees with the forward @c image reflection.
 *
 *  @details @c C=1 (the identity) needs no order structure; @c C=−1 requires
 *  @c IsOrderedAdditiveGroup --- a genuine order-REVERSING additive inverse (on
 *  a bounded-below rig or a wrapping group @f$x\mapsto -x@f$ does not reverse
 *  the order). */
export template <typename T, auto C, typename LG, typename CU, typename LH,
                 typename CH, IsSide Lo, IsSide Hi>
  requires((C == 1 ||
            (C == -1 && dedekind::algebra::IsOrderedAdditiveGroup<T>)) &&
           (is_bounded_side_v<Lo> != is_bounded_side_v<Hi>))
constexpr auto preimage(
    const Comprehension<𝔸<std::pair<T, T>, LG, CU>,
                        ProjMulConstProj<1, C, Rel::Eq, 2>>&,
    const Comprehension<𝔸<T, LH, CH>, Bounds<Lo, Hi, T>>& h) {
  // A reflection (C < 0) swaps the sides: the bound keeps its strictness and
  // changes direction.
  using B = std::conditional_t<(C < 0), Bounds<Hi, Lo, T>, Bounds<Lo, Hi, T>>;
  // The pivot P rides in the Halfspace VALUE; C·P (C=±1) is computed at
  // constexpr in the carrier's arithmetic, C lifted into the carrier first.
  // Graph logic (LG) and target logic (LH) deduced separately; the pullback
  // inherits the target's LH.
  return Comprehension<𝔸<T, LH>, B>{
      B{static_cast<T>(C) * pivot(h)}};  // target logic LH
}

}  // namespace dedekind::order

// ── #875: the group translation's retractability lives in the graph `inverse`
// overload above (no standalone arrow wrapper).  A group translation @c x↦x+K
// is a bijection whose inverse is the negated-shift converse graph, and that IS
// its retract; the generalization from @f$\mathbb{Z}@f$ to an arbitrary @c
// IsGroup is done by relaxing that overload's gate from @c
// IsOrderedAdditiveGroup to @c IsAbelianGroup (superseded standalone-arrow
// reading, cf. morphism.cppm: functions are graphs).

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
template <typename T, auto K, typename L, typename C>
inline constexpr bool is_left_total_v<dedekind::sets::Comprehension<
    dedekind::sets::𝔸<std::pair<T, T>, L, C>,
    dedekind::order::ProjAddConstProj<1, K, dedekind::order::Rel::Eq, 2>>> =
    dedekind::order::IsEntireTranslationCarrier<T, K>;

// LEAF: the diagonal π1==π2 (the identity relation) is entire on any carrier --
// a ↦ a is total.
template <typename T, typename L, typename C>
inline constexpr bool is_left_total_v<dedekind::sets::Comprehension<
    dedekind::sets::𝔸<std::pair<T, T>, L, C>,
    dedekind::order::ProjProj<1, dedekind::order::Rel::Eq, 2>>> = true;

// NODE (the compositional closure) for ComposePred's ENTIRENESS moved to
// dedekind.relational:dyadic (PR #797, Copilot review), alongside its
// functionality sibling --- both are structure-independent relation-algebra
// (R>>S entire iff both factors are), so they live with ComposePred and are
// reachable from :relational alone, not only via @c order / @c algebra.

}  // namespace dedekind::category
