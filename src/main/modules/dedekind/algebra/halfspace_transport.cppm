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
#include <stdexcept>    // std::domain_error (a scale other than ±1)
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::pair

export module dedekind.algebra:halfspace_transport;

import dedekind.category; // IsAbelianGroup, is_left_total_v (the trait primary)
import dedekind.sets;     // Set, 𝔸, Singleton, Cardinality, SignedCardinality
import dedekind.order;    // Translation / Scaling, ProductRestrict, rays
import dedekind.relational; // ComposePred (Tarski :dyadic), the >> trait NODE (#792)
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
 * @brief The carriers on which a translation @f$x \mapsto x+k@f$ is a @b total
 *        (entire) function: an @c algebra::IsOrderedAdditiveGroup carrier
 *        (ℤ-like, closed under @c +k for any @c k), or ℕ = @c Cardinality,
 * where a constant cannot be negative (it is refused when it meets ℕ), so the
 *        translation is the total successor-like @c x+k.  Excludes the cyclic
 *        (wrapping) carriers (@c unsigned, @c bool).  @tparam T the carrier.
 */
export template <typename T>
concept IsEntireTranslationCarrier =
    dedekind::algebra::IsOrderedAdditiveGroup<T> ||
    std::same_as<std::remove_cvref_t<T>, dedekind::sets::Cardinality>;

/** @brief A cylinder atom @c π_I @c ⋈ @c c.  @tparam R the comparison.
 *  @tparam I the slot.  @tparam K the constant's type. */
template <Rel R, IsRingIntegral auto I, typename K>
using Cylinder = Cmp<R, Projection<I>, Const<K>>;
/** @brief The residue atom @c π_I @c % @c m @c == @c w. */
template <IsRingIntegral auto I, typename M, typename W>
using Residue =
    Cmp<Rel::Eq, BinOp<std::modulus<>, Projection<I>, Const<M>>, Const<W>>;

/** @brief @c inverse of a translation graph = its CONVERSE, the translation by
 *  the group inverse of the shift, again a graph.  The inverse is the group's
 *  @b unary inverse (@c category::inverse_v), not the carrier's @c operator-:
 *  @c IsGroup guarantees a group inverse, not a negation operator, and on a
 *  characteristic-two field the two differ.  Gated on @c IsGroup under the very
 *  @c std::plus the graph evaluates.  On a wrapping group such as @c unsigned
 *  the inverse folds modulo capacity, the correct converse of a bijection;
 *  so the inverse is taken in the carrier, after the shift is cast into it. */
export template <typename T, typename K, typename L, typename C>
  requires dedekind::category::IsGroup<T, std::plus<T>>
constexpr auto inverse(
    const Comprehension<𝔸<std::pair<T, T>, L, C>, Translation<K>>& g) {
  const T neg = dedekind::category::inverse_v<T, std::plus<T>>(
      carrier_cast<T>(g.predicate.a.b.value));
  return 𝔸<std::pair<T, T>, L>{} | (π1 + neg == π2);
}

// #875: retractability generalizes ℤ → any IsGroup: on the cyclic unsigned the
// inverse of the successor graph is the translation by the modular inverse.
static_assert(
    inverse(𝔸<std::pair<unsigned, unsigned>, Boole>{} | (π1 + 1u == π2))
            .predicate.a.b.value == -1u,
    "inverse over unsigned is the translation by the modular inverse −1.");

/** @brief image of a translation graph = the whole carrier: a translation is a
 *  bijection on any additive group (a cyclic one included), hence onto.  On ℕ
 *  (not a group) @c x+k misses @c {0,…,k−1}, so the gate is the group. */
export template <typename T, typename K, typename L, typename C>
  requires dedekind::category::IsAbelianGroup<T, std::plus<T>>
constexpr auto image(
    const Comprehension<𝔸<std::pair<T, T>, L, C>, Translation<K>>&) {
  return 𝔸<T, L>{};
}

/** @brief image of a translation graph restricted to @c {x⋈p}: the affine
 *  pushforward @c {y⋈p+k}, a halfspace of the same shape.  Order comparisons
 *  only (equality restricts to a point, not a halfspace), and only where the
 *  translation preserves the order (@c IsEntireTranslationCarrier). */
export template <IsTotallyOrdered T, typename K, Rel R, typename V,
                 IsOckhamAlgebra L, IsCardinality C>
  requires(is_order_rel(R) && IsEntireTranslationCarrier<T>)
constexpr auto image(
    const Comprehension<𝔸<std::pair<T, T>, L, C>,
                        ProductRestrict<Translation<K>, Cylinder<R, 1, V>>>&
        s) {
  return make_ray<dir_of(R), strict_of(R), L>(
      carrier_cast<T>(s.predicate.rp.b.value) +
      carrier_cast<T>(s.predicate.product.a.b.value));
}

/** @brief image of a restricted scaling @c x↦c·x, @c c=±1, on @c {x⋈p}: the
 *  ray scaled by @c c, its sense flipped when @c c<0.  The sense depends on
 *  the constant's value, so the result is the value leaf @c SetVal.  @c c=−1
 *  needs a genuine order-reversing additive inverse (@c
 * IsOrderedAdditiveGroup); any other scale has no ray for an image here and is
 * refused. */
export template <IsTotallyOrdered T, typename K, Rel R, typename V,
                 IsOckhamAlgebra L, IsCardinality CU>
  requires(is_order_rel(R))
constexpr SetVal<T, L> image(
    const Comprehension<𝔸<std::pair<T, T>, L, CU>,
                        ProductRestrict<Scaling<K>, Cylinder<R, 1, V>>>& s) {
  const T c = carrier_cast<T>(s.predicate.product.a.b.value);
  const T p = carrier_cast<T>(s.predicate.rp.b.value);
  if (c == carrier_cast<T>(1))
    return SetVal<T, L>::half(p, dir_of(R), strict_of(R));
  if constexpr (dedekind::algebra::IsOrderedAdditiveGroup<T>) {
    if (c == carrier_cast<T>(-1))
      return SetVal<T, L>::half(c * p, flip(dir_of(R)), strict_of(R));
  }
  throw std::domain_error("the image of a ray under x ↦ c·x, c ≠ ±1");
}

/** @brief @c is_function(R): a translation graph is single-valued and, where
 *  @c x+k stays in the carrier, total. */
export template <typename T, typename K, typename L, typename C>
  requires IsEntireTranslationCarrier<T>
consteval bool is_function(
    const Comprehension<𝔸<std::pair<T, T>, L, C>, Translation<K>>&) {
  return true;
}

/** @brief @c is_entire(R): the bare translation graph is total; a codomain
 *  bound (alone or with a residue) on the unbounded ℤ-like carrier pulls the
 *  domain back to a proper subset.  ℕ is not matched for the codomain cases: a
 *  lower bound there can be vacuous, so no false non-entire claim is made. */
export template <typename T, typename K, typename L, typename C>
  requires IsEntireTranslationCarrier<T>
consteval bool is_entire(
    const Comprehension<𝔸<std::pair<T, T>, L, C>, Translation<K>>&) {
  return true;
}
export template <typename T, typename K, Rel R, typename V, typename L,
                 typename C>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
consteval bool is_entire(
    const Comprehension<𝔸<std::pair<T, T>, L, C>,
                        ProductRestrict<Translation<K>, Cylinder<R, 2, V>>>&) {
  return false;
}
export template <typename T, typename K, Rel R, typename V, typename M,
                 typename W, typename L, typename C>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T>
consteval bool is_entire(
    const Comprehension<
        𝔸<std::pair<T, T>, L, C>,
        ProductRestrict<Translation<K>,
                        RelAnd<Cylinder<R, 2, V>, Residue<2, M, W>>>>&) {
  return false;
}

/** @brief @c argmax over a partial function: the translation @c x↦x+k into a
 *  codomain bounded above (@c π2≤P) and restricted to a residue class
 *  (@c π2≡W mod M), read off structurally.  Gated on an ordered additive group
 *  of integers: the domain is unbounded below, so the feasible set is never
 *  empty and the read-off is the optimum. */
export template <typename T, typename K, typename V, typename M, typename W,
                 IsOckhamAlgebra L, IsCardinality C>
  requires(dedekind::algebra::IsOrderedAdditiveGroup<T> &&
           dedekind::sets::IsRingIntegral<T>)
constexpr auto
argmax(const Comprehension<
       𝔸<std::pair<T, T>, L, C>,
       ProductRestrict<Translation<K>,
                       RelAnd<Cylinder<Rel::Le, 2, V>, Residue<2, M, W>>>>& s) {
  using W_ = long long;  // wider than the pivot: no overflow in P−k
  const W_ k = W_(s.predicate.product.a.b.value);
  const W_ m_ = W_(s.predicate.rp.b.a.b.value);
  const W_ w = W_(s.predicate.rp.b.b.value);
  const W_ p = W_(s.predicate.rp.a.b.value) - k;  // domain bound {x ≤ P−k}
  const W_ r0 = (w - k) % m_;
  const W_ r = r0 < 0 ? r0 + m_ : r0;  // residue x ≡ (W−k) mod M
  const W_ d0 = (p - r) % m_;
  return dedekind::sets::singleton<L>(p - (d0 < 0 ? d0 + m_ : d0));
}

// ── preimage: the contravariant inverse of image ───────────────────────────
// image pushes a domain halfspace forward along an affine map; preimage pulls a
// codomain halfspace back, in closed form.  preimage(f, P)(a) == P(f(a)).

/** @brief preimage of @c {y⋈P} under @f$x\mapsto x+k@f$: @c {x⋈P−k}, same
 *  direction and strictness.  Gated on a translation-invariant order. */
export template <typename T, typename K, IsOckhamAlgebra LG, IsCardinality C,
                 IsOckhamAlgebra LH, IsCardinality CH, IsSide Lo, IsSide Hi>
  requires dedekind::algebra::IsOrderedAdditiveGroup<T> &&
           (is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr auto preimage(
    const Comprehension<𝔸<std::pair<T, T>, LG, C>, Translation<K>>& g,
    const Comprehension<𝔸<T, LH, CH>, Bounds<Lo, Hi, T>>& h) {
  return 𝔸<T, LH>{} |
         Bounds<Lo, Hi, T>{pivot(h) - carrier_cast<T>(g.predicate.a.b.value)};
}

/** @brief preimage of @c {y⋈P} under @f$x\mapsto c\cdot x@f$, @c c=±1:
 *  @c {x⋈'c·P}, the sense flipped when @c c<0, so the value leaf.  @c c=−1
 *  needs an order-reversing additive inverse. */
export template <typename T, typename K, IsOckhamAlgebra LG, IsCardinality CU,
                 IsOckhamAlgebra LH, IsCardinality CH, IsSide Lo, IsSide Hi>
  requires(is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr SetVal<T, LH> preimage(
    const Comprehension<𝔸<std::pair<T, T>, LG, CU>, Scaling<K>>& g,
    const Comprehension<𝔸<T, LH, CH>, Bounds<Lo, Hi, T>>& h) {
  const T c = carrier_cast<T>(g.predicate.a.b.value);
  constexpr Direction d = direction_of<Lo, Hi>();
  constexpr Strictness s = ray_strictness<Lo, Hi>();
  if (c == carrier_cast<T>(1)) return SetVal<T, LH>::half(pivot(h), d, s);
  if constexpr (dedekind::algebra::IsOrderedAdditiveGroup<T>) {
    if (c == carrier_cast<T>(-1))
      return SetVal<T, LH>::half(c * pivot(h), flip(d), s);
  }
  throw std::domain_error("the preimage of a ray under x ↦ c·x, c ≠ ±1");
}

}  // namespace dedekind::order

// ── Entireness inference (Table 3), the algebraic half: a translation is total
// exactly where the carrier keeps x+k in it; the diagonal is total anywhere.
namespace dedekind::category {

template <typename T, typename K, typename L, typename C>
inline constexpr bool is_left_total_v<
    dedekind::sets::Comprehension<dedekind::sets::𝔸<std::pair<T, T>, L, C>,
                                  dedekind::order::Translation<K>>> =
    dedekind::order::IsEntireTranslationCarrier<T>;

template <typename T, typename L, typename C>
inline constexpr bool is_left_total_v<dedekind::sets::Comprehension<
    dedekind::sets::𝔸<std::pair<T, T>, L, C>,
    dedekind::sets::Cmp<dedekind::sets::Rel::Eq, dedekind::sets::Projection<1>,
                        dedekind::sets::Projection<2>>>> = true;

}  // namespace dedekind::category
