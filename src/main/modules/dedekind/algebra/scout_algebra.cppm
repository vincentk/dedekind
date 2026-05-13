/**
 * @file dedekind/algebra/scout_algebra.cppm
 * @partition :scout_algebra
 * @brief Symbolic scout type for the in-line comprehension surface,
 *        gated on algebraic structure of the carrier (#664).
 *
 * @section scout_algebra__Motivation
 *
 * The algebraic constraints give rise to the symbolic scout type.  A
 * @c GroupScout<T,Op,Element,Inner> exists exactly when @c (T, @c Op)
 * is a group --- the gate is purely algebraic, in textbook vocabulary,
 * not in implementation-specific concepts.  This partition is the
 * algebraic foundation for #664's in-line scout-algebra surface
 * @c Set{f(x) @c | @c P(x)}: once a scout is a valid algebraic
 * expression, the comprehension pipe transports the source predicate
 * structurally and yields a typed @c Set with the @b shifted predicate.
 *
 * @section scout_algebra__Design
 *
 * One basic type, gated on @c IsGroup<T,Op>.  Two named specialisations
 * via operator overloads --- not template specialisations of the type
 * itself, but factory operator overloads that produce specific group
 * instances:
 *
 *  - @c operator+(BoundScout<T>, @c Bound<k>) for
 *    @c IsAdditiveGroup<T> --- produces a
 *    @c GroupScout<T, @c std::plus<T>, @c k, @c BoundScout<T>>.
 *  - @c operator-(BoundScout<T>, @c Bound<k>) for
 *    @c IsAdditiveGroup<T> --- produces the group-inverse shift:
 *    @c GroupScout<T, @c std::plus<T>, @c -k, @c BoundScout<T>>
 *    (subtraction encoded via the additive inverse).
 *  - @c operator*(BoundScout<T>, @c Bound<k>) for
 *    @c IsAbelianGroup<T, @c std::multiplies<T>> (i.e.\ a field
 *    carrier, gated on PR #674's @c IsField cert) --- produces
 *    @c GroupScout<T, @c std::multiplies<T>, @c k, @c BoundScout<T>>.
 *
 * The comprehension pipe @c GroupScout::operator|(Halfspace) performs
 * @b halfspace-pivot @b transport.  The additive pipe shifts the
 * source pivot by the scout's @c Element along the additive group
 * action.  The multiplicative pipe (Slice 3) scales the source pivot,
 * preserves direction when @c k > 0, flips direction when @c k < 0,
 * and Honest-Rejects @c k = 0 (collapsing-halfspace case).  Both
 * yield a @c Comprehension that Set CTAD picks up and resolves to a
 * @c Set with the transported halfspace as predicate.  No Set CTAD
 * changes needed; the existing @c Comprehension path in
 * @c :sets:expressions is the consumer.
 *
 * @section scout_algebra__Honest_Rejection
 *
 * Carriers that do @b not satisfy @c IsAdditiveGroup --- e.g.\
 * @c sets::Cardinality (ℕ has no additive inverses), signed @c int
 * (overflow is UB so the group axioms fail) --- cause the operator
 * overload to be removed from the candidate set.  The compile error
 * names the missing textbook axiom, not a library-specific failure.
 *
 * @section scout_algebra__Out_Of_Scope_For_This_Partition_Today
 *
 *  - Retract-tier scaling on rings (where the carrier is a ring but
 *    not a multiplicative group, e.g.\ @c bound<2> @c * @c in<ℤ>):
 *    Honest-Rejected today by @c IsAbelianGroup<T, std::multiplies<T>>
 *    on @c SignedCardinality being @c false (ℤ is not a field).
 *    Lifting this to a ring-shaped scaling tier is a follow-up.
 *  - Division @c operator/(BoundScout<T>, @c Bound<k>) for fields:
 *    semantically @c x @c / @c k @c = @c x @c * @c k^{-1}.  Computing
 *    @c k^{-1} at the factory site embeds a non-trivial @c T value
 *    (e.g.\ @c Rational{1,k}) into the resulting scout's @c Element,
 *    which forces @c T to be a structural NTTP carrier ---
 *    incompatible with the variant-carrier ℚ today.  Follow-up.
 *  - Composition of scouts beyond a single @c BoundScout inner
 *    (e.g.\ @c bound<2> @c * @c in<ℤ> @c + @c bound<1> --- where the
 *    inner of the outer @c + is itself a @c GroupScout) (follow-up).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>

export module dedekind.algebra:scout_algebra;

import dedekind.category; // IsGroup<T, Op>
import dedekind.sets;     // BoundScout<auto>, SignedExtensionalCardinal
import dedekind.order;    // Bound<auto>
import :group;            // IsAdditiveGroup<T, Add>

namespace dedekind::algebra {

/**
 * @brief Marker trait: @c T is a non-cyclic ordered additive group
 *        whose order is translation-invariant, with the saturating
 *        escape contract for values that would exceed representable
 *        capacity.
 *
 * @details
 * Defaults to @c false.  Carriers opt in via specialisation when their
 * arithmetic is @b not @b cyclic --- i.e.\ they @b saturate or
 * @b escalate (e.g.\ to @f$\pm \aleph_0@f$ sentinels) rather than
 * wrapping at the representable bound.  Saturation preserves the
 * partial order @f$\le@f$ under translation (if @c a @c <= @c b then
 * @c a+c @c <= @c b+c, including the case where both saturate to the
 * same sentinel); wrapping does @b not (the modular wrap reverses the
 * order at the boundary).
 *
 * @note The structural fact this marker discriminates is @b not
 *       boundedness (every C++ carrier is bounded), but @b cyclicity.
 *       A saturating carrier is bounded but non-cyclic: arithmetic
 *       past the bound escalates to a distinguishable sentinel
 *       (@f$\pm \aleph_0@f$ or @c NaZ for indeterminate forms in the
 *       project's @c SignedCardinality variant), keeping
 *       translation-invariance honest @b including at the boundary.
 *       Modular carriers (@c unsigned @c int, the finite
 *       @c sets::SignedExtensionalCardinal<N>) wrap, reversing order;
 *       they fail the marker by default.
 *
 * @note The project put deliberate effort into the saturating
 *       variants (@c sets::SignedCardinality, @c sets::Cardinality)
 *       precisely so they would be bona-fide proxies for
 *       @f$\mathbb{Z}@f$ / @f$\mathbb{N}@f$ rather than cyclic
 *       (cf.\ @c cardinality.cppm:878-967 --- "@c SignedCardinality
 *       is the signed counterpart of @c Cardinality (the ℕ ∪ ℵ_0
 *       variant): ... saturating ... not periodic ... the library's
 *       bona-fide proxy for ℤ modulo physical limits").  The marker
 *       opt-in below honours that effort: it is specialised for
 *       @c SignedCardinality, @b not for the cyclic finite-fragment
 *       @c SignedExtensionalCardinal<N>.
 */
template <typename T>
struct is_translation_invariant_ordered : std::false_type {};

/** @brief @c sets::SignedCardinality is the project's bona-fide
 *  saturating proxy for @f$\mathbb{Z}@f$: arithmetic escalates to
 *  @f$\pm \aleph_0@f$ on overflow and propagates @c NaZ on
 *  indeterminate forms, rather than wrapping modulo capacity
 *  (cf.\ @c cardinality.cppm:923-967).  This is the structural
 *  guarantee the marker certifies: translation preserves the partial
 *  order @f$\le@f$ on the finite fragment; saturation collapses
 *  ordering past the boundary into the @f$\pm \aleph_0@f$ sentinel
 *  (the meet still holds: both sides land at the same sentinel).  The
 *  finite-fragment carrier @c SignedExtensionalCardinal<N> is
 *  intentionally @b not opted in: it is cyclic
 *  (mod @f$2^{N \cdot 64}@f$), so its addition would reverse the
 *  order at the wrap boundary.  The variant @c SignedCardinality is
 *  what callers should use whenever they want translation-invariance
 *  at the type level (i.e.\ the halfspace pipe). */
template <>
struct is_translation_invariant_ordered<dedekind::sets::SignedCardinality>
    : std::true_type {};

template <typename T>
inline constexpr bool is_translation_invariant_ordered_v =
    is_translation_invariant_ordered<T>::value;

/** @brief Carrier-promise marker: @c T is an @b ordered multiplicative
 *  group on its non-zero cone.  Mirrors @c is_translation_invariant_ordered
 *  for the additive case: the marker is the axiom-level promise that
 *  scaling a halfspace pivot by @c k preserves the order @b when
 *  @c k > 0 and reverses it @b when @c k < 0 (with @c k = 0 collapsing
 *  the halfspace --- handled by the pipe's @c Element != zero gate).
 *
 *  Default is @c false_type --- carriers must positively register the
 *  ordered-field structure.  Specialised in @c numbers:rational for
 *  @c Rational<I> on any @c IsInteger I: ℚ is an ordered field (Lang,
 *  @em Algebra §III.1 + the order-compatibility axioms).  Carriers that
 *  satisfy @c algebra::IsField but @b not the order axiom (e.g.\
 *  finite fields like @c bool / 𝔽₂; complex numbers @c Complex<R>
 *  where there is no order compatible with @c *) fail this marker by
 *  default.
 *
 *  The marker pairs with @c IsAbelianGroup<T, std::multiplies<T>> in
 *  the @c IsOrderedMultiplicativeGroup concept below, exactly as the
 *  additive marker pairs with @c IsAbelianGroup<T, std::plus<T>> in
 *  @c IsOrderedAdditiveGroup.
 *
 *  Exported (unlike the additive sibling) so downstream carrier-side
 *  partitions (@c numbers:rational) can specialise without a
 *  scout-algebra-imports-numbers reverse dependency.  The additive
 *  marker's specialisation lives in this same file because
 *  @c sets::SignedCardinality is upstream of @c :algebra; the
 *  multiplicative marker's specialisation lives in @c numbers:rational
 *  because @c Rational<I> is downstream.
 */
export template <typename T>
struct is_ordered_multiplicative_group : std::false_type {};

export template <typename T>
inline constexpr bool is_ordered_multiplicative_group_v =
    is_ordered_multiplicative_group<T>::value;

/**
 * @concept IsOrderedAdditiveGroup
 * @brief An additive group whose order is translation-invariant.
 *
 * @details
 * Combines the algebraic gate (@c IsAdditiveGroup) with the @b axiom
 * marker (@c is_translation_invariant_ordered_v).  This is the right
 * precondition for halfspace-pivot transport: shifting
 * @f$\{x \mid x > k\}@f$ by @c +c yields @f$\{y \mid y > k+c\}@f$
 * exactly when @c c-translation preserves the order, which is what
 * the marker certifies.
 *
 * @note Earlier drafts also required @c std::totally_ordered<T>.
 *       Dropped because the project's saturating ℤ proxy
 *       (@c sets::SignedCardinality, a @c std::variant) uses
 *       custom comparison operators (specialised on the saturating
 *       semantics and on @c NaZ propagation), which do not satisfy
 *       the @c std::totally_ordered structural concept --- yet the
 *       carrier @b is the right one for translation-invariant
 *       ordered-group semantics.  The marker carries the order claim;
 *       a separate @c std::totally_ordered structural check would
 *       Honest-Reject the very carriers the marker is supposed to
 *       accept.  The structural order check belongs at the marker
 *       opt-in site, not at the concept.
 *
 * Modular carriers (@c unsigned @c int as @f$\mathbb{Z}/2^N\mathbb{Z}@f$)
 * satisfy @c IsAdditiveGroup but NOT this concept --- they fail the
 * marker by default.  Honest Rejection on halfspace-pipe attempts with
 * modular carriers.
 */
export template <typename T>
concept IsOrderedAdditiveGroup =
    dedekind::category::IsAbelianGroup<T, std::plus<T>> &&
    is_translation_invariant_ordered_v<T>;

/**
 * @concept IsOrderedMultiplicativeGroup
 * @brief A multiplicative group whose order is compatible with @c *.
 *
 * @details
 * Multiplicative twin of @c IsOrderedAdditiveGroup: combines the
 * algebraic gate @c IsAbelianGroup<T, std::multiplies<T>> with the
 * @b axiom marker @c is_ordered_multiplicative_group_v<T>.  This is
 * the precondition for halfspace-pivot transport under scaling:
 * scaling @f$\{x \mid x > k\}@f$ by @c k_E yields
 * @f$\{y \mid y > k \cdot k_E\}@f$ when @c k_E > 0 and the
 * direction-flipped @f$\{y \mid y < k \cdot k_E\}@f$ when @c k_E < 0,
 * exactly when @c k_E-scaling preserves / reverses the order under
 * the multiplicative-group structure.  The marker certifies the
 * order-compatibility axiom.
 *
 * @note The @c k_E = 0 case is degenerate (the halfspace collapses);
 *       handled at the pipe's @c Element != zero gate, not here.
 *
 * Per PR #674: @c IsField<Rational<default_integer>> fires structurally,
 * so the algebraic side closes on ℚ.  The marker side is registered in
 * @c numbers:rational.
 */
export template <typename T>
concept IsOrderedMultiplicativeGroup =
    dedekind::category::IsAbelianGroup<T, std::multiplies<T>> &&
    is_ordered_multiplicative_group_v<T>;

/**
 * @brief Symbolic scout parameterised by carrier @c T, group operation
 *        @c Op, an element of the group (NTTP), and an inner
 *        expression.
 *
 * @details
 * The algebraic constraint @c IsGroup<T,Op> gives rise to the scout
 * type: a well-formed @c GroupScout exists exactly when @c (T, @c Op)
 * is a group, with @c Element a member of @c T (NTTP-encoded) and
 * @c Inner an inner scout/expression of type @c Inner.  This is the
 * "algebraic-over-architectural" stance applied to the comprehension
 * surface: the scout is gated on a textbook concept
 * (@c IsGroup), not on an implementation-specific marker.
 *
 * Per-operator factories (@c operator+, @c operator-, [future
 * @c operator*]) populate the @c Op and @c Element fields.  The type
 * itself stays uniform across the additive and multiplicative
 * specialisations.
 *
 * @c GroupScout is @b purely symbolic --- a type-level marker used by
 * the Set CTAD (Slice 2) to perform halfspace-pivot transport.  It has
 * @b no @c operator() and no @c Domain typedef, mirroring the
 * @c BoundScout convention (cf.\ @c sets/expressions.cppm:168-178:
 * "the scout is type-only --- it has no runtime state and no
 * operator() on plain values").  The carrier is recoverable as
 * @c GroupScout::carrier_type.
 *
 * @tparam T       The carrier type.
 * @tparam Op      The group operation (a callable type, e.g.\
 *                 @c std::plus<T> or @c std::multiplies<T>).
 * @tparam Element The group element (NTTP).
 * @tparam Inner   The inner expression type
 *                 (e.g.\ @c BoundScout<Ambient>).
 */
export template <typename T, typename Op, auto Element, typename Inner>
  requires dedekind::category::IsGroup<T, Op>
struct GroupScout {
  using carrier_type = T;
  using op_type = Op;
  using inner_type = Inner;

  static constexpr auto element = Element;

  /**
   * @brief Comprehension pipe: @c GroupScout @c | @c Halfspace →
   *        @c Comprehension with the halfspace's pivot shifted by
   *        @c Element.
   *
   * @details
   * Halfspace-pivot transport along the additive group action: a source
   * predicate @f$\{x \in T \mid x \bowtie k\}@f$ pushed through the
   * scout function @f$\lambda x.\,k_E + x@f$ (where @f$k_E@f$ is this
   * scout's @c Element) yields the image halfspace
   * @f$\{y \in T \mid y \bowtie k + k_E\}@f$ --- the pivot shifts by
   * @c Element, the direction and strictness are preserved (group
   * translation is monotone).
   *
   * The result is a @c Comprehension over the inner scout's @c Ambient,
   * with the @b shifted halfspace as predicate.  Downstream @c Set CTAD
   * (already present in @c :sets:expressions) picks this up unchanged
   * and produces a @c Set<T,L,Halfspace<T,k+k_E,...>> --- no Set CTAD
   * changes needed.
   *
   * @section Specialisation gate
   *
   * Currently restricted to @c Op @c = @c std::plus<T> (additive group
   * action) and @c Inner exposing @c AmbientType / @c ambient (i.e.\
   * a @c BoundScout).  Slice 3 adds the multiplicative case
   * (@c Op @c = @c std::multiplies<T>); composition of scouts
   * (e.g.\ @c bound<2> @c * @c in<ℤ> @c + @c bound<1>) is further
   * follow-up.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::plus<T>> && IsOrderedAdditiveGroup<T> &&
             requires {
               typename Inner::AmbientType;
               Inner::ambient;
             }
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L>) const {
    // Compute the shifted pivot in its natural arithmetic --- not via a
    // forced cast to @c T.  This preserves the original pivot's
    // structural NTTP type (e.g.\ @c int when @c T is a non-structural
    // proxy) and follows the @c Halfspace contract that @c Pivot may
    // legitimately differ in type from @c T (cf.\ @c halfspace.cppm:189
    // "Pivot may be a different structural type than T ... the carrier's
    // converting ctor / overload set handles the comparison").
    constexpr auto new_pivot = Pivot + Element;
    using NewHalfspace = dedekind::order::Halfspace<T, new_pivot, D, S, L>;
    using AmbientType = typename Inner::AmbientType;
    return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
        Inner::ambient, NewHalfspace{}};
  }

  /**
   * @brief Comprehension pipe (multiplicative): @c GroupScout @c |
   *        @c Halfspace → @c Comprehension with the halfspace's pivot
   *        scaled by @c Element, and the direction flipped if
   *        @c Element < 0 (#664 Slice 3).
   *
   * @details
   * Halfspace-pivot transport along the multiplicative-group action: a
   * source predicate @f$\{x \in T \mid x \bowtie k\}@f$ pushed through
   * the scout function @f$\lambda x.\,k_E \cdot x@f$ (where @f$k_E@f$
   * is this scout's @c Element) yields the image halfspace
   * @f$\{y \in T \mid y \bowtie' k \cdot k_E\}@f$, where:
   *   * if @c k_E > 0: direction and strictness preserved
   *     (@c ⋈' @c = @c ⋈);
   *   * if @c k_E < 0: direction reversed
   *     (@c >  @c ↔ @c <,  @c ≥ @c ↔ @c ≤); strictness preserved.
   *
   * The @c k_E = 0 case (where the halfspace collapses to the whole
   * carrier or the empty set depending on direction / strictness) is
   * Honest-Rejected by the @c Element @c != zero gate in the
   * requires-clause: callers can re-spell it as the appropriate
   * universal / empty Set explicitly.
   *
   * @section Specialisation gate
   *
   * Currently restricted to @c Op @c = @c std::multiplies<T> and
   * @c Inner exposing @c AmbientType / @c ambient (i.e.\ a
   * @c BoundScout).  Mirrors the additive specialisation above.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::multiplies<T>> &&
             IsOrderedMultiplicativeGroup<T> &&
             // Honest Rejection on the degenerate @c k_E = 0 case.
             (Element != decltype(Element){}) && requires {
               typename Inner::AmbientType;
               Inner::ambient;
             }
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L>) const {
    // Same natural-arithmetic principle as the additive pipe: the
    // scaled pivot keeps @c Pivot 's NTTP type when feasible (e.g.\
    // @c int * @c int → @c int when both come from @c bound<int>),
    // sidestepping non-structural carriers.
    constexpr auto new_pivot = Element * Pivot;
    using AmbientType = typename Inner::AmbientType;
    if constexpr (Element > decltype(Element){}) {
      // @c k_E > 0: direction and strictness preserved.
      using NewHalfspace = dedekind::order::Halfspace<T, new_pivot, D, S, L>;
      return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
          Inner::ambient, NewHalfspace{}};
    } else {
      // @c k_E < 0: direction flipped (Upward ↔ Downward), strictness
      // preserved.
      constexpr auto flipped = (D == dedekind::order::Direction::Upward)
                                   ? dedekind::order::Direction::Downward
                                   : dedekind::order::Direction::Upward;
      using NewHalfspace =
          dedekind::order::Halfspace<T, new_pivot, flipped, S, L>;
      return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
          Inner::ambient, NewHalfspace{}};
    }
  }
};

}  // namespace dedekind::algebra

// The factory operator overloads are declared in @c dedekind::sets ---
// the namespace of the @b first operand @c BoundScout --- so they
// participate in ADL when callers write @c in<T> @c + @c bound<k>
// without an explicit @c using @c dedekind::algebra::operator+
// directive.  This mirrors @c :order:halfspace 's placement of
// @c operator>(BoundScout,Bound) in @c dedekind::order (the namespace
// of @c Bound), reachable via ADL on either operand.
namespace dedekind::sets {

/**
 * @brief Additive specialisation: @c in<T> @c + @c bound<k> →
 *        @c GroupScout<T, @c std::plus<T>, @c k, @c BoundScout<T>>.
 *
 * @details
 * Gated on @c IsAdditiveGroup<T> @b and @c std::convertible_to<K,T> ---
 * the algebraic axioms guarantee the shift is well-defined, and the
 * convertibility check makes invalid @c K values fail in the
 * requires-clause rather than as hard errors during instantiation.
 *
 * Honest Rejection: writing @c in<ℕ> @c + @c bound<3> fails (ℕ is not
 * an additive group --- @c IsAdditiveGroup<Cardinality> @c ==
 * @c false), with the diagnostic naming the missing group axiom.
 *
 * The scout represents the function @c λx.k+x where @c + is the group
 * operation on @c T.
 */
export template <auto Ambient, auto K>
  requires dedekind::category::IsAbelianGroup<
               typename BoundScout<Ambient>::T,
               std::plus<typename BoundScout<Ambient>::T>> &&
           std::convertible_to<decltype(K), typename BoundScout<Ambient>::T>
constexpr auto operator+(BoundScout<Ambient>, dedekind::order::Bound<K>) {
  using T = typename BoundScout<Ambient>::T;
  return dedekind::algebra::GroupScout<T, std::plus<T>, K,
                                       BoundScout<Ambient>>{};
}

/**
 * @brief Additive group subtraction: @c in<T> @c - @c bound<k> →
 *        @c GroupScout<T, @c std::plus<T>, @c -k, @c BoundScout<T>>.
 *
 * @details
 * Encoded as the additive inverse: @c x @c - @c k @c = @c x @c + @c (-k).
 * Group axioms guarantee @c (-k) ∈ T (additive inverse exists for
 * every element).  The convertibility constraint applies to @c (-K)
 * since that is the value embedded in the resulting scout's
 * @c Element field.
 *
 * Honest Rejection on non-groups (ℕ): the negation @c -K cannot be
 * expressed in the carrier without an inverse, so the gate
 * @c IsAdditiveGroup<T> fails and the overload is removed.
 */
export template <auto Ambient, auto K>
  requires dedekind::category::IsAbelianGroup<
               typename BoundScout<Ambient>::T,
               std::plus<typename BoundScout<Ambient>::T>> &&
           std::convertible_to<decltype(-K), typename BoundScout<Ambient>::T>
constexpr auto operator-(BoundScout<Ambient>, dedekind::order::Bound<K>) {
  using T = typename BoundScout<Ambient>::T;
  return dedekind::algebra::GroupScout<T, std::plus<T>, -K,
                                       BoundScout<Ambient>>{};
}

/**
 * @brief Multiplicative specialisation: @c in<T> @c * @c bound<k> →
 *        @c GroupScout<T, @c std::multiplies<T>, @c k, @c BoundScout<T>>
 *        (#664 Slice 3).
 *
 * @details
 * Gated on @c IsAbelianGroup<T, std::multiplies<T>> @b and
 * @c std::convertible_to<K, T>.  Multiplicative-group axioms (every
 * non-zero element invertible) certify that scaling is well-defined;
 * the convertibility check makes invalid @c K values fail at the
 * requires-clause rather than as hard errors during instantiation.
 *
 * The scout represents the function @c λx.k @c * @c x where @c * is
 * the group operation on @c T.  The downstream
 * @c GroupScout::operator|(Halfspace) pipe (above) does the
 * direction-aware halfspace-pivot transport: @c k > 0 preserves the
 * direction, @c k < 0 flips it (Upward ↔ Downward), @c k = 0 is
 * Honest-Rejected at the pipe.
 *
 * Honest Rejection: writing @c in<ℤ> @c * @c bound<3> fails (post-#674,
 * @c IsAbelianGroup<SignedCardinality, std::multiplies> is @b false
 * --- the saturating ℤ proxy is a ring but @b not a field; ℤ has no
 * multiplicative inverses for non-units).  On ℚ the gate fires:
 * @c IsField<Rational<default_integer>> is the post-#674 witness that
 * @c (ℚ\\{0}, @c *) is a multiplicative abelian group.
 */
export template <auto Ambient, auto K>
  requires dedekind::category::IsAbelianGroup<
               typename BoundScout<Ambient>::T,
               std::multiplies<typename BoundScout<Ambient>::T>> &&
           std::convertible_to<decltype(K), typename BoundScout<Ambient>::T>
constexpr auto operator*(BoundScout<Ambient>, dedekind::order::Bound<K>) {
  using T = typename BoundScout<Ambient>::T;
  return dedekind::algebra::GroupScout<T, std::multiplies<T>, K,
                                       BoundScout<Ambient>>{};
}

}  // namespace dedekind::sets
