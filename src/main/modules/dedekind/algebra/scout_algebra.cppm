/**
 * @file dedekind/algebra/scout_algebra.cppm
 * @partition :scout_algebra
 * @brief Symbolic scout type for the in-line comprehension surface,
 *        gated on algebraic structure of the carrier (#664 Slice 1).
 *
 * @section scout_algebra__Motivation
 *
 * The algebraic constraints give rise to the symbolic scout type.  A
 * @c GroupScout<T,Op,Element,Inner> exists exactly when @c (T, @c Op)
 * is a group --- the gate is purely algebraic, in textbook vocabulary,
 * not in implementation-specific concepts.  This partition is the
 * algebraic foundation for #664's in-line scout-algebra surface
 * @c Set{f(x) @c | @c P(x)}: once a scout is a valid algebraic
 * expression, downstream slices wire it into the comprehension DSL.
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
 *
 * The multiplicative specialisation
 * (@c operator*(Bound<k>, @c BoundScout<T>) for
 * @c IsMultiplicativeGroup<T>) is deferred to Slice 3.
 *
 * @section scout_algebra__Honest_Rejection
 *
 * Carriers that do @b not satisfy @c IsAdditiveGroup --- e.g.\
 * @c sets::Cardinality (ℕ has no additive inverses), signed @c int
 * (overflow is UB so the group axioms fail) --- cause the operator
 * overload to be removed from the candidate set.  The compile error
 * names the missing textbook axiom, not a library-specific failure.
 *
 * @section scout_algebra__Out_Of_Scope_For_This_Slice
 *
 *  - @c MembershipBinding<GroupScout<...>> @c | @c predicate routing
 *    (Slice 2).
 *  - Halfspace-pivot transport through the scout
 *    (@c Halfspace<T,k,↑,S,L> shifted to @c Halfspace<T,k+Element,↑,S,L>)
 *    (Slice 2).
 *  - Set CTAD deduction guide for transformed-scout comprehensions
 *    (Slice 2).
 *  - Multiplicative specialisation @c operator* (Slice 3).
 *  - Retract-tier scaling on rings (where the carrier is not a
 *    multiplicative group, e.g.\ @c bound<2> @c * @c in<ℤ>) (further
 *    follow-up).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:scout_algebra;

import dedekind.category; // IsGroup<T, Op>
import dedekind.sets;     // BoundScout<auto>
import dedekind.order;    // Bound<auto>
import :group;            // IsAdditiveGroup<T, Add>

namespace dedekind::algebra {

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
 * @c GroupScout exposes @c Domain / @c Codomain typedefs and an
 * @c operator()(Domain) that applies @c Op(Element, Inner(x)) --- so a
 * @c GroupScout is itself @c IsArrow-shaped, ready for downstream
 * use in the Set CTAD (Slice 2).
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
  using Domain = typename Inner::Domain;
  using Codomain = T;
  using op_type = Op;
  using inner_type = Inner;

  static constexpr auto element = Element;

  /**
   * @brief Evaluate the scout: apply @c Op(Element, @c Inner(x)).
   *
   * Group axioms guarantee closure (@c Op(Element, @c Inner(x)) ∈ T),
   * associativity (composition of scouts composes by composing
   * elements under @c Op), and inverse (every @c GroupScout has a
   * @c GroupScout inverse with element @c Op⁻¹(Element)).
   */
  constexpr T operator()(const Domain& x) const {
    return Op{}(static_cast<T>(Element), Inner{}(x));
  }
};

/**
 * @brief Additive specialisation: @c in<T> @c + @c bound<k> →
 *        @c GroupScout<T, @c std::plus<T>, @c k, @c BoundScout<T>>.
 *
 * @details
 * Gated on @c IsAdditiveGroup<T>.  Honest Rejection: writing
 * @c in<ℕ> @c + @c bound<3> fails (ℕ is not an additive group ---
 * @c IsAdditiveGroup<Cardinality> @c == @c false), with the diagnostic
 * naming the missing group axiom.
 *
 * The scout represents the function @c λx.k+x where @c + is the group
 * operation on @c T.
 */
export template <auto Ambient, auto K>
  requires dedekind::algebra::IsAdditiveGroup<
      typename dedekind::sets::BoundScout<Ambient>::T>
constexpr auto operator+(dedekind::sets::BoundScout<Ambient>,
                         dedekind::order::Bound<K>) {
  using T = typename dedekind::sets::BoundScout<Ambient>::T;
  return GroupScout<T, std::plus<T>, K, dedekind::sets::BoundScout<Ambient>>{};
}

/**
 * @brief Additive group subtraction: @c in<T> @c - @c bound<k> →
 *        @c GroupScout<T, @c std::plus<T>, @c -k, @c BoundScout<T>>.
 *
 * @details
 * Encoded as the additive inverse: @c x @c - @c k @c = @c x @c + @c (-k).
 * Group axioms guarantee @c (-k) ∈ T (additive inverse exists for
 * every element).  Honest Rejection on non-groups (ℕ): the negation
 * @c -K cannot be expressed in the carrier without an inverse, so the
 * gate @c IsAdditiveGroup<T> fails and the overload is removed.
 */
export template <auto Ambient, auto K>
  requires dedekind::algebra::IsAdditiveGroup<
      typename dedekind::sets::BoundScout<Ambient>::T>
constexpr auto operator-(dedekind::sets::BoundScout<Ambient>,
                         dedekind::order::Bound<K>) {
  using T = typename dedekind::sets::BoundScout<Ambient>::T;
  return GroupScout<T, std::plus<T>, -K, dedekind::sets::BoundScout<Ambient>>{};
}

}  // namespace dedekind::algebra
