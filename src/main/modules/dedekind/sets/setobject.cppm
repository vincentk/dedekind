/**
 * @file dedekind/sets/setobject.cppm
 * @partition :setobject
 * @brief The noun: what it is to be an @b object of the category Set.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section setobject__Role
 * This partition holds ONE concept, @ref IsSetObject, and nothing else.  It
 * sits upstream of every other @c :sets partition: @c :boundaries realises the
 * two trivial set objects (Ø and 𝔸) and asserts against it at their
 * definition, @c :expressions supplies the CRTP spelling (@c SetExpr) and the
 * opaque arm (@c Comprehension), and the @c :order arms (@c Halfspace,
 * @c Singleton, @c SetVal) are the structured normal forms.  Whatever leaves
 * the library --- the Python surface's @c Set --- is pinned against it as a
 * MUST (RFC 2119).
 *
 * The concept depends on @c category alone (@c IsSubobject), so it can be the
 * first partition; that is the point of giving the noun its own file rather
 * than tucking it behind a helper.
 *
 * @see F. W. Lawvere, "An elementary theory of the category of sets",
 *      Proc. Nat. Acad. Sci. 52 (1964): the objects of Set are characterised
 *      by their arrows alone; this partition names that characterisation.
 */
module;

#include <concepts>
#include <type_traits>  // std::remove_cvref_t (universe_t)
#include <utility>      // std::declval (universe_t)

export module dedekind.sets:setobject;

import dedekind.category;

namespace dedekind::sets {

using dedekind::category::IsSubobject;

/**
 * @concept IsSetObject
 * @brief An @b object of the category Set --- a set is @f$(U, \chi)@f$: a
 *        @b reified @b universe and a @b characteristic map
 *        @f$\chi : T \to \Omega_L@f$ into a @b named logic species.
 *
 * @details The model in one sentence: the set is (reified universe, χ); χ's
 * decidable normal forms form a per-carrier, meet-closed coproduct whose
 * discriminant is the only "flag", and everything that escapes it ---
 * complements of intervals, cofinite sets, opaque predicates --- lives honestly
 * as a term with pointwise χ, where Rice says it must.
 *
 * @code
 *   SetObject(T, L)  =  UNIVERSE  ×  CHARACTERISTIC  χ : T → Ω_L
 *
 *   UNIVERSE (π_1)                 the reified type constraint
 *     carrier      T
 *     cardinality  C               (ℵ_0, Finite, ...)
 *     logic        L               Ω_L a De Morgan algebra (Truth<L>)
 *     decidable == ?               a property of T, not of being a set
 *     ≡ 𝔸<T, L, C>      𝔸 is its own universe: the fixpoint
 *
 *   CHARACTERISTIC (π_2)           one of two kinds of predicate
 *     STRUCTURED   ⊥ ⊕ ⊤ ⊕ Halfspace ⊕ Singleton ⊕ Interval ⊕ ...
 *                  flat normal-form values, not trees; meet-closed;
 *                  ==/≤/∩ decided by arm-pair overloads (ADL, in the
 *                  carrier's own module)
 *     OPAQUE       the Default arm: a callable, or a lattice term
 *                  Meet / Join / Not over leaves --- "the AST is the set";
 *                  χ pointwise; ==/≤ undecidable → the cycle → Unknown
 * @endcode
 *
 * This is the mereological / product model recorded on #824 / #826 (the
 * ambient is the whole, χ the part-selector), named as a concept.  It is
 * @b structural and deliberately light --- a shape, not a type hierarchy: it
 * refines @c category::IsSubobject (an object @b in Set: χ by call shape, the
 * inclusion @c ι, the domain tie) by requiring the codomain lattice to be a
 * @b named @c logic_species with @c Codomain @c = @c logic_species::Ω, the
 * handle through which De Morgan (pointwise, free) and the undecided verdict
 * @c Unknown reach the set.  It is @b not @c category::IsSet: that is the
 * @b category (the ETCS axioms, among them the NNO).  The NNO is an axiom of
 * the category, not of an object, so successor is deliberately absent here;
 * and whether @c == is decidable is a property of the universe's carrier, not a
 * requirement of being a set.  @c SetExpr (@c :expressions) is the CRTP mixin
 * that realises this surface (@c Singleton, @c Halfspace, @c SetVal derive
 * from it); @c Set realises it by hand.
 *
 * @section setobject__Legs
 * The two legs are @b named customization points, @c universe(s) and
 * @c classifier(s), found by ADL (every set object lives in, or derives from
 * a mixin in, @c dedekind::sets, so one default per leg in @c :boundaries
 * serves them all; a type with better knowledge overloads in its own module).
 * They are deliberately @b not @c π_1 / @c π_2: one object cannot carry two
 * products under the same projection names, and the honest @c IsProduct here
 * belongs to the @b universe of a pair carrier, @c 𝔸<A×B> @c ≅ @c 𝔸<A> @c ×
 * @c 𝔸<B> (§4: a relation's @c dom / @c cod are @c π_1 / @c π_2 of its
 * universe), while a lattice node @c Meet<A,B> is already the product of its
 * @b operands.  Had @c S itself projected to (universe, χ) through @c π_1, the
 * universe of a relation could not also project to its factors.  The paper's
 * reading survives intact: a set is a subobject of a universe over a regular
 * carrier (Definition Lwv), the predicate @b is the set, and the universe is
 * the reified type constraint the Python surface hands out.
 *
 * @code
 *   universe(s)    : S → 𝔸<Domain, L, C>     the reified type constraint;
 *                                            𝔸 is its own universe (fixpoint)
 *   classifier(s)  : S → (Domain → Ω_L)      the χ datum: the set itself for
 *                                            the structured arms and for a
 *                                            comprehension over a restricted
 *                                            base (the AST IS the set); the
 *                                            bare predicate P for a
 *                                            comprehension over a universe
 *   π_1 / π_2      : 𝔸<A×B> → 𝔸<A> / 𝔸<B>    the universe of a pair carrier
 *                                            is the product of the factors
 * @endcode
 */

// The surface is the category-level noun, category::IsLSet (a Goguen L-set):
// no legs yet, so the universe leg of IsSetObject below does not recurse.
using dedekind::category::IsFiniteLSet;
using dedekind::category::IsLSet;

/** @brief A @b universe: the terminal object of @c Sub(T) --- the reified type
 *  constraint @c 𝔸<T,L,C> itself (carrier, logic, cardinality class). */
export template <typename U>
concept Is𝔸 = IsLSet<U> && dedekind::category::IsTerminalObject<U>;

/** @brief @c U is the universe leg @b of @c S: a universe over the same
 *  carrier under the same logic. */
export template <typename U, typename S>
concept Is𝔸Of =
    Is𝔸<U> && std::same_as<typename U::Domain, typename S::Domain> &&
    std::same_as<typename U::logic_species, typename S::logic_species>;

/** @brief The @b leaf case: a set object that carries the subobject surface
 *  itself (Definition Lwv) together with its two legs. */
template <typename S>
concept IsSetObjectLeaf =
    IsLSet<S> && requires(const S& s, const typename S::Domain& x) {
      /** @brief The universe leg: the reified type constraint. */
      { universe(s) } -> Is𝔸Of<S>;
      /** @brief The classifier leg: a χ datum callable on the carrier. */
      classifier(s)(x);
    };

/** @brief Two types carry the same carrier: the domain tie a lattice node over
 *  set objects needs (the logic may differ; the codomain leg reconciles it). */
template <typename A, typename B>
concept SameCarrier = requires {
  typename A::Domain;
  typename B::Domain;
} && std::same_as<typename A::Domain, typename B::Domain>;

template <typename T>
struct is_set_object : std::bool_constant<IsSetObjectLeaf<T>> {};
template <typename A, typename B>
struct is_set_object<dedekind::category::Meet<A, B>>
    : std::bool_constant<is_set_object<A>::value && is_set_object<B>::value &&
                         SameCarrier<A, B>> {};
template <typename A, typename B>
struct is_set_object<dedekind::category::Join<A, B>>
    : std::bool_constant<is_set_object<A>::value && is_set_object<B>::value &&
                         SameCarrier<A, B>> {};
template <typename A>
struct is_set_object<dedekind::category::Not<A>> : is_set_object<A> {};

/** @brief An @b object of the category Set: a leaf carrying the subobject
 *  surface and its legs, @b or a lattice node --- @c Meet / @c Join / @c Not
 *  --- over set objects on one carrier.  The node case is @b structural: the
 *  node's χ is the pointwise evaluation @c category:lattice already gives it
 *  (an arrow into Ω), its universe is its operands', its classifier is the
 *  node itself.  This is the opaque arm, "the AST is the set", without any
 *  set semantics in the lattice partition: the member shape, the inclusion
 *  @c ι and the pullback / pushout apex legs are a @c sets-side view
 *  (@c as_pullback / @c as_pushout in @c :expressions), not a property of the
 *  node.  The same-logic requirement of the leaf surface is @b not imposed on
 *  nodes: @c Ø<T,Kleene> @c ∧ @c S<T,Boole> is a set object whose reduction the
 *  bounded law and the codomain leg decide. */
export template <typename S>
concept IsSetObject =
    is_set_object<std::remove_cvref_t<S>>::value;  // decays: decltype(a & b) is
                                                   // const

/** @brief The type of a set object's universe leg. */
export template <IsSetObject S>
using universe_t =
    std::remove_cvref_t<decltype(universe(std::declval<const S&>()))>;

}  // namespace dedekind::sets
