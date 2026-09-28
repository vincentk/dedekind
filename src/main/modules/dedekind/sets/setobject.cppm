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
 *     ≡ Universe<T, L, C>      𝔸 is its own universe: the fixpoint
 *
 *   CHARACTERISTIC (π_2)           one of two kinds of predicate
 *     STRUCTURED   ⊥ ⊕ ⊤ ⊕ Halfspace ⊕ Singleton ⊕ Meet<H↑,H↓> ⊕ ...
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
 * that realises this surface; @c Set / @c SingletonSet realise it by hand.
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
 *                                            comprehension (the AST IS the
 *                                            set); Set<T,L,P>'s predicate P
 *   π_1 / π_2      : 𝔸<A×B> → 𝔸<A> / 𝔸<B>    the universe of a pair carrier
 *                                            is the product of the factors
 * @endcode
 */

/** @brief The set-object @b surface alone (Definition Lwv, §3): a subobject
 *  of a @c std::regular carrier whose codomain is the @c Ω of a named logic
 *  species.  No legs yet --- this is what @ref IsUniverse refines, so the
 *  universe leg of @ref IsSetObject does not recurse. */
export template <typename S>
concept IsSetObjectSurface =
    std::regular<typename S::Domain> && IsSubobject<S, typename S::Domain> &&
    requires { typename S::logic_species; } &&
    std::same_as<typename S::Codomain, typename S::logic_species::Ω>;

/** @brief A @b universe: the terminal object of @c Sub(T) --- the reified type
 *  constraint @c 𝔸<T,L,C> itself (carrier, logic, cardinality class). */
export template <typename U>
concept IsUniverse =
    IsSetObjectSurface<U> && dedekind::category::IsTerminalObject<U>;

/** @brief @c U is the universe leg @b of @c S: a universe over the same
 *  carrier under the same logic. */
export template <typename U, typename S>
concept IsUniverseOf =
    IsUniverse<U> && std::same_as<typename U::Domain, typename S::Domain> &&
    std::same_as<typename U::logic_species, typename S::logic_species>;

export template <typename S>
concept IsSetObject =
    IsSetObjectSurface<S> && requires(const S& s, const typename S::Domain& x) {
      /** @brief The universe leg: the reified type constraint. */
      { universe(s) } -> IsUniverseOf<S>;
      /** @brief The classifier leg: a χ datum callable on the carrier. */
      classifier(s)(x);
    };

/** @brief The type of a set object's universe leg. */
export template <IsSetObject S>
using universe_t =
    std::remove_cvref_t<decltype(universe(std::declval<const S&>()))>;

}  // namespace dedekind::sets
