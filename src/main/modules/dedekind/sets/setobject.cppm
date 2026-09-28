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
 *     ≡ UniversalSet<T, L, C>      𝔸 is its own universe: the fixpoint
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
 * @section setobject__MUST
 * The intended refinement (RFC 2119 @b MUST, today a @b SHOULD) adds the
 * product itself: @c IsProduct<S, @c S::Universe, @c S::Classifier> --- a set
 * object @b projects (@c π_1) to its universe and (@c π_2) to its classifier,
 * making the mereological product structural rather than nominal.  (Since
 * @c 𝔸 is the ⊤ of @c Sub(T), this is @c S @c ≅ @c ⊤ @c × @c S: degenerate as
 * a product in @c Sub(T), yet informative as a reification, since the universe
 * carries the type descriptor.)  Nothing in the layering blocks it:
 * @c IsProduct calls @c π_1 / @c π_2 unqualified inside a requires-expression,
 * so satisfaction finds a carrier's overloads by ADL wherever they are
 * defined.  What the clause needs is for every set object to @b name its two
 * legs --- an associated @c Universe (the reified type constraint; @c 𝔸 is its
 * own universe, the fixpoint) and @c Classifier (the χ data: a predicate for
 * the opaque arm, the normal-form fields for a structured arm) --- with
 * @c π_1 / @c π_2 reading them off.  @c Comprehension<Base,P> already IS that
 * product as data; @c Set, @c Ø / @c 𝔸 and the @c :order arms have to spell
 * theirs out.  Tracked in #970: add the legs, then the @c IsProduct clause,
 * and the SHOULD becomes a MUST.
 */
export template <typename S>
concept IsSetObject = IsSubobject<S, typename S::Domain> && requires {
  typename S::logic_species;
} && std::same_as<typename S::Codomain, typename S::logic_species::Ω>;

}  // namespace dedekind::sets
