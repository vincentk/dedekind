/**
 * @file dedekind/sets/quantifier.cppm
 * @partition :quantifier
 * @brief Bounded first-order quantifiers as @b set operations: @f$\exists@f$ is
 *        non-emptiness of a comprehension, @f$\forall@f$ its
 * @f$\neg\exists\neg@f$ dual.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section quantifier__The_Definition
 * A quantifier @b is a set operation (the Bourbaki / structural reading).  We
 * take that literally as the definition, not merely as a characterisation:
 * @f[ \exists x \in S.\,P(x) \;:\equiv\; \{x\in S \mid P(x)\} \neq \varnothing,
 *     \qquad
 *     \forall x \in S.\,P(x) \;:\equiv\; \neg\,\exists x \in S.\,\neg P(x). @f]
 * In code the comprehension is the meet @c S @c & @c P (#826): @c exists is
 * @c !(Ø{} @c == @c (S @c | @c P)) and @c forall is @c S @c == @c (S @c | @c
 * P). There is no separate fold; the reduction lives entirely in the @c == the
 * comprehension's fragment supplies, and the Rice boundary is therefore a
 * @b type check (an operand with no suitable @c == overload is a compile error)
 * rather than hand-waved prose.
 *
 * @section quantifier__L_Valued
 * The set-level forms take any L-set (Goguen) and answer in its @c L: with
 * @c == the internal logic's equality @f$\bigwedge_x (\chi \Leftrightarrow
 * \chi')@f$, @f$\exists = \neg(\varnothing = S|P) = \bigvee (\chi_S \wedge
 * \chi_P)@f$ by De Morgan, and @f$\forall = ((S|P) = S) = \bigwedge \chi_P@f$
 * on a @b decidable @c S (@c HasDecidableMembership, or a boundary in any
 * species). So @c Unknown is a verdict, not a failure, and there is still no
 * separate fold: whatever @c == a fragment supplies (on a chain, the run
 * lists of @c dedekind.order) is the quantifier.  Boolean sets answer @c bool
 * as before.
 *
 * @build_order after :cardinality
 * @dependency :category
 */
module;

#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::move

export module dedekind.sets:quantifier;

import dedekind.category; // IsSet, ambient_set (the domain-is-a-set witness)
import :boundaries;       // Ø — the emptiness anchor the quantifiers compare to
import :expressions;      // the set former and the reducer's nodes over 𝔹
import :computability;    // HasDecidableMembership: the Δ-reader the bounded ∀
                          // asks of S

namespace dedekind::sets {

/**
 * @section quantifier__Finite_Quotient
 * @brief Quantifiers over a @b finite carrier, decided @b by type through the
 *        finite quotient rather than by enumeration.
 *
 * @details The comprehension is the meet @c S @c & @c P: on
 * @f$\mathbb{B}@f$ a fragment collapses structurally (@c π==v to a @c
 * Singleton, @c π⋈v to a @c Halfspace), each carrying a finite @c ==Ø /
 * @c ==𝔸 over @c {false, true}; on @f$\mathbb{N}@f$ a congruence @c π%n==r
 * materialises to a @c ResidueClass, decided analytically. No range, no
 * @c begin/end walk. See the §3.1 exhibit.
 */

/** @brief A where-clause over the set @c S: a fragment @c P for which
 *  @c S @c & @c P is the comprehension @f$\{x \in S \mid P(x)\}@f$ --- a datum
 *  (@c π @c == @c v, @c π @c > @c v) or a carrier-agnostic fragment (@c π @c ==
 *  @c c, @c π @c % @c n @c == @c r), which has no domain until
 * the former binds it.  Not a set: @c S @c & @c T would be the meet of two
 * sets.  What the former returns is the fragment's business (a comprehension, a
 * residue set), so only its acceptance is asked.
 *  @tparam P the fragment.  @tparam S the set it refines. */
export template <typename P, typename S>
concept IsWhereClause = !dedekind::category::IsLSet<std::remove_cvref_t<P>> &&
                        requires(const S& s, P p) { s & p; };

// A quantifier is one comparison of the intensional comprehension @c s&p
// against a lattice bound (Eqn 2): @c exists tests against @c ∅ (scheme A),
// @c forall against the input set @c S (scheme B); each recovers its partner by
// complementing the predicate, @c !P.  Generic over any @c IsSet domain @c S
// and a query @c P for which the where-clause @c s|p is well-formed: the @c |
// materialises the
// comprehension per carrier (@c 𝔹 here, ℤ/Nℤ in :numbers). The point-free query
// @c P is an UNBOUND fragment (@c π == v, @c π % n == r, a
// halfspace) --- it has no @c Domain, so it is not
// @c IsPredicate; the gate is simply that @c s @c | @c p is well-formed (the
// where-clause materialises the comprehension).  A raw lambda has no @c
// operator| against the universe, so it is refused.
// The predicate must be an UNBOUND fragment, never an @c IsSet: for a
// set-valued operand @c s|p resolves to set UNION (@c 𝔸|Ø = 𝔸), not the
// comprehension, which would make @c exists(𝔸<bool>, Ø{}) wrongly true.
// Excluding @c IsSet keeps @c | bound to the where-clause here.
/** @brief A verdict in the species @c L: an @c == that answers in @c L::Ω
 *  passes through, one that answers @c bool (a sized or structural equality)
 *  is lifted to ⊥ / ⊤.  @tparam L the species.  @param v the verdict. */
template <typename L>
constexpr typename L::Ω in_species(auto v) {
  if constexpr (std::same_as<std::remove_cvref_t<decltype(v)>, typename L::Ω>)
    return v;
  else
    return v ? L::True : L::False;
}

export template <dedekind::category::IsLSet S, IsWhereClause<S> P>
constexpr typename S::logic_species::Ω exists(const S& s, P p) {
  using L = typename S::logic_species;
  // (A): ¬({x ∈ S | P(x)} = ∅) = ⋁ (χ_S ∧ χ_P), in L.
  return L::RFL(
      in_species<L>(Ø<typename S::Domain, L>{} == (s & std::move(p))));
}

/** @brief Bounded @f$\forall@f$ over a @b decidable @c S (its χ in 𝔹: the Δ
 *  grade, @c HasDecidableMembership) or a boundary in any species (χ constant).
 *  There the identity @f$\forall x \in S.\,P \iff (S|P = S)@f$ holds in @c L,
 *  because for two-valued @f$\chi_S@f$ the internal equality
 *  @f$(\chi_S \wedge \chi_P \Leftrightarrow \chi_S)@f$ coincides with the
 *  implication @f$\chi_S \Rightarrow \chi_P@f$.  In a Heyting algebra that is
 *  a theorem for every @f$\chi_S@f$; Ω here is a De Morgan chain (#901) whose
 *  material implication is not a residuum, so over an L-valued @c S the
 *  bounded @f$\forall = \bigwedge (\chi_S \Rightarrow \chi_P)@f$ needs the
 *  chain's residuated implication, which is #980's remainder. */
export template <dedekind::category::IsLSet S, IsWhereClause<S> P>
  requires HasDecidableMembership<S> || dedekind::category::IsBoundaryObject<S>
constexpr typename S::logic_species::Ω forall(const S& s, P p) {
  using L = typename S::logic_species;
  // (B): S = {x ∈ S | P(x)} = ⋀ χ_P, in L.  S first: the species' Ω-valued ==
  // is then called directly; a reversed candidate would have to return bool.
  return in_species<L>(s == (s & std::move(p)));
}

/** @section quantifier__Formal_Verification */

// The finite-quotient witnesses (𝔸<bool> with a structural Singleton predicate,
// 𝔸<Cardinality> with a Congruence) live in :numbers, where the structural
// IsPredicate operands (dedekind.order Singleton, morphologies Congruence) are
// in scope.  Source for the §3.1 exhibit (lst:quantifiers_def).

}  // namespace dedekind::sets
