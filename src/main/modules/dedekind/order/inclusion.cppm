/**
 * @file dedekind/order/inclusion.cppm
 * @partition :inclusion
 * @brief The inclusion (subset) order on subobject lattices, derived from the
 *        structural meet and equality (#831).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @details Subset is the lattice identity @f$A \subseteq B \iff A \cap B = A@f$
 * (Birkhoff §1.4), gated on the canonical rung @c category::IsSubobjectLattice;
 * the strict / dual relations derive from @c <= and @c ==.  Together they
 * supply the surface @c HasPartialOrderOperators names.
 *
 * It computes via the @b structural @c & operator, @b not the CT-vocabulary
 * @c meet: the two diverge for structured carriers (#834) --- @c & rides
 * @c structured_and and collapses (@c {x>5} @c ∩ @c {x>3} @c = @c {x>5}), so
 * only @c & makes the subset @b decidable.  Per the operating model, the
 * identity is the @b default and each concrete carrier proves its own
 * decidability by a @b specialised @c <= (@c Ø / @c Ω trivial, @c Singleton
 * membership, non-empty @c Halfspace by pivot); those win by partial ordering,
 * and this generic fills the gaps.
 */

module;

#include <concepts>
#include <type_traits>

export module dedekind.order:inclusion;

import dedekind.category; // IsSubobjectLattice, IsSet, IsLogicalSpecies
import :halfspace;        // Singleton (its membership <=), Halfspace (& ladder)

namespace dedekind::order {

/** @brief A compatible pair of subobject lattices --- same ambient @c Domain
 *  and @c logic_species: the canonical rung on which subset is defined.  Ties
 *  the order to @c category::IsSubobjectLattice rather than a bespoke shape;
 *  the same-logic clause is the cross-logic-mereology guard (#833 review). */
export template <typename A, typename B>
concept SubobjectLatticePair =
    dedekind::category::IsSubobjectLattice<A> &&
    dedekind::category::IsSubobjectLattice<B> &&
    std::same_as<typename A::Domain, typename B::Domain> &&
    std::same_as<typename A::logic_species, typename B::logic_species>;

/** @brief @f$A \subseteq B@f$ is @b decidable: a subobject-lattice pair whose
 *  structural meet-then-equality is a truth value in the ambient logic.  Where
 *  the meet does not collapse, this is unsatisfied --- the honest @c exists /
 *  @c forall wall, no guess. */
export template <typename A, typename B>
concept DecidableSubset =
    SubobjectLatticePair<A, B> && requires(const A& a, const B& b) {
      { (a & b) == a } -> std::convertible_to<typename A::logic_species::Ω>;
    };

/** @brief Its strict refinement: additionally @c == is a @c bool, so
 *  @f$A \subsetneq B \equiv A \subseteq B \wedge A \neq B@f$ is expressible. */
export template <typename A, typename B>
concept DecidableProperSubset =
    DecidableSubset<A, B> && requires(const A& a, const B& b) {
      { a == b } -> std::convertible_to<bool>;
    };

/** @brief @f$A \subseteq B \iff A \cap B = A@f$.  More-specialized per-carrier
 *  @c <= (@c Set / @c Ø / @c UniversalSet, the @c Singleton membership below)
 *  win by partial ordering; this fills the gaps (@c Halfspace ⊆ @c Halfspace).
 */
export template <typename A, typename B>
  requires DecidableSubset<A, B>
constexpr typename A::logic_species::Ω operator<=(const A& a, const B& b) {
  return (a & b) == a;  // A ⊆ B ⟺ A ∩ B = A (via the structural &)
}

/** @brief Superset, proper subset, proper superset --- each once from @c <= and
 *  @c ==, combined in the ambient logic.  Together they supply
 *  @c HasPartialOrderOperators. */
export template <typename A, typename B>
  requires DecidableSubset<B, A>
constexpr typename A::logic_species::Ω operator>=(const A& a, const B& b) {
  return b <= a;  // A ⊇ B := B ⊆ A
}
export template <typename A, typename B>
  requires DecidableProperSubset<A, B>
constexpr typename A::logic_species::Ω operator<(const A& a, const B& b) {
  using L = typename A::logic_species;
  return L::AND(a <= b, (a == b) ? L::False : L::True);  // A ⊊ B
}
export template <typename A, typename B>
  requires DecidableProperSubset<B, A>
constexpr typename A::logic_species::Ω operator>(const A& a, const B& b) {
  using L = typename A::logic_species;
  return L::AND(b <= a, (a == b) ? L::False : L::True);  // A ⊋ B := B ⊊ A
}

/** @brief @f$\{V\} \subseteq S \iff V \in S@f$: a singleton is a subset iff its
 *  sole point is a member --- the membership base case, decidable whenever
 *  @c S's χ is.  Calls the classifier @c other(V) directly (@c IsSet guarantees
 *  @c operator(), not @c contains); the universal set is excluded so its own
 *  @c X ⊆ Ω overload stays unambiguous. */
export template <auto V, typename L, typename S>
  requires(
      dedekind::category::IsSet<S> &&
      std::same_as<typename S::logic_species, L> &&
      !requires { typename S::is_universal_boundary; } &&
      requires(const S& s) { s(V); })
constexpr typename L::Ω operator<=(const Singleton<V, L>&, const S& other) {
  return other(V);
}

}  // namespace dedekind::order
