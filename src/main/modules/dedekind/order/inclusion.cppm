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

/** @brief @f$A \subseteq B@f$ is decidable @b via the structural meet: a
 *  subobject-lattice pair whose meet-then-equality is a truth value.  The gate
 *  for the @b generic identity @c <=; where the meet does not collapse this is
 *  unsatisfied (the honest @c exists / @c forall wall) and a per-carrier @c <=
 *  must supply the case. */
export template <typename A, typename B>
concept DecidableMeetSubset =
    SubobjectLatticePair<A, B> && requires(const A& a, const B& b) {
      { (a & b) == a } -> std::convertible_to<typename A::logic_species::Ω>;
    };

/** @brief A decidable @c <= exists for the pair --- generic (meet) @b or a
 *  per-carrier specialisation (@c Singleton membership, @c OrderInterval
 *  endpoints).  The gate for the derived relations, so they ride @b any
 *  @c <=, not only the meet-based one. */
export template <typename A, typename B>
concept HasSubset =
    SubobjectLatticePair<A, B> && requires(const A& a, const B& b) {
      { a <= b } -> std::convertible_to<typename A::logic_species::Ω>;
    };

/** @brief @c HasSubset plus a @c bool @c ==, so @f$A \subsetneq B \equiv A
 *  \subseteq B \wedge A \neq B@f$ is expressible. */
export template <typename A, typename B>
concept HasProperSubset = HasSubset<A, B> && requires(const A& a, const B& b) {
  { a == b } -> std::convertible_to<bool>;
};

/** @brief @f$A \subseteq B \iff A \cap B = A@f$ --- the generic identity.
 *  More-specialized per-carrier @c <= (@c Set / @c Ø / @c UniversalSet, the
 *  @c Singleton membership and @c OrderInterval endpoints below) win by partial
 *  ordering; this fills the gaps (@c Halfspace ⊆ @c Halfspace). */
export template <typename A, typename B>
  requires DecidableMeetSubset<A, B>
constexpr typename A::logic_species::Ω operator<=(const A& a, const B& b) {
  return (a & b) == a;  // A ⊆ B ⟺ A ∩ B = A (via the structural &)
}

/** @brief Superset, proper subset, proper superset --- each once from @c <= and
 *  @c ==, gated on @c HasSubset so they ride @b any decidable @c <= (generic or
 *  specialised).  Together they supply @c HasPartialOrderOperators. */
export template <typename A, typename B>
  requires HasSubset<B, A>
constexpr typename A::logic_species::Ω operator>=(const A& a, const B& b) {
  return b <= a;  // A ⊇ B := B ⊆ A
}
export template <typename A, typename B>
  requires HasProperSubset<A, B>
constexpr typename A::logic_species::Ω operator<(const A& a, const B& b) {
  using L = typename A::logic_species;
  return L::AND(a <= b, (a == b) ? L::False : L::True);  // A ⊊ B
}
export template <typename A, typename B>
  requires HasProperSubset<B, A>
constexpr typename A::logic_species::Ω operator>(const A& a, const B& b) {
  using L = typename A::logic_species;
  return L::AND(b <= a, (a == b) ? L::False : L::True);  // A ⊋ B := B ⊊ A
}

/** @brief @f$[a,b] \subseteq [c,d]@f$ by endpoint + strictness comparison
 *  (#831): an @c OrderInterval is the meet of an upward and a downward
 *  halfspace, so containment is the two halfspace containments --- decidable
 *  directly, no meet materialisation.  The per-carrier specialisation for
 *  intervals; it wins over the generic @c <= by partial ordering.
 *
 *  An empty left interval (a degenerate construction like @c (5,5), which
 *  @c OrderInterval represents rather than forbids) short-circuits to @c True:
 *  @f$\emptyset \subseteq X@f$ for every @c X, and the endpoint test alone
 *  would wrongly report @c False (#835 review). */
export template <typename T, auto ALo, auto AHi, Strictness ASL, Strictness ASU,
                 auto BLo, auto BHi, Strictness BSL, Strictness BSU, typename L>
constexpr typename L::Ω operator<=(
    const OrderInterval<T, ALo, AHi, ASL, ASU, L>&,
    const OrderInterval<T, BLo, BHi, BSL, BSU, L>&) {
  if constexpr (OrderInterval<T, ALo, AHi, ASL, ASU, L>::is_empty) {
    return L::True;  // ∅ ⊆ X
  } else {
    // Endpoint containment through the carrier-aware order (pivot_less /
    // pivot_equal), not raw NTTP comparison: a signed and an unsigned pivot
    // must rank by mathematical value, not by C++'s usual conversions (#835
    // review).  A's lower end sits inside B, and dually its upper end.
    constexpr bool lower =
        pivot_less<BLo, ALo>() ||
        (pivot_equal<ALo, BLo>() &&
         !(ASL == Strictness::NonStrict && BSL == Strictness::Strict));
    constexpr bool upper =
        pivot_less<AHi, BHi>() ||
        (pivot_equal<AHi, BHi>() &&
         !(ASU == Strictness::NonStrict && BSU == Strictness::Strict));
    return (lower && upper) ? L::True : L::False;
  }
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
