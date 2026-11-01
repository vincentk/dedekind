/**
 * @file dedekind/category/lattice_term.cppm
 * @partition :lattice_term
 * @brief The assembled lattice-law term reducer: the normal form of a term
 *        over a bounded lattice, built from the induced laws in @c :lattice,
 *        as one algebra for @c cata, its type at compile time and its value at
 *        run time.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section lattice_term__Overview
 * @c :lattice defines the term AST (`Meet`, `Join`, `Not`) and, next to each
 * lattice concept, the equational law it @b induces as a decomposable,
 * independently testable part.  This partition is the @b assembly: @c Reduce
 * is the algebra that folds those parts into one normal form under @c cata
 * (@c :f_algebra), applying exactly the laws the carrier proves; @c reduce_t
 * is that normal form's type.  Terms are compared @b only as normal forms,
 * where structural equality is equality of what they denote: a node-by-node
 * @c == on unreduced terms would give false negatives (@c a∧b against
 * @c b∧a), so the term nodes define none (#1116).  Reduction power scales
 * with the carrier's
 * structure: a bounded chain collapses the most, and @c bool (the two-element
 * Boolean lattice) is the optimal witness where every law fires.
 *
 * It is the engine of the set-expression collapse (epic #888/#890): @c sets
 * maps `& ↦ Meet`, `| ↦ Join`, `~ ↦ Not`, `𝔸 ↦ ⊤`, `Ø ↦ ⊥` and injects the
 * carrier's `structured_and` / `structured_or` as the leaf-combine.
 *
 * @section lattice_term__Law_Surface
 * The induced laws assembled here (all in @c :lattice): unit / annihilator
 * (@c meet_bounded_law), idempotence (@c idempotent_law), @b structural
 * absorption @c a∧(a∨b)=a (@c meet_structural_absorption_law), @b glb/lub
 * collapse of `≤`-comparable operands (@c meet_glb_law), @b distributivity
 * @c X∧(P∨Q)→(X∧P)∨(X∧Q) toward DNF (@c meet_distributivity_law; @b one
 * direction — meet over join — for termination, @b not the join-over-meet CNF
 * dual), and @b De Morgan negation @c ¬¬A→A / @c ¬(A∧B)→¬A∨¬B
 * (@c de_morgan_law), plus commutative canonicalisation by an @b injected total
 * order.  The complement @b collapse @c a∧¬a→⊥ (needing a genuinely
 * complemented lattice) and associativity-flattening remain deferred (#890).
 *
 * @section lattice_term__Ordering
 * Two orders enter, both @b injected (the Juliet posture: the engine clicks
 * against a concept, the caller asserts it over exactly its carriers):
 *  - the @b semantic order @c Ord that defines the glb/lub collapse and
 *    boundedness (see
 *    @c :lattice); default @c canonical_order is the carrier's @c
 * std::less_equal chain.  A carrier may bear several lattice orders, so the
 * caller supplies the one its `∧`/`∨` induce.
 *  - the @b total order @c Less, used only to canonicalise commutative
 * operands. It is partial-safe: an @c Unknown (undecidable) comparison leaves
 * the operands in authoring order (the honest fallback).
 *
 * Wikipedia: "Lattice (order)", "Absorption law", "Rewriting".
 *
 * @note "Never in the history of mathematics has a mathematical theory been the
 *  object of such vociferous vituperation as lattice theory."
 *  — Gian-Carlo Rota, "The Many Lives of Lattice Theory", Notices of the AMS
 *  44(11), 1997, p. 1440.
 */
module;

#include <concepts>
#include <functional>  // std::logical_and (the Boolean-lattice glb witness)
#include <type_traits>
#include <utility>  // std::forward

export module dedekind.category:lattice_term;

import :lattice;  // the term AST + the induced laws (the validated parts)
import :logic;    // Boole (default), Ternary, IsOckhamAlgebra
import :cartesian_bicategory;  // Copy / Merge / IsMeetAsRightAdjoint (Δ ⊣ ∧)
import :f_algebra;  // cata: the ONE catamorphism engine (#961).  A lattice is a
                    // (thin) category and its term reducer is a fold, so the
                    // value-first reducer below is an ALGEBRA for cata, not a
                    // parallel fold (specific → general).

namespace dedekind::category {

/** @brief The logic species a comparator reports in: @c Boole (a
 *  @c bool decision) unless the comparator names its own @c logic typedef (e.g.
 *  @c Kleene, so an @b undecidable comparison can be @c Unknown). */
export template <typename Less>
struct lattice_less_logic {
  using type = Boole;
};
export template <typename Less>
  requires requires { typename Less::logic; }
struct lattice_less_logic<Less> {
  using type = typename Less::logic;
};
export template <typename Less>
using lattice_less_logic_t = typename lattice_less_logic<Less>::type;

/** @concept IsLatticeLess
 *  @brief The injected total order: a caller-supplied comparator with a
 *  @c static @c consteval @c less<X,Y>() returning a logic value in its
 *  @c lattice_less_logic (default @c Boole, i.e. @c bool).  The
 *  reported logic must be a full @c IsOckhamAlgebra, so @c ::True (accessed
 *  by @c lattice_definitely_less) is guaranteed present.  The reducer
 *  canonicalises only on a @b definitely-True result, so an @c Unknown
 *  (undecidable) comparison leaves the operands in authoring order.  Total-ness
 *  is the caller's obligation (Jlt: asserted downstream), not checked here. */
export template <typename Less, typename X, typename Y>
concept IsLatticeLess =
    IsOckhamAlgebra<lattice_less_logic_t<Less>> && requires {
      {
        Less::template less<X, Y>()
      } -> std::convertible_to<typename lattice_less_logic_t<Less>::Ω>;
      // Constant-evaluability gate on the EXACT operation the helper runs
      // (`less() == True`): a non-constexpr comparator fails the concept, so
      // `lattice_definitely_less` takes the fail-closed `else` rather than
      // hard-erroring inside its consteval body.
      typename std::bool_constant<(Less::template less<X, Y>() ==
                                   lattice_less_logic_t<Less>::True)>;
    };

/** @brief Did the comparator decide @b definitely @c True? (`Unknown`/`False`
 *  ⟹ don't reorder: keep authoring order.) */
export template <typename Less, typename X, typename Y>
consteval bool lattice_definitely_less() {
  if constexpr (IsLatticeLess<Less, X, Y>) {
    return Less::template less<X, Y>() == lattice_less_logic_t<Less>::True;
  } else {
    return false;  // no order for this pair ⟹ leave as authored
  }
}

/** @brief The default leaf-combiner performs no domain combination.  Two
 *  order-incomparable leaves stay a @c Meet / @c Join node.  A downstream
 *  carrier (e.g.\ @c sets) injects a @c Combine that computes the actual domain
 *  meet / join of two compatible leaves.  @c structured_and of two halfspaces
 *  yields an @c Interval, say.  @c Combine returns @c law_inactive when no
 *  domain combination applies.  This injected policy lets the reducer's
 *  incomparable-leaf residual fall through to the carrier's own `∧`/`∨`. */
export struct no_leaf_combine {};

// The reduction laws assembled here are order-theoretic: the meet IS the glb,
// the right adjoint of the diagonal (Δ ⊣ ∧, epic #946).  Pin that reification
// as a compile-time check against the reducer's canonical carrier --- @c bool,
// the two-element Boolean lattice where every induced law fires (see
// @ref lattice_term__Overview).  The injected glb is the Boolean @c AND (the
// meet under the chain @c false ≤ @c true) and @c Copy is the diagonal
// @c bool → @c bool×bool.  This edge makes @c :cartesian_bicategory
// load-bearing: the reducer's meet path type-checks against the category-level
// glb theory rather than importing an orphan partition.
// This is the reducer EDGE type-check, so it stays next to the reducer: the
// carrier-generic coherence witnesses (int / Ternary chains, the Sup
// rejection, and the meet-trichotomy IsProduct bridge) live with the concept in
// :cartesian_bicategory (#946).
static_assert(
    IsMeetAsRightAdjoint<Copy<bool>, Merge<bool, std::logical_and<bool>>>,
    "the reducer's meet path must be the right adjoint of the diagonal "
    "(Δ ⊣ ∧) over the Boolean-lattice witness.");

/** @brief Whether a law fired: its result is not @c law_inactive.  @tparam R
 *  the law's result type. */
template <typename R>
concept Fires = !std::same_as<std::remove_cvref_t<R>, law_inactive>;

/** @brief The lattice-law reducer as the algebra of @c cata (@c :f_algebra,
 *  the one fold).  A leaf reduces to itself.  A @c Meet / @c Join assembles the
 *  induced laws over its two reduced operand @b values, most-collapsing first
 *  (bounded ▸ idempotent ▸ complement ▸ absorption ▸ distributivity ▸ glb/lub ▸
 *  the injected leaf-combine), a law returning @c law_inactive ceding to the
 *  next; a rewrite (distributivity, the leaf-combine, De Morgan) is folded
 *  again.  @c Less canonicalises the commutative residue.  Every decision a law
 *  makes on the operand @b types is made inside @c decltype as well, so the
 *  normal form's type is @c reduce_t below and its value is this fold at run
 *  time: one law, two binding times.
 *  @tparam Less the injected total order (canonicalisation).  @tparam Ord the
 *  semantic lattice order (boundedness, glb, distributivity).  @tparam Combine
 *  the carrier's domain meet / join of two leaves, @c meet_value / @c
 *  join_value, each present only where the carrier combines that pair. */
/** @brief The injected leaf-combine has a domain meet (join) for this pair
 *  of leaves: @c C::meet_value(a, b) (@c join_value) is formed.
 *  @tparam C the combine.  @tparam A, B the leaves. */
template <typename C, typename A, typename B>
concept CombinesMeet =
    requires(const A& a, const B& b) { C::meet_value(a, b); };
template <typename C, typename A, typename B>
concept CombinesJoin =
    requires(const A& a, const B& b) { C::join_value(a, b); };

export template <typename Less, typename Ord = canonical_order,
                 typename Combine = no_leaf_combine>
struct Reduce {
  template <typename Leaf>
  constexpr Leaf operator()(const Leaf& leaf) const {
    return leaf;
  }
  template <typename A, typename B, typename RA, typename RB>
  constexpr auto operator()(const Meet<A, B>&, const RA& a, const RB& b) const {
    if constexpr (Fires<decltype(meet_bounded_law<Ord>(a, b))>) {
      return meet_bounded_law<Ord>(a, b);
    } else if constexpr (Fires<decltype(idempotent_law(a, b))>) {
      return idempotent_law(a, b);
    } else if constexpr (Fires<decltype(meet_complement_law<Ord>(a, b))>) {
      return meet_complement_law<Ord>(a, b);  // a ∧ ¬a = ⊥
    } else if constexpr (Fires<decltype(meet_structural_absorption_law(a,
                                                                       b))>) {
      return meet_structural_absorption_law(a, b);  // a ∧ (a ∨ b) = a
    } else if constexpr (Fires<decltype(meet_distributivity_law<Ord>(a, b))>) {
      // Distributed to a join of meets: fold again toward DNF.  This terminates
      // in one direction only.
      return cata(*this, meet_distributivity_law<Ord>(a, b));
    } else if constexpr (Fires<decltype(meet_glb_law<Ord>(a, b))>) {
      return meet_glb_law<Ord>(a, b);
    } else if constexpr (CombinesMeet<Combine, RA, RB>) {
      // The order-incomparable residual: the carrier's own domain meet on the
      // two leaves' data (two halfspaces whose pivots decide empty / point /
      // interval), folded again.
      return cata(*this, Combine::meet_value(a, b));
    } else if constexpr (lattice_definitely_less<Less, RB, RA>()) {
      return Meet<RB, RA>{b, a};  // canonicalise
    } else {
      return Meet<RA, RB>{a, b};  // Unknown ⟹ keep
    }
  }
  /** @brief The exact dual for join (⊤ annihilates, ⊥ is the unit; join =
   *  lub). */
  template <typename A, typename B, typename RA, typename RB>
  constexpr auto operator()(const Join<A, B>&, const RA& a, const RB& b) const {
    if constexpr (Fires<decltype(join_bounded_law<Ord>(a, b))>) {
      return join_bounded_law<Ord>(a, b);
    } else if constexpr (Fires<decltype(idempotent_law(a, b))>) {
      return idempotent_law(a, b);
    } else if constexpr (Fires<decltype(join_complement_law<Ord>(a, b))>) {
      return join_complement_law<Ord>(a, b);  // a ∨ ¬a = ⊤
    } else if constexpr (Fires<decltype(join_structural_absorption_law(a,
                                                                       b))>) {
      return join_structural_absorption_law(a, b);  // a ∨ (a ∧ b) = a
    } else if constexpr (Fires<decltype(join_lub_law<Ord>(a, b))>) {
      return join_lub_law<Ord>(a, b);
    } else if constexpr (CombinesJoin<Combine, RA, RB>) {
      return cata(*this, Combine::join_value(a, b));
    } else if constexpr (lattice_definitely_less<Less, RB, RA>()) {
      return Join<RB, RA>{b, a};
    } else {
      return Join<RA, RB>{a, b};
    }
  }
  /** @brief ¬(reduced): the De Morgan negation law (involution @c ¬¬A→A, De
   *  Morgan), gated on an involutive De Morgan negation, pushes @c ¬ toward the
   *  leaves and the result is folded again (termination: @c ¬ descends);
   *  otherwise @c ¬(reduced) is already negation-normal and stays. */
  template <typename A, typename RA>
  constexpr auto operator()(const Not<A>&, const RA& a) const {
    if constexpr (Fires<decltype(de_morgan_law<Ord>(a))>) {
      return cata(*this, de_morgan_law<Ord>(a));
    } else {
      return Not<RA>{a};
    }
  }
};

/** @brief The normal form of @c Term under @c Reduce<Less, Ord, Combine>: the
 *  type the fold produces, read without running it. */
export template <typename Term, typename Less, typename Ord = canonical_order,
                 typename Combine = no_leaf_combine>
using reduce_t =
    decltype(cata(std::declval<const Reduce<Less, Ord, Combine>&>(),
                  std::declval<const Term&>()));

}  // namespace dedekind::category
