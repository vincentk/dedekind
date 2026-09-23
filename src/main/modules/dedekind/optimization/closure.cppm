/**
 * @file dedekind/optimization/closure.cppm
 * @partition :closure
 * @brief Single-source graph closure over a semiring, as a fold over its edges.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section closure__Overview
 * A directed graph is a relation on nodes (@c IsDirectedGraph).  Presented
 * extensionally its edges are a @c FiniteSeq, and the closure threads a
 * potential @c FiniteNet @c d : V → S over it by a fold, @c d(v) ← d(v) ⊕ d(u)
 * ⊗ c. The choice of semiring is the choice of problem: @c bool @c (∨,∧) gives
 * reachability, @c MaxPlus @c (max,+) the critical path.  Over a @b selective
 * tropical dioid (an @c IsTropical @c ⊕ that returns one of its two operands,
 * e.g.\ @c MaxPlus) it also collapses the relation to a single-valued
 * @c pred : V → V --- the critical-path tree --- which @ref critical_path
 * iterates back from the sink.  Idempotence (@c IsTropical) alone is @b not
 * enough: a non-selective dioid needs a set-valued predecessor (see the
 * @ref annotate precondition, FIXME(#769)).  The O(V) memo lives inside
 * @c FiniteNet and @c FiniteSeq (the compile-time @c :sequences realizations),
 * not on display.
 *
 * Wikipedia: Shortest path problem, Semiring, Algebraic path problem
 *
 * @note "This 'shadow' stands approximately in the same relation to the
 *       traditional mathematics as does classical physics to quantum theory."
 *       --- G. L. Litvinov, "The Maslov Dequantization, Idempotent and Tropical
 *         Mathematics: a Brief Introduction", arXiv:math/0507014 (2005), §1.
 *       [Litvinov writes in English; the "shadow" is idempotent mathematics ---
 *       the tropical (min/max-plus) dioid this closure runs the critical path
 *       over, obtained from the ordinary field by Maslov dequantization.]
 */
module;

#include <concepts>  // std::convertible_to / std::invocable: the cost is a bare
                     // BINARY callable (edge → weight), a raw lambda with no
                     // Domain/Codomain, so it is gated structurally, not by
                     // IsArrow (which models a unary morphism); see below.
#include <cstddef>

export module dedekind.optimization:closure;

import dedekind.algebra;   // semiring_ops, IsTropical
import dedekind.category;  // identity_v, IsSemiring
import dedekind.sequences; // fold, Net, FiniteSeq

namespace dedekind::optimization {

using dedekind::sequences::FiniteNet;
using dedekind::sequences::FiniteSeq;

/** @brief A directed edge of the graph: @c tail → @c head (node indices). */
export struct Edge {
  std::size_t tail;
  std::size_t head;
};

/**
 * @brief Materialise an intensional graph rule extensionally: the edge
 *        sequence @c { (u,v) : edge(u,v) } over nodes @c 0 … Nodes−1, in
 *        topological (tail-ascending) order.  Where a rule becomes elements.
 */
export template <std::size_t Nodes, std::size_t Cap, typename Pred>
constexpr FiniteSeq<Edge, Cap> materialise(Pred edge) {
  FiniteSeq<Edge, Cap> es{};
  for (std::size_t u = 0; u < Nodes; ++u)
    for (std::size_t v = 0; v < Nodes; ++v)
      if (edge(u, v)) es.push({u, v});
  return es;
}

/**
 * @brief Fold step of the single-source semiring closure: relax the potential
 *        at @c e.head through the edge @c e, @c d(v) ← d(v) ⊕ d(u) ⊗ c(u,v).
 *
 * @details The named replacement for the capturing @c relax lambda (#920): the
 * closure structure --- which @c cost the fold threads --- is an inspectable
 * type rather than a nameless closure.  The semiring ops @c ⊕ / @c ⊗ are fixed
 * by the carrier @c S itself (@c semiring_ops<S>) rather than exposed as
 * parameters, so @c cost is the only captured state.  Models the @c fold op
 * shape @c op(acc&,Edge).
 *
 * @note The ops are the carrier's canonical semiring (the @c IsSemiring gate).
 * @c cost is a bare BINARY callable (edge → weight, e.g.\ the raw lambda the
 * necklace showcase passes) with no @c Domain / @c Codomain, so it is gated
 * structurally (a @c requires on @c cost), NOT by @c IsArrow, which models a
 * unary morphism and would reject every caller.
 */
export template <typename S, std::size_t Cap,
                 typename CostFn = S (*)(std::size_t, std::size_t)>
  requires dedekind::category::IsSemiring<
               S, typename dedekind::algebra::semiring_ops<S>::add,
               typename dedekind::algebra::semiring_ops<S>::mult> &&
           requires(const CostFn& cost, std::size_t u) {
             { cost(u, u) } -> std::convertible_to<S>;
           }
struct Relax {
  /** @brief The carrier's canonical additive op @c ⊕ (@c semiring_ops<S>). */
  using Add = typename dedekind::algebra::semiring_ops<S>::add;
  /** @brief The carrier's canonical multiplicative op @c ⊗
   *  (@c semiring_ops<S>). */
  using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
  /** @brief The edge-cost function @c c(tail,head); the only captured state. */
  CostFn cost;
  /** @brief Relax @c e.head in place: @c d(head) ← d(head) ⊕ d(tail) ⊗
   *  @c c(tail,head).  The @c fold op contract, @c op(acc&,Edge). */
  constexpr void operator()(FiniteNet<S, Cap>& acc, const Edge& e) const {
    acc.at(e.head) =
        Add{}(acc(e.head), Mult{}(acc(e.tail), cost(e.tail, e.head)));
  }
};

/**
 * @brief Single-source semiring closure: fold the edge sequence into the
 *        potential net, return its value at the sink.  The semiring ops are the
 *        carrier's canonical @c semiring_ops<S>.
 *
 * @note The semiring is the carrier's @b canonical @c semiring_ops<S>; the ops
 * are not parameters.  A carrier that certifies more than one semiring (e.g.
 * @c bool: @c (∨,∧) here, but also @c (⊕,∧)) uses only its canonical pair; a
 * @b different semiring on the same carrier is out of scope --- pick a distinct
 * carrier type for it, as the graph-closure use does (@c bool / @c MaxPlus /
 * @c MinPlus).
 *
 * @note This is a @b value-level algebraic reduction over a @b semiring.  The
 * gate is @c IsSemiring, so a non-idempotent semiring is admitted too; only the
 * idempotent (@c IsTropical) instantiations --- reachability, critical path ---
 * are dioids.  Its @c ⊗-over-@c ⊕ distributivity is the @b same law the
 * @b type-level term reducer (@c category:lattice_term, epic #890) normalises,
 * and a tropical @c ⊕ (min/max) is a semilattice op --- so the two share the
 * algebraic substrate, but live on opposite sides of the phase wall (value fold
 * here vs compile-time type rewrite there) and share no implementation today. A
 * @b value-first reducer (#922) could subsume this closure as a semiring star
 * (a dioid star in the idempotent case); the kinship is tracked in #926.
 */
export template <typename S, std::size_t Cap, typename Edges, typename CostFn>
  requires dedekind::category::IsSemiring<
      S, typename dedekind::algebra::semiring_ops<S>::add,
      typename dedekind::algebra::semiring_ops<S>::mult>
constexpr S semiring_closure(std::size_t source, std::size_t sink,
                             const Edges& edges, CostFn cost) {
  using Add = typename dedekind::algebra::semiring_ops<S>::add;
  using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
  static_assert(S{} == dedekind::category::identity_v<S, Add>);  // 0-bar is S{}
  FiniteNet<S, Cap> d{};                                   // 0-bar everywhere
  d.at(source) = dedekind::category::identity_v<S, Mult>;  // 1-bar at source
  return dedekind::sequences::fold(edges, d, Relax<S, Cap, CostFn>{cost})(sink);
}

/**
 * @brief Fold accumulator of @ref annotate: the transient potential net paired
 *        with the critical-path predecessor map it induces.
 *
 * @details Promoted from the function-local @c Anno struct so the fold step
 * (@ref CriticalPathStep) that threads it is a named, inspectable type (#920)
 * rather than a closure over an anonymous local.
 */
export template <typename S, std::size_t Cap>
struct CriticalPathState {
  /** @brief The transient potential net @c d : V → S (the working memo). */
  FiniteNet<S, Cap> d{};
  /** @brief The induced single-valued critical map @c pred : V → V. */
  FiniteNet<std::size_t, Cap> pred{};
};

/**
 * @brief Fold step of @ref annotate: relax through @c e and, when the candidate
 *        wins the (selective) join, record @c e.tail as @c e.head's
 *        predecessor.
 *
 * @details The named replacement for the capturing @c step lambda (#920).  The
 * selective @c ⊕ test @c (d(head) ⊕ cand != d(head)) is what makes the recorded
 * @c pred single-valued; see @ref annotate for the further selectivity
 * precondition.  The ops are fixed by the carrier @c S (@c semiring_ops<S>), so
 * @c cost is the only captured state.  Models the @c fold op shape
 * @c op(acc&,Edge).
 *
 * @note Gated on @c IsTropical (idempotent @c ⊕), NOT bare @c IsSemiring: the
 * update overwrites @c d(head) with @c cand alone when @c (d(head) ⊕ cand)
 * differs from @c d(head).  Storing @c cand recovers the true join only when
 * @c ⊕ is @b selective (@c a ⊕ b @c ∈ @c {a,b}): the changed result is then
 * @c cand itself.  Idempotence (@c a ⊕ a = a), the @c IsTropical gate, is
 * necessary but @b not sufficient --- a non-selective idempotent @c ⊕ (a
 * lattice join to a third value) would store @c cand and lose that value; and
 * an ordinary non-idempotent @c + would detect @c (old + cand != old) and then
 * WRONGLY store @c cand instead of the sum.  @c MaxPlus is selective; the gate
 * is only the tight-enough idempotence proxy pending a selectivity concept
 * (FIXME(#769), as @ref annotate states).
 *
 * @note As in @ref Relax, @c cost is a bare binary callable gated structurally,
 * not via @c IsArrow; the gate also requires the selective @c != that the
 * argmax test needs.
 */
export template <typename S, std::size_t Cap,
                 typename CostFn = S (*)(std::size_t, std::size_t)>
  requires dedekind::algebra::IsTropical<
               S, typename dedekind::algebra::semiring_ops<S>::add,
               typename dedekind::algebra::semiring_ops<S>::mult> &&
           requires(const CostFn& cost, std::size_t u, S s) {
             { cost(u, u) } -> std::convertible_to<S>;
             { s != s } -> std::convertible_to<bool>;
           }
struct CriticalPathStep {
  /** @brief The carrier's canonical additive op @c ⊕ (@c semiring_ops<S>). */
  using Add = typename dedekind::algebra::semiring_ops<S>::add;
  /** @brief The carrier's canonical multiplicative op @c ⊗
   *  (@c semiring_ops<S>). */
  using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
  /** @brief The edge-cost function @c c(tail,head); the only captured state. */
  CostFn cost;
  /** @brief Relax @c e.head and, when the candidate @c d(tail) ⊗ c(tail,head)
   *  wins the selective join, record @c e.tail as its predecessor.  The
   *  @c fold op contract, @c op(acc&,Edge). */
  constexpr void operator()(CriticalPathState<S, Cap>& acc,
                            const Edge& e) const {
    const S cand = Mult{}(acc.d(e.tail), cost(e.tail, e.head));
    if (Add{}(acc.d(e.head), cand) != acc.d(e.head)) {  // cand wins the join
      acc.d.at(e.head) = cand;
      acc.pred.at(e.head) = e.tail;
    }
  }
};

/**
 * @brief Collapse the edge relation to the critical-path FUNCTION @c pred :
 *        V → V (each node's predecessor on the longest path).  Gated on
 *        @c IsTropical (idempotent @c ⊕).  @b Precondition beyond the gate:
 *        @c ⊕ must be @b selective (its result is one of its two operands),
 *        which is what makes @c argmax --- and hence a single-valued @c pred
 *        --- well defined.  @c MaxPlus satisfies this; @c IsTropical alone does
 *        @b not guarantee it (a non-selective dioid such as powerset-union
 *        would yield a third value and need a set-valued @c pred).
 *        FIXME(#769): tighten the gate to a selectivity concept.
 */
export template <typename S, std::size_t Cap, typename Edges, typename CostFn>
  requires dedekind::algebra::IsTropical<
      S, typename dedekind::algebra::semiring_ops<S>::add,
      typename dedekind::algebra::semiring_ops<S>::mult>
constexpr FiniteNet<std::size_t, Cap> annotate(std::size_t source,
                                               const Edges& edges,
                                               CostFn cost) {
  using Add = typename dedekind::algebra::semiring_ops<S>::add;
  using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
  static_assert(S{} == dedekind::category::identity_v<S, Add>);
  CriticalPathState<S, Cap> a{};
  a.d.at(source) = dedekind::category::identity_v<S, Mult>;  // 1-bar
  return dedekind::sequences::fold(edges, a,
                                   CriticalPathStep<S, Cap, CostFn>{cost})
      .pred;
}

/**
 * @brief Iterate the @c pred net back from @c sink to @c source: the critical
 *        path as an edge sequence.  A functional graph --- one way between any
 *        two nodes --- ready to fold for cost or query for criticality.
 */
export template <std::size_t Cap>
constexpr FiniteSeq<Edge, Cap> critical_path(
    const FiniteNet<std::size_t, Cap>& pred, std::size_t source,
    std::size_t sink) {
  FiniteSeq<Edge, Cap> p{};
  for (std::size_t v = sink; v != source; v = pred(v)) p.push({pred(v), v});
  return p;
}

// Type-level witness (#920): the named fold steps model the @c fold op shape
// @c op(acc&,Edge), so @ref semiring_closure / @ref annotate thread them
// exactly as the @c relax / @c step lambdas they replaced.  Paired with the
// runtime necklace showcase (@c showcase_13_necklace_critical_path), whose @c
// witness_* functions and value @c static_asserts fold these very functors to
// concrete costs (@c static_asserts are invisible to coverage on their own).
static_assert(
    std::invocable<const Relax<bool, 4>&, FiniteNet<bool, 4>&, const Edge&>,
    "Relax is a fold op op(FiniteNet&, Edge).");
static_assert(std::invocable<const CriticalPathStep<bool, 4>&,
                             CriticalPathState<bool, 4>&, const Edge&>,
              "CriticalPathStep is a fold op op(CriticalPathState&, Edge).");

// The idempotent-⊕ gate on @ref CriticalPathStep is load-bearing, not
// decoration: its selective overwrite is valid only for an idempotent @c ⊕, so
// a @b certified non-idempotent semiring carrier is REJECTED at the gate, and
// the wrong-⊕ update is unreachable.  @c bool @c (∨,∧) is idempotent and
// admitted.  The negative witness uses @c unsigned @c int: its @c (+,×) are
// total, so it IS an @c IsSemiring, but @c + is not idempotent, so it is
// rejected by the @c IsTropical gate --- this pins the idempotence boundary.
// (Plain @c int would already fail @c IsSemiring on signed-overflow totality,
// so it would stay rejected even if the gate regressed to @c IsSemiring, and
// thus would NOT witness the idempotence requirement.)  (No runtime pair: this
// is a non-instantiation.)
template <typename S>
concept HasCriticalPathStep = requires { typename CriticalPathStep<S, 4>; };
static_assert(HasCriticalPathStep<bool>,
              "idempotent (∨,∧) carrier: CriticalPathStep is admitted.");
static_assert(!HasCriticalPathStep<unsigned int>,
              "certified non-idempotent semiring (unsigned (+,×)): "
              "CriticalPathStep is rejected by the IsTropical gate.");

}  // namespace dedekind::optimization
