/**
 * @file dedekind/optimization/closure.cppm
 * @partition :closure
 * @brief Single-source graph closure over a semiring, point-free: a fold over
 *        the edge sequence producing a potential @b net; the critical path is
 *        that net's induced @c pred function, iterated.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section closure__Overview
 * A directed graph is a relation on nodes (@c IsDirectedGraph).  Presented
 * extensionally its edges are a @c FiniteSeq, and the closure threads a
 * potential @c FiniteNet @c d : V → S over it by a fold, @c d(v) ← d(v) ⊕ d(u)
 * ⊗ c. The choice of semiring is the choice of problem: @c bool @c (∨,∧) gives
 * reachability, @c MaxPlus @c (max,+) the critical path.  Over a dioid
 * (@c IsTropical) the closure also collapses the relation to a @b function
 * @c pred : V → V --- the critical-path tree --- which @ref critical_path
 * iterates back from the sink.  The O(V) memo lives inside @c FiniteNet and
 * @c FiniteSeq (the compile-time @c :sequences realizations), not on display.
 */
module;

#include <concepts>  // std::convertible_to / std::invocable (functor gates)
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
 * closure structure --- which semiring @c Add / @c Mult and which @c cost the
 * fold threads --- is an inspectable type rather than a nameless closure.
 * @c cost is the only captured state; @c Add / @c Mult default to the carrier's
 * canonical operations, as in @ref semiring_closure.  Models the @c fold op
 * shape @c op(acc&,Edge).
 */
export template <
    typename S, std::size_t Cap,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult,
    typename CostFn = S (*)(std::size_t, std::size_t)>
  requires requires(const CostFn& cost, std::size_t u, S s) {
    { cost(u, u) } -> std::convertible_to<S>;
    { Add{}(s, s) } -> std::convertible_to<S>;
    { Mult{}(s, s) } -> std::convertible_to<S>;
  }
struct Relax {
  CostFn cost;
  constexpr void operator()(FiniteNet<S, Cap>& acc, const Edge& e) const {
    acc.at(e.head) =
        Add{}(acc(e.head), Mult{}(acc(e.tail), cost(e.tail, e.head)));
  }
};

/**
 * @brief Single-source semiring closure: fold the edge sequence into the
 *        potential net, return its value at the sink.  @c Add / @c Mult
 *        default to the carrier's canonical operations.
 */
export template <
    typename S, std::size_t Cap,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult,
    typename Edges, typename CostFn>
  requires dedekind::category::IsSemiring<S, Add, Mult>
constexpr S semiring_closure(std::size_t source, std::size_t sink,
                             const Edges& edges, CostFn cost) {
  static_assert(S{} == dedekind::category::identity_v<S, Add>);  // 0-bar is S{}
  FiniteNet<S, Cap> d{};                                   // 0-bar everywhere
  d.at(source) = dedekind::category::identity_v<S, Mult>;  // 1-bar at source
  return dedekind::sequences::fold(
      edges, d, Relax<S, Cap, Add, Mult, CostFn>{cost})(sink);
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
  FiniteNet<S, Cap> d{};               // potentials (transient memo)
  FiniteNet<std::size_t, Cap> pred{};  // the V → V critical map
};

/**
 * @brief Fold step of @ref annotate: relax through @c e and, when the candidate
 *        wins the (selective) join, record @c e.tail as @c e.head's
 *        predecessor.
 *
 * @details The named replacement for the capturing @c step lambda (#920).  The
 * selective @c ⊕ test @c (d(head) ⊕ cand != d(head)) is what makes the recorded
 * @c pred single-valued; see @ref annotate for the gate.  @c cost is the only
 * captured state.  Models the @c fold op shape @c op(acc&,Edge).
 */
export template <
    typename S, std::size_t Cap,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult,
    typename CostFn = S (*)(std::size_t, std::size_t)>
  requires requires(const CostFn& cost, std::size_t u, S s) {
    { cost(u, u) } -> std::convertible_to<S>;
    { Add{}(s, s) } -> std::convertible_to<S>;
    { Mult{}(s, s) } -> std::convertible_to<S>;
    { s != s } -> std::convertible_to<bool>;
  }
struct CriticalPathStep {
  CostFn cost;
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
export template <
    typename S, std::size_t Cap,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult,
    typename Edges, typename CostFn>
  requires dedekind::algebra::IsTropical<S, Add, Mult>
constexpr FiniteNet<std::size_t, Cap> annotate(std::size_t source,
                                               const Edges& edges,
                                               CostFn cost) {
  static_assert(S{} == dedekind::category::identity_v<S, Add>);
  CriticalPathState<S, Cap> a{};
  a.d.at(source) = dedekind::category::identity_v<S, Mult>;  // 1-bar
  return dedekind::sequences::fold(
             edges, a, CriticalPathStep<S, Cap, Add, Mult, CostFn>{cost})
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

}  // namespace dedekind::optimization
