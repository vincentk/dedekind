/**
 * @file optimization.cppm
 * @brief Mathematical programming — optimum as a typed constant.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section optimization__Description
 * This module hosts compile-time problem classes from mathematical
 * programming: linear programming (`:lp`), later quadratic programming
 * (`:qp`) and beyond. The umbrella claim: for problem instances whose
 * structure can be named at the type level — objective, constraints,
 * active sets — the optimum is reducible to a typed constant (`Singleton`
 * of the witnessing vertex / point) by the compiler, without runtime
 * iteration.
 *
 * This realises the paper's thesis in a sharpened form: the feasible
 * polytope is a set-as-predicate, its finite vertex set is enumerated
 * structurally, and the objective's ordering collapses the candidate set
 * to a singleton optimum.  "Rules on Buckets are Set" specialised to
 * convex programming.
 *
 * @note "Modellbildung ist die halbe Miete."
 *       (Modelling is half the battle.)
 *       — Operations-research folk wisdom.
 *
 * @section optimization__Roadmap Roadmap (work in progress)
 * @li <a href="https://github.com/vincentk/dedekind/issues/319">#319
 *     undergraduate graph theory</a>: the semiring closure and the critical
 *     path over a DAG; still open are Dijkstra and Bellman-Ford on general
 *     graphs, spanning trees, flows and matchings.
 * @li <a href="https://github.com/vincentk/dedekind/issues/330">#330
 *     undergraduate algorithms and data structures</a>: dynamic programming
 *     as an algebraic path problem; still open are optimal substructure and
 *     memoisation as concepts, and topological sort.
 */
export module dedekind.optimization;

export import :closure;  // semiring_closure — reachability / critical path
export import :lp;
