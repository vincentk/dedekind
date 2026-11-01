/**
 * @file dedekind/order/order.cppm
 * @brief Order relations and compile-time halfspace predicates.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section order__Partitions
 *   - `:poset`        — preorders, partial orders, directed sets; the
 *                       homogeneous `HasPartialOrderOperators` shape and
 *                       its heterogeneous sibling
 *                       `HasPartialOrderOperatorsWith<T, U>`.
 *   - `:total`        — total orders, linear orders, strict weak orders,
 *                       chains; the homogeneous `HasTotalOrderOperators`
 *                       (spaceship) shape and its heterogeneous sibling
 *                       `HasTotalOrderOperatorsWith<T, U>`.  Strictly
 *                       stronger than `:poset`; mirrors `:category:total`'s
 *                       order/algebra split (per #410).
 *   - `:lattice`      — meet / join / lattice / distributive lattice.
 *   - `:completeness` — successor, Archimedean, dense / discrete,
 *                       Dedekind-complete.
 *   - `:halfspace`    — NTTP halfspace DSL (`bound<V>`, `Halfspace`,
 *                       `Interval`, `Singleton`,
 *                       `structured_and`).
 *
 * Wikipedia: Partially ordered set, Total order, Half-space (geometry)
 *
 * @note "Mathematics is not the study of abstract objects; it is the
 *       study of clarity of thought."
 *       — William P. Thurston, *On proof and progress in mathematics*,
 *         Bulletin of the AMS 30, 2 (1994), §1.
 *
 * @section order__Roadmap Roadmap (work in progress)
 * @li <a href="https://github.com/vincentk/dedekind/issues/308">#308
 *     undergraduate probability and statistics</a>: @c Sub holds the
 *     convex subobjects of an ordered carrier (intervals, not arbitrary
 *     subsets: no @c {0,2} in @c Sub<int>), so it is closed under meets but
 *     not under unions or complements; σ-algebras and measures are open.
 */
export module dedekind.order;

export import :poset;
export import :total;
export import :lattice;
export import :completeness;
export import :halfspace;
export import :runs;  // run lists: the normal form of sets built from intervals
export import :inclusion;
export import :powerset;
