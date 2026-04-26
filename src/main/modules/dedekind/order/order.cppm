/**
 * @file dedekind/order/order.cppm
 * @module dedekind.order
 * @brief Umbrella module for Level 1.5 — order relations and compile-time
 *        halfspace predicates.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Partitions
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
 *                       `OrderInterval`, `Singleton`, `IntervalProduct`,
 *                       `structured_and`).
 *
 * Wikipedia: Partially ordered set, Total order, Half-space (geometry)
 *
 * @note "Mathematics is not the study of abstract objects; it is the
 *       study of clarity of thought."
 *       — William P. Thurston, *On proof and progress in mathematics*,
 *         Bulletin of the AMS 30, 2 (1994), §1.
 */
export module dedekind.order;

export import :poset;
export import :total;
export import :lattice;
export import :completeness;
export import :halfspace;
