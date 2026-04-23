/**
 * @file dedekind/order/order.cppm
 * @brief Umbrella module for Level 1.5 — order relations and compile-time
 *        halfspace predicates.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Partitions
 *   - `:concepts`   — pre/total/partial orders, lattices, Archimedean,
 *                     Dedekind-complete.
 *   - `:halfspace`  — NTTP halfspace DSL (`bound<V>`, `Halfspace`,
 *                     `OrderInterval`, `Singleton`, `IntervalProduct`,
 *                     `structured_and`).
 *
 * Wikipedia: Partially ordered set, Total order, Half-space (geometry)
 *
 * @note "Mathematics is not the study of abstract objects; it is the
 *       study of clarity of thought."
 *       — William P. Thurston, *On proof and progress in mathematics*,
 *         Bulletin of the AMS 30, 2 (1994), §1.
 */
export module dedekind.order;

export import :concepts;
export import :halfspace;
