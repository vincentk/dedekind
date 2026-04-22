/**
 * @file dedekind/order/order.cppm
 * @brief Umbrella module for Level 1.5 — order relations and compile-time
 *        halfspace predicates.
 *
 * @partitions
 *   :concepts   — pre/total/partial orders, lattices, Archimedean, Dedekind
 *   :halfspace  — NTTP halfspace DSL (`bound<V>`, `Halfspace`, structured_and)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
export module dedekind.order;

export import :concepts;
export import :halfspace;
