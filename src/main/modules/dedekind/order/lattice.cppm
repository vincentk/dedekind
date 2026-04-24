/**
 * @file dedekind/order/lattice.cppm
 * @partition :lattice
 * @brief Level 1.5: Order-theoretic meet / join / lattice structures.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Scope
 * Species carrying binary meet and/or join operations that respect
 * the underlying order, reaching up to distributive lattices.  The
 * actual axiomatic certification (associativity, commutativity,
 * idempotence, absorption, distributivity) lives in
 * `dedekind.category:posetal` as
 * `IsCertifiedOrder{Meet,Join}Semilattice` etc.; this partition
 * re-exports those witnesses under the order-theoretic names a
 * mathematician expects to find alongside posets and chains.
 *
 * Wikipedia: Semilattice, Lattice (order), Distributive lattice.
 */
module;
#include <algorithm>
#include <functional>

export module dedekind.order:lattice;

import dedekind.category;

namespace dedekind::order {
using namespace dedekind::category;

/**
 * @concept IsOrderMeetSemilattice
 * @brief Re-export the certified meet-semilattice stage from `:posetal`.
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsOrderMeetSemilattice =
    dedekind::category::IsCertifiedOrderMeetSemilattice<T, Meet>;

/**
 * @concept IsOrderJoinSemilattice
 * @brief Re-export the certified join-semilattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsOrderJoinSemilattice =
    dedekind::category::IsCertifiedOrderJoinSemilattice<T, Join>;

/**
 * @concept IsOrderLattice
 * @brief Re-export the certified lattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderLattice =
    dedekind::category::IsCertifiedOrderLatticeOperations<T, Join, Meet>;

/**
 * @concept IsOrderDistributiveLattice
 * @brief Re-export the certified distributive lattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderDistributiveLattice =
    dedekind::category::IsCertifiedOrderDistributiveLatticeOperations<T, Join,
                                                                      Meet>;

}  // namespace dedekind::order
