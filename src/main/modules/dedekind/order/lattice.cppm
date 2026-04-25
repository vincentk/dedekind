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

/**
 * @concept HasLatticeOperators
 * @brief @b Pure @b syntactic @b shape: T supports the bitwise / lattice
 *        operators @c &, @c |, @c ^, @c ~ with closed results.
 *
 * @details
 * Use this concept where the callsite needs the lattice operators
 * @c {meet, join, symmetric-difference, complement} to compile and
 * close back into @c T --- e.g.\ Boolean carriers, integral
 * bit-twiddle types, sublattices of a Heyting algebra, the additive
 * @f$\mathbb{F}_2@f$ surface.  No axiomatic claim about idempotency,
 * absorption, distributivity, or De Morgan duality is made here; for
 * those, use the certified lattice concepts above.  Sibling of
 * @c dedekind::algebra::HasRingOperators (in @c algebra:ring) and
 * @c dedekind::category::HasLogicalOperators (in @c category:logic)
 * in the shape-concept family --- introduced under #393.
 */
export template <typename T>
concept HasLatticeOperators = requires(T a, T b) {
  { a & b } -> std::same_as<T>;
  { a | b } -> std::same_as<T>;
  { a ^ b } -> std::same_as<T>;
  { ~a } -> std::same_as<T>;
};

/** @section Formal_Verification */

// Pure-syntactic-shape witness: integer bit-twiddle types have all
// four lattice operators closing on themselves.  Strict closure on T
// --- integral promotion to int means `bool & bool` is int rather
// than bool, so bool does NOT satisfy this concept as written;
// unsigned int is the canonical fit.
static_assert(HasLatticeOperators<unsigned int>,
              "unsigned int has the syntactic lattice-operator surface "
              "(bitwise &, |, ^, ~ all close to unsigned int).");

}  // namespace dedekind::order
