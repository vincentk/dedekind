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
 * @concept HasLatticeOperators
 * @brief @b Pure @b syntactic @b shape: T supports the bitwise / lattice
 *        operators @c &, @c |, @c ^, @c ~ with results convertible to T.
 *
 * @details
 * Use this concept where the callsite needs the lattice operators
 * @c {meet, join, symmetric-difference, complement} to compile and
 * yield a value coercible back into @c T --- e.g.\ Boolean carriers,
 * integer bit-twiddle types, sublattices of a Heyting algebra, the
 * additive @f$\mathbb{F}_2@f$ surface.  No axiomatic claim about
 * idempotency, absorption, distributivity, or De Morgan duality is
 * made here; for those, use the certified lattice concepts below or
 * the bundled @c IsOrderLattice further down.
 *
 * The return-type is @c convertible_to<T> rather than @c same_as<T>
 * (cf.\ the strict @c HasRingOperators) because integer promotion
 * makes @c bool & bool @c -> int in C++; relaxing to convertibility
 * lets the canonical Boolean carrier (@c bool) satisfy the concept
 * via promotion-and-back.
 *
 * Sibling of @c dedekind::algebra::HasRingOperators (in @c
 * algebra:ring) and @c dedekind::category::HasLogicalOperators (in
 * @c category:logic) in the shape-concept family --- introduced
 * under #393.
 */
export template <typename T>
concept HasLatticeOperators = requires(T a, T b) {
  { a & b } -> std::convertible_to<T>;
  { a | b } -> std::convertible_to<T>;
  { a ^ b } -> std::convertible_to<T>;
  { ~a } -> std::convertible_to<T>;
};

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
 * @brief @b Bundled @b lattice @b witness: the strict commutative-ring
 *        proof under @c (Add, Mult) AND the bitwise/lattice operator
 *        surface on @c T, by definition.
 *
 * @details
 * The Boolean-ring reading of "lattice": a commutative ring where
 * @c Add plays the role of symmetric difference (join modulo
 * absorption) and @c Mult plays the role of meet.  Defaults
 * @c std::bit_xor<T> / @c std::bit_and<T> match the bitwise operators
 * the @c HasLatticeOperators shape concept describes; the bundle is
 * the conjunction of "the type system has the categorical proof" and
 * "the type system has the operators", in one query.
 *
 * @b Meaning @b shift @b (#393): this concept previously re-exported
 * @c category::IsCertifiedOrderLatticeOperations<T, Join, Meet> with
 * @c std::ranges::min / @c std::ranges::max as the order-theoretic
 * Join/Meet defaults.  The order-theoretic certified lattice machinery
 * still lives at @c category::IsCertifiedOrderLatticeOperations and
 * is reachable directly; @c IsOrderLattice now bundles the
 * Boolean-ring lattice reading instead --- which is the canonical
 * @c HasLatticeOperators shape↔concept mapping the audit recorded.
 * The Boolean ring and Boolean lattice are equivalent categories, so
 * either reading recovers the other on Boolean-flavoured carriers.
 */
export template <typename T, typename Add = std::bit_xor<T>,
                 typename Mult = std::bit_and<T>>
concept IsOrderLattice =
    dedekind::category::IsCommutativeRing<T, Add, Mult> && HasLatticeOperators<T>;

/**
 * @concept IsOrderDistributiveLattice
 * @brief Re-export the certified distributive lattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderDistributiveLattice =
    dedekind::category::IsCertifiedOrderDistributiveLatticeOperations<T, Join,
                                                                      Meet>;

/** @section Formal_Verification */

// Pure-syntactic-shape witnesses: integer bit-twiddle types and
// bool itself satisfy the four lattice operators with results
// convertible back to the carrier (bool's & under integer promotion
// returns int but is convertible to bool, hence the deliberately
// loose return-type spec).
static_assert(HasLatticeOperators<unsigned int>,
              "unsigned int has the syntactic lattice-operator surface "
              "(bitwise &, |, ^, ~).");
static_assert(HasLatticeOperators<bool>,
              "bool has the syntactic lattice-operator surface "
              "(bitwise operators yield int via promotion, "
              "convertible back to bool).");

// IsOrderLattice (bundled, Boolean-ring reading) on bool: bool
// satisfies IsCommutativeRing<bool, std::bit_xor, std::bit_and> per
// the Galois-field witness shipped in #378, and now also satisfies
// HasLatticeOperators<bool>; the bundled IsOrderLattice<bool> fires.
static_assert(IsOrderLattice<bool>,
              "bool is the canonical Boolean-ring lattice "
              "(under bit_xor / bit_and; both halves of the bundle "
              "fire today).");

}  // namespace dedekind::order
