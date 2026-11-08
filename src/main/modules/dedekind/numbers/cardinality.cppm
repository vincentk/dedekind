/**
 * @file dedekind/numbers/cardinality.cppm
 * @partition :cardinality
 * @brief Re-export of dedekind.sets:cardinality with numbers-level proofs.
 *
 * @details
 * The extensional cardinality carrier `ExtensionalCardinal<W>` is defined in
 * `dedekind.sets:cardinality` (below the `numbers` and `algebra` layers),
 * making it available for use in polynomial ring assertions in `algebra`.
 *
 * This partition re-exports the set-level cardinality types and adds the
 * two assertions that require `dedekind.order` and `dedekind.numbers:natural`:
 *   - `IsTotallyOrdered<ExtensionalCardinal<>>` (needs order)
 *   - `IsNatural<ExtensionalCardinal<>>` (needs numbers:natural)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Das Wesen der Mathematik liegt in ihrer Freiheit."
 *       ("The essence of mathematics lies entirely in its freedom.")
 *       -- Georg Cantor, Grundlagen einer allgemeinen
 *          Mannigfaltigkeitslehre (1883)
 */
export module dedekind.numbers:cardinality;

export import dedekind.sets;

import dedekind.order;
import :natural;

namespace dedekind::numbers {

using C1 = dedekind::sets::ExtensionalCardinal<>;
using Z1 = dedekind::sets::SignedExtensionalCardinal<>;

static_assert(dedekind::numbers::IsNatural<C1>,
              "ExtensionalCardinal<> must satisfy IsNatural.");

// Order witnesses: both bounded ℕ (C1) and bounded ℤ (Z1) carry the
// total order inherited from their bit-pattern representation, with
// the trait specialisations provided in `sets:cardinality`.  Asserted
// here (in `numbers`) because `sets` is upstream of `order` and
// cannot import it.
static_assert(dedekind::order::IsPreOrdered<C1>,
              "ExtensionalCardinal<> with <= is a pre-order.");
static_assert(dedekind::order::IsPartiallyOrdered<C1>,
              "ExtensionalCardinal<> with <= is a partial order.");
static_assert(dedekind::order::IsTotallyOrdered<C1>,
              "ExtensionalCardinal<> must satisfy IsTotallyOrdered.");

static_assert(dedekind::order::IsPreOrdered<Z1>,
              "SignedExtensionalCardinal<> with <= is a pre-order.");
static_assert(dedekind::order::IsPartiallyOrdered<Z1>,
              "SignedExtensionalCardinal<> with <= is a partial order.");
static_assert(dedekind::order::IsTotallyOrdered<Z1>,
              "SignedExtensionalCardinal<> with <= is totally ordered.");

// Cardinal types are valid \emph{net domains}: ℕ-bounded and ℤ-bounded
// carriers are directed sets (the join-semilattice structure under
// max is induced by the total order).  Per Munkres / Kelley, this is
// the textbook condition for a function defined on the carrier to be
// a net.  Both bool (in `algebra:boolean`) and cardinal types
// (here) are witnessed as candidate net domains.
static_assert(dedekind::order::IsDirectedSet<C1>,
              "ExtensionalCardinal<> is a directed set --- a valid "
              "net domain (the bounded ℕ case).");
static_assert(dedekind::order::IsDirectedSet<Z1>,
              "SignedExtensionalCardinal<> is a directed set "
              "(the bounded ℤ case).");

// IsInteger<C1> is verified in rational.cppm (which imports both
// :cardinality and :integer) to avoid a circular import chain.

// The ℤ proxy is a chain on the same footing: its order laws are registered in
// :sets under the posture that the non-finite alternatives (±ℵ_0, NaZ) are the
// out-of-memory boundary of the backing, not points of ℤ.
static_assert(dedekind::order::IsPreOrdered<dedekind::sets::SignedCardinality>,
              "SignedCardinality with <= is a pre-order.");
static_assert(
    dedekind::order::IsPartiallyOrdered<dedekind::sets::SignedCardinality>,
    "SignedCardinality with <= is a partial order.");
static_assert(
    dedekind::order::IsTotallyOrdered<dedekind::sets::SignedCardinality>,
    "SignedCardinality is a certified chain: −ℵ_0 < finite < +ℵ_0, NaZ being "
    "the indeterminate-form tripwire and no point of ℤ.");

// The proxy steps by ±1, so it is not dense: the guard is the NNO step, and
// it fires here although the carrier divides.
static_assert(!dedekind::order::IsDense<dedekind::sets::SignedCardinality>,
              "ℤ is discrete, not dense.");

// Heterogeneous partial-order shape: the variant ℤ-proxy carrier
// admits cross-type relational comparison with built-in @c std::integral
// values via the operators defined in @c sets/cardinality.cppm
// (closes #415).  Pinning the shape concept here --- not in @c sets ---
// because the @c HasPartialOrderOperatorsWith concept itself lives in
// @c dedekind.order:poset, which is downstream of @c sets.
//
// The load-bearing pair is @c (SignedCardinality, int) --- the halfspace
// machinery's @c (x @c > @c Pivot) substitution where @c x is the
// variant ℤ-proxy and @c Pivot is the @c int NTTP from @c -21.
// @c HasPartialOrderOperatorsWith is symmetric in @c T and @c U
// (mirrors @c std::three_way_comparable_with), so a single witness per
// pair pins both directions of the relational surface.

using SC = dedekind::sets::SignedCardinality;

static_assert(dedekind::order::HasPartialOrderOperatorsWith<SC, int>,
              "SignedCardinality must accept partial-order comparison "
              "with int (the halfspace -21 NTTP path).");
static_assert(dedekind::order::HasPartialOrderOperatorsWith<SC, long>,
              "SignedCardinality must accept partial-order comparison "
              "with long (cross-width signed integral).");

// Floating-point cross-type comparison (closes #428).  Load-bearing for
// the real-pivot showcases (e.g.\ @c var<ℤ> @c > @c -21.0 in
// showcase 07) where the bound's NTTP type is @c double and the carrier
// is the variant.  @c HasPartialOrderOperatorsWith is symmetric, so one
// witness per pair pins both directions.
static_assert(dedekind::order::HasPartialOrderOperatorsWith<SC, double>,
              "SignedCardinality must accept partial-order comparison "
              "with double (the real-pivot showcase path).");
static_assert(dedekind::order::HasPartialOrderOperatorsWith<SC, float>,
              "SignedCardinality must accept partial-order comparison "
              "with float (cross-width floating).");

}  // namespace dedekind::numbers
