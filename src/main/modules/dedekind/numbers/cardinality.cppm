/**
 * @file dedekind/numbers/cardinality.cppm
 * @partition :cardinality
 * @brief Re-export of dedekind.sets:cardinality with numbers-level proofs.
 *
 * @details
 * The extensional cardinality carrier `ExtensionalCardinal<N>` is defined in
 * `dedekind.sets:cardinality` (below the `numbers` and `algebra` layers),
 * making it available for use in polynomial ring assertions in `algebra`.
 *
 * This partition re-exports the set-level cardinality types and adds the
 * two assertions that require `dedekind.order` and `dedekind.numbers:naturals`:
 *   - `IsTotallyOrdered<ExtensionalCardinal<>>` (needs order)
 *   - `IsNatural<ExtensionalCardinal<>>` (needs numbers:naturals)
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
import :naturals;

namespace dedekind::numbers {

using C1 = dedekind::sets::ExtensionalCardinal<>;
using Z1 = dedekind::sets::SignedExtensionalCardinal<>;

static_assert(dedekind::numbers::IsNatural<C1>,
              "ExtensionalCardinal<1> must satisfy IsNatural.");

// Order witnesses: both bounded ℕ (C1) and bounded ℤ (Z1) carry the
// total order inherited from their bit-pattern representation, with
// the trait specialisations provided in `sets:cardinality`.  Asserted
// here (in `numbers`) because `sets` is upstream of `order` and
// cannot import it.
static_assert(dedekind::order::IsPreOrdered<C1>,
              "ExtensionalCardinal<1> with <= is a pre-order.");
static_assert(dedekind::order::IsPartiallyOrdered<C1>,
              "ExtensionalCardinal<1> with <= is a partial order.");
static_assert(dedekind::order::IsTotallyOrdered<C1>,
              "ExtensionalCardinal<1> must satisfy IsTotallyOrdered.");

static_assert(dedekind::order::IsPreOrdered<Z1>,
              "SignedExtensionalCardinal<1> with <= is a pre-order.");
static_assert(dedekind::order::IsPartiallyOrdered<Z1>,
              "SignedExtensionalCardinal<1> with <= is a partial order.");
static_assert(dedekind::order::IsTotallyOrdered<Z1>,
              "SignedExtensionalCardinal<1> with <= is totally ordered.");

// Cardinal types are valid \emph{net domains}: ℕ-bounded and ℤ-bounded
// carriers are directed sets (the join-semilattice structure under
// max is induced by the total order).  Per Munkres / Kelley, this is
// the textbook condition for a function defined on the carrier to be
// a net.  Both bool (in `algebra:boolean`) and cardinal types
// (here) are witnessed as candidate net domains.
static_assert(dedekind::order::IsDirectedSet<C1>,
              "ExtensionalCardinal<1> is a directed set --- a valid "
              "net domain (the bounded ℕ case).");
static_assert(dedekind::order::IsDirectedSet<Z1>,
              "SignedExtensionalCardinal<1> is a directed set "
              "(the bounded ℤ case).");

// IsInteger<C1> is verified in rational.cppm (which imports both
// :cardinality and :integer) to avoid a circular import chain.

// Variant ℕ-proxy @c Cardinality order witnesses (#424).  The
// homogeneous operators (@c +, @c *, @c <=>) and trait specialisations
// landed in @c sets/cardinality.cppm; the order-side witnesses live
// here because @c IsPreOrdered / @c IsPartiallyOrdered / @c
// IsTotallyOrdered / @c HasPartialOrderOperators / @c
// HasTotalOrderOperators / @c IsDirectedSet are concepts in @c
// dedekind.order, which is downstream of @c sets.
//
// These are the witnesses that the upcoming @c using @c ℕ @c = @c
// Cardinality flip (#402) needs in place to keep the load-bearing
// claims from PR #409 firing.
static_assert(
    dedekind::order::HasPartialOrderOperators<dedekind::sets::Cardinality>,
    "Cardinality carries the partial-order operator surface "
    "(< / <= / > / >=) post-#424.");
static_assert(
    dedekind::order::HasTotalOrderOperators<dedekind::sets::Cardinality>,
    "Cardinality carries the total-order operator surface "
    "(spaceship + four partial-order ops).");
static_assert(dedekind::order::IsPreOrdered<dedekind::sets::Cardinality>,
              "Cardinality with <= is a pre-order.");
static_assert(dedekind::order::IsPartiallyOrdered<dedekind::sets::Cardinality>,
              "Cardinality with <= is a partial order.");
static_assert(dedekind::order::IsTotallyOrdered<dedekind::sets::Cardinality>,
              "Cardinality is axiomatically totally ordered (the chain "
              "axiom is satisfied: every pair is comparable, since the "
              "finite fragment inherits from ExtensionalCardinal<>'s "
              "strict total order and ℵ_0 dominates every finite value).");
static_assert(dedekind::order::IsDirectedSet<dedekind::sets::Cardinality>,
              "Cardinality is a directed set — a valid net domain "
              "(the unbounded-ℕ-with-saturation case).");
static_assert(dedekind::order::IsDirectedPoset<dedekind::sets::Cardinality>,
              "Cardinality is a directed poset (every pair has a join, "
              "and the order is antisymmetric).");

// Heterogeneous partial-order shape: the variant ℕ-/ℤ-proxy carriers
// admit cross-type relational comparison with built-in @c std::integral
// values via the operators defined in @c sets/cardinality.cppm
// (closes #415).  Pinning the shape concept here --- not in @c sets ---
// because the @c HasPartialOrderOperatorsWith concept itself lives in
// @c dedekind.order:poset, which is downstream of @c sets.
//
// The load-bearing pair is @c (SignedCardinality, int) --- the halfspace
// machinery's @c (x @c > @c Pivot) substitution where @c x is the
// variant ℤ-proxy and @c Pivot is the @c int NTTP from @c bound<-21>.
// @c HasPartialOrderOperatorsWith is symmetric in @c T and @c U
// (mirrors @c std::three_way_comparable_with), so a single witness per
// pair pins both directions of the relational surface.

using SC = dedekind::sets::SignedCardinality;
using Card = dedekind::sets::Cardinality;

static_assert(dedekind::order::HasPartialOrderOperatorsWith<SC, int>,
              "SignedCardinality must accept partial-order comparison "
              "with int (the halfspace bound<-21> NTTP path).");
static_assert(dedekind::order::HasPartialOrderOperatorsWith<SC, long>,
              "SignedCardinality must accept partial-order comparison "
              "with long (cross-width signed integral).");

static_assert(dedekind::order::HasPartialOrderOperatorsWith<Card, unsigned int>,
              "Cardinality must accept partial-order comparison with "
              "unsigned int (the ℕ-proxy slot).");
static_assert(dedekind::order::HasPartialOrderOperatorsWith<Card, int>,
              "Cardinality vs signed int: negative signed values land "
              "strictly below the ℕ proxy (no element is negative).");
static_assert(dedekind::order::HasPartialOrderOperatorsWith<Card, bool>,
              "Cardinality vs bool: bool is std::integral so the "
              "ctor / comparison path must handle it (regression for "
              "the make_unsigned_t<bool>-undefined trap).");

}  // namespace dedekind::numbers
