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

}  // namespace dedekind::numbers
