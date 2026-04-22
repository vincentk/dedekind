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

using C1 = ExtensionalCardinal<>;

static_assert(dedekind::numbers::IsNatural<C1>,
              "ExtensionalCardinal<1> must satisfy IsNatural.");

static_assert(dedekind::order::IsTotallyOrdered<C1>,
              "ExtensionalCardinal<1> must satisfy IsTotallyOrdered.");

// IsInteger<C1> is verified in rational.cppm (which imports both
// :cardinality and :integer) to avoid a circular import chain.

}  // namespace dedekind::numbers
