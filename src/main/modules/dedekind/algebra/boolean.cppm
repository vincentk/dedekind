/**
 * @file boolean.cppm
 * @partition :boolean
 * @module dedekind.algebra:boolean
 * @brief Boolean Starter Package: canonical Boolean ambient species aliases.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Starter_Intent
 * This partition offers a small, explicit entry point for Boolean algebra in
 * the set-builder DSL. It exports canonical Boolean universe aliases so
 * examples remain readable and stable.
 *
 * @section Notation
 * - `𝔹`: canonical Unicode symbol for the Boolean ambient set.
 * - `B`: ASCII alias for environments where Unicode input is inconvenient.
 *
 * @section Paper_Alignment
 * In the paper's Feature Cube (bool row), logical (`||`, `&&`) and bitwise
 * (`|`, `&`) operators over bool share the same lattice behavior (join/meet,
 * identities, absorbers, and distributivity). The test suite validates this
 * alignment explicitly.
 *
 * Element scouts are intentionally local (e.g. `auto b = var<BooleanSet>;`) to
 * avoid global name shadowing in downstream code.
 *
 * @section Historical_Note
 * "La matematica non e una collezione di trucchi: e grammatica delle forme."
 * (Mathematics is not a bag of tricks; it is a grammar of forms.)
 * — Emma Castelnuovo
 *
 * @note "The mathematician is born, not made."
 *       -- Henri Poincare, The Value of Science (1905)
 */
module;

export module dedekind.algebra:boolean;

import dedekind.category;
import dedekind.order;
import dedekind.sets;

namespace dedekind::algebra {
using namespace dedekind::sets;

export template <typename L = dedekind::category::ClassicalLogic,
                 typename C = Finite>
using BooleanSetOf = Ω<bool, L, C>;

export using BooleanSet = BooleanSetOf<>;
export using 𝔹 = BooleanSet;

export inline constexpr BooleanSet B{};

/** @section Formal_Verification */

// BooleanSet is the canonical IsSet witness for the Boolean ambient universe.
static_assert(dedekind::category::IsSet<
                  decltype(dedekind::category::ambient_set<bool>(B))>,
              "BooleanSet must be the canonical IsSet anchor for bool.");

// `bool` under (min, max) is a (distributive) lattice --- the Boolean
// lattice 𝔹.  Witnessed at the source so downstream code does not
// have to rederive the claim.  These complement the algebraic
// witnesses elsewhere: `bool` under (XOR, AND) is the Galois field
// 𝔽_2 (see :galois), and `bool` under (OR, AND) is the Boolean rig
// (see :ring).  All three views agree on the underlying carrier.
static_assert(dedekind::order::IsOrderJoinSemilattice<bool>,
              "bool under max is a join-semilattice (the Boolean "
              "lattice's join).");
static_assert(dedekind::order::IsOrderMeetSemilattice<bool>,
              "bool under min is a meet-semilattice (the Boolean "
              "lattice's meet).");
static_assert(dedekind::order::IsOrderLattice<bool>,
              "bool under (min, max) is a lattice --- the Boolean "
              "lattice 𝔹.");
static_assert(dedekind::order::IsOrderDistributiveLattice<bool>,
              "bool under (min, max) is a distributive lattice "
              "(meet and join distribute over each other).");

// Order witnesses: bool with `<=` is totally ordered (false ≤ true).
// The `is_reflexive_v` / `is_transitive_v` / `is_antisymmetric_v`
// specs covering integral types in `:species` lift here, plus
// `std::totally_ordered<bool>` from the standard library.
static_assert(dedekind::order::IsPreOrdered<bool>,
              "bool with <= is a pre-order (reflexive + transitive).");
static_assert(dedekind::order::IsPartiallyOrdered<bool>,
              "bool with <= is a partial order (adds antisymmetry).");
static_assert(dedekind::order::IsTotallyOrdered<bool>,
              "bool with <= is totally ordered (false ≤ true).");

// `bool` is also a directed set: every pair has a common upper bound
// (trivially: `true` dominates).  This makes `bool` a valid \emph{net
// domain} in the @c sequences sense (cf.\ Munkres / Kelley: a net is
// a function from a directed set, not just from ℕ).  Witnessed here
// rather than in @c order:poset because the lattice structure on
// @c bool is anchored in the algebraic Boolean partition.
static_assert(dedekind::order::IsDirectedSet<bool>,
              "bool with <= is a directed set --- a valid net domain.");
static_assert(dedekind::order::IsDirectedPoset<bool>,
              "bool is a directed poset (directed + antisymmetric).");

}  // namespace dedekind::algebra
