/**
 * @file kleene.cppm
 * @partition :kleene
 * @module dedekind.algebra:kleene
 * @brief Kleene Algebra Starter Package: algebraic foundations for regular
 * languages.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Starter_Intent
 * This partition offers a small, explicit entry point for Kleene algebra in
 * the set-builder DSL. It exports canonical Kleene universe aliases so
 * examples remain readable and stable.
 *
 * @section Notation
 * - `𝕶`: canonical Unicode symbol for the Kleene ambient set.
 * - `K`: ASCII alias for environments where Unicode input is inconvenient.
 *
 * @section Paper_Alignment
 * In the paper's algebraic hierarchy, Kleene algebra extends Boolean lattices
 * with a closure (iteration) operator, enabling the representation of regular
 * languages and finite automata. The algebraic structure validates the
 * equivalence of regular expressions and state machines.
 *
 * Element scouts are intentionally local (e.g. `auto k = var<KleeneSet>;`) to
 * avoid global name shadowing in downstream code.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "The theory of algorithms is at the foundation of mathematics and of
 * all rigorous thinking whatsoever. Every proof is an algorithm, and every
 * algorithm is a proof."
 *       -- Stephen Cole Kleene, *Introduction to Metamathematics* (1952)
 */
module;

export module dedekind.algebra:kleene;

import :boolean;
import dedekind.category;
import dedekind.sets;

namespace dedekind::algebra {
using namespace dedekind::sets;

export template <typename L = dedekind::category::ClassicalLogic,
                 typename C = Finite>
using KleeneSetOf = Ω<bool, L, C>;

export using KleeneSet = KleeneSetOf<>;
export using 𝕶 = KleeneSet;

export inline constexpr KleeneSet K{};

}  // namespace dedekind::algebra
