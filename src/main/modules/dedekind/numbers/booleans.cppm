/**
 * @file ontology:numbers.cppm
 * @brief Formal definition of the Boolean system 𝔹 within the numeric ontology.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :numbers
 * @dependency :algebra, :topology, :cardinalities, :scalars
 *
 * @section Booleans: The Binary Logic Structure
 * This partition defines the @b Boolean species—the simplest non-trivial
 * numerical system. It establishes the formal mapping between logical
 * truth values and the algebraic structure of a semiring.
 *
 * @details
 * The Boolean system is modeled as an @b Idempotent @b Commutative @b Semiring
 * over the set {0, 1}. In this mapping:
 * - Addition (⊕) is represented by @b Logical @b OR (∨).
 * - Multiplication (⊗) is represented by @b Logical @b AND (∧).
 * - The Additive Identity (0) is @b False (⊥).
 * - The Multiplicative Identity (1) is @b True (⊤).
 *
 * This structure is uniquely characterized by its cardinality |S| = 2 and its
 * idempotency property (x ⊕ x = x), which distinguishes it from the
 * additive properties of ℕ or ℝ.
 *
 * Wikipedia: Boolean algebra, Semiring, Idempotency
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "It is not of the essence of mathematics to be conversant with the
 * ideas of number and quantity."
 *       -- George Boole, An Investigation of the Laws of Thought (1854)
 */
module;

#include <concepts>
#include <functional>

export module dedekind.numbers:booleans;

import dedekind.category;
import dedekind.sets;
import :scalars;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename L = ClassicalLogic>
using FiniteBooleanSetOf = dedekind::sets::FiniteBooleanSet<L>;

/**
 * @concept Is_B
 * @brief Identifies a structure as a formal Boolean Semiring (𝔹).
 *
 * Defines a set where the algebraic rules are governed by logical
 * disjunction and conjunction, maintaining a cardinality of exactly 2.
 *
 * @tparam M The Algebraic Structure governing the Boolean set.
 * @tparam E The underlying element type (e.g., bool).
 */
export template <typename M, typename E = typename M::Domain>
concept Is_B = std::same_as<E, bool> && requires(const M& m) {
  // The structure must possess a discrete cardinality |S| = 2.
  { m.cardinality() } -> std::same_as<std::size_t>;
  requires m.cardinality() == 2;
};

}  // namespace dedekind::numbers
