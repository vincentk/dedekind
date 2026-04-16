/**
 * @file ontology:numbers.cppm
 * @brief Semirings as the formal algebraic basis for scalar number systems.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :scalars
 * @dependency :algebra, :topology, :cardinalities
 *
 * @section Numbers: The Registry of Numeric Types
 * This partition defines the @b Scalar—the fundamental unit of the ontology.
 * It maps concrete C++ types to their formal algebraic and topological
 * identities.
 *
 * @details
 * A Scalar is the "Lowest Common Denominator" of our number systems. By
 * grounding the definition in a @b Semiring (or "Rig"), we create a unified
 * interface that encompasses:
 * - @b Booleans : The Binary Semiring ({0, 1}, ∨, ∧).
 * - @b Naturals : The Discrete Monoid (ℕ, +, ×).
 * - @b Reals    : The Continuous Field (ℝ, +, ×).
 *
 * This structure guarantees an additive identity (Zero) and a multiplicative
 * identity (One), providing the necessary anchors for all coordinate-based
 * calculations without requiring additive inverses (negatives).
 *
 * @anchors C++ Fundamental Types: bool, char, int, long, float, double.
 *
 * Wikipedia: Scalar (mathematics), Semiring, Monoid, Number system
 */
module;

#include <concepts>
#include <functional>

export module dedekind.numbers:scalars;

import dedekind.algebra; // The Pure Laws (Groups, Rings, and Fields)
import dedekind.category;
import dedekind.sets;

namespace dedekind::numbers {
using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @concept IsIntegralScalar
 * @brief Numbers-layer alias for the algebra scalar bridge concept.
 */
export template <typename S>
concept IsIntegralScalar = dedekind::algebra::IsIntegralScalar<S>;

/**
 * @concept IsUnsignedIntegralScalar
 * @brief Numbers-layer alias for the algebra scalar bridge concept.
 */
export template <typename S>
concept IsUnsignedIntegralScalar =
    dedekind::algebra::IsUnsignedIntegralScalar<S>;

/**
 * @concept IsFloatingScalar
 * @brief Numbers-layer alias for the algebra scalar bridge concept.
 */
export template <typename S>
concept IsFloatingScalar = dedekind::algebra::IsFloatingScalar<S>;

/**
 * @concept IsNumbers
 * @brief The Root Category for all Numerical Structures.
 *
 * @tparam M The Algebraic Structure (The "Rule").
 * @tparam C The Cardinality (The "Magnitude").
 */
export template <typename M, typename C>
concept IsScalar =
    dedekind::algebra::IsSemiring<M> && IsCardinality<C> && requires {
      // This "locks" the structure to the ruler
      requires std::same_as<typename M::cardinality_type, C>;
    };

};  // namespace dedekind::numbers
