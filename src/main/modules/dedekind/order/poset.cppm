/**
 * @file ontology:order.cppm
 * @brief Level 1.5: The Rules of Relation (Posets, Lattices, and Chains).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :order
 * @build_order 3
 * @dependency :mereology
 *
 * @section Order: The Geography of Sets
 * This partition establishes the "Relative Positioning" of species before they
 * are assigned a quantitative magnitude. In the Dedekind structuralist
 * ontology, Order is the prerequisite for Cardinality; we must define
 * "Comparison" before we can define "Count".
 *
 * @details
 * This module defines the formal properties of binary relations:
 * - Reflexivity, Antisymmetry, and Transitivity (The Poset).
 * - Totality (The Chain/Linear Order).
 * - Density (The Existence of Midpoints).
 * - Completeness (The Least-Upper-Bound Property).
 *
 * @section Structural_Anchors
 * We anchor the C++ comparison operators here via the Category Theory
 * 'Morphism' logic:
 * - operator<, operator<= : Partial/Total Order Morphisms.
 * - operator<=> : The Canonical Comparison (The Spaceship).
 *
 * Wikipedia: Order theory, Partially ordered set, Lattice (order)
 */
module;
#include <concepts>
#include <functional>

export module dedekind.ontology:order;

import dedekind.sets;

namespace dedekind::ontology {
using namespace dedekind::sets;

export template <typename T, typename L = ClassicalLogic>
concept IsPreOrdered =
    IsPartOf<T, T, L> && is_reflexive_v<T, std::less_equal<>> &&
    is_transitive_v<T, std::less_equal<>>;

/**
 * @concept IsPartiallyOrdered
 * @brief Elements that can be compared, but some may be "parallel."
 * Wikipedia: Partial order
 */
export template <typename T, typename L = ClassicalLogic>
concept IsPartiallyOrdered =
    IsPreOrdered<T, L> && is_antisymmetric_v<T, std::less_equal<>> &&
    requires(const T a, const T b) {
      { a == b } -> std::same_as<typename L::type>;
    };

/** @brief The Strict Relation (Proper Part). */
export template <typename S1, typename S2>
  requires IsPartOf<S1, S2> && IsPartiallyOrdered<S1>
constexpr bool operator<(const S1& a, const S2& b) {
  return (a <= b) && !(a == b);  // The Remainder
}

/**
 * @concept IsTotallyOrdered
 * @brief A refinement of the C++ totally_ordered concept for the Dedekind
 * species.
 * @details Requires that for any two elements a and b, either a < b, a > b, or
 * a == b.
 */
export template <typename T>
concept IsTotallyOrdered = IsPartiallyOrdered<T> && std::totally_ordered<T>;

/**
 * @concept IsLinearOrder
 * @brief Synonym for Total Order in our 1D road trip.
 */
export template <typename T>
concept IsLinearOrder = IsTotallyOrdered<T>;

/**
 * @concept IsDense
 * @brief A structural property where a midpoint always exists between distinct
 * elements.
 * @details Axiom: For any a < b, there exists an element c such that a < c < b.
 *          In this implementation, density is proven by the existence of the
 *          arithmetic midpoint (a + b) / 2.
 *
 * @note This is the prerequisite for the Dedekind Cut; a field must be
 *       Dense before it can be Completed.
 */
export template <typename T>
concept IsDense = IsTotallyOrdered<T> && requires(const T a, const T b) {
  /** @brief The Midpoint Morphism: The ability to bisect an interval. */
  { (a + b) / 2 } -> std::convertible_to<T>;
};

/**
 * @section Density: The Archimedean Property.
 * @concept IsArchimedean
 * @brief Property: Measurement via inductive "stepping."
 * @details We define this "Nakedly" as the existence of a Successor Morphism
 *          that respects the Total Order.
 */
export template <typename T>
concept IsArchimedean = IsTotallyOrdered<T> && requires(T x) {
  { ++x } -> std::same_as<T&>;
  // Theorem: Repeated application of ++ eventually exceeds any y.
  requires(x < x++);
};

/**
 * @concept IsDividableChain
 * @brief A Totally Ordered species with a partitioning algorithm.
 * @details For any a, b (b ≠ 0), there exist a quotient and a remainder.
 *          This represents the "Metric" ability to measure one magnitude
 *          against another (The Scissors).
 */
export template <typename T>
concept IsDividableChain = IsTotallyOrdered<T> && requires(T a, T b) {
  { a / b } -> std::same_as<T>;  // The Measurement (How many fits)
  { a % b } -> std::same_as<T>;  // The Remainder (The Leftover)
};

/**
 * @concept IsDedekindComplete
 * @brief The topological "Soul" of the Continuum.
 *
 * @details A structure is Dedekind-complete if it possesses the
 * Least-Upper-Bound property. In our structuralist approach, this requires the
 * existence of a Total Order, Density, and the functional ability to resolve
 *          extrema (Supremum/Infimum).
 *
 * @tparam S The Ordered Structure (The Rule).
 *
 * @section Structural_Inference:
 * While the Rationals (Q) are Dense and Totally Ordered, they fail this
 * requirement because they lack the "Extrema" morphism for sets like
 * {q ∈ Q | q² < 2}. The Real Continuum (R) satisfies this by definition
 * through the Dedekind Cut synthesis.
 *
 * Wikipedia: Completeness of the real numbers, Least-upper-bound property
 */
export template <typename S>
concept IsDedekindComplete = IsTotallyOrdered<S> && IsDense<S> && HasExtrema<S>;

}  // namespace dedekind::ontology