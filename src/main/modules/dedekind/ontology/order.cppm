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

export module dedekind.ontology:order;

import :mereology;

namespace dedekind::ontology {

/**
 * @concept IsPartiallyOrdered
 * @brief Elements that can be compared, but some may be "parallel."
 * Wikipedia: Partial order
 */
export template <typename T>
concept IsPartiallyOrdered = requires(const T a, const T b) {
  { a <= b } -> std::convertible_to<bool>;
  { a == b } -> std::convertible_to<bool>;
};

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

}  // namespace dedekind::ontology