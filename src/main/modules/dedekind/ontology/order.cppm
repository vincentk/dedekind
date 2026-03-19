/**
 * @file ontology:order.cppm
 * @brief The Foundation of Ordered Structures and Density.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Order Theory: The Logic of Midpoints and Totality.
 * @details This partition defines the properties of ordered types.
 *          It introduces Density—the algebraic requirement that a
 *          midpoint exists between any two distinct elements.
 *
 * Wikipedia: Total order, Dense set, Midpoint
 */

module;
#include <concepts>

export module dedekind.ontology:order;

import :algebra;  // For arithmetic requirements (+, /)

namespace dedekind::ontology {

/**
 * @concept IsTotallyOrdered
 * @brief A refinement of the C++ totally_ordered concept for the Dedekind
 * species.
 * @details Requires that for any two elements a and b, either a < b, a > b, or
 * a == b.
 */
export template <typename T>
concept IsTotallyOrdered = std::totally_ordered<T>;

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

}  // namespace dedekind::ontology