/**
 * @file ontology:density.cppm
 * @brief The Study of In-Betweenness and Limits.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Density: The Grain of the Continuum.
 * @details This partition defines the requirements for a species to be "Dense,"
 *          ensuring that no gaps exist that cannot be approached by a sequence.
 * Wikipedia: Dense set, Order density, Archimedean property
 */

export module dedekind.ontology:density;

import :mereology;
import :numbers;

namespace dedekind::ontology {

/**
 * @concept IsDense
 * @brief A Total Order where a midpoint always exists between distinct
 * elements.
 * @details Structural Proof: For any a < b, there exists c such that a < c < b.
 *          In our "Naked" world, this is satisfied by the existence of (a + b)
 * / 2.
 * @note Wikipedia: Dense set
 */
export template <typename T>
concept IsDense = IsTotallyOrdered<T> && requires(const T a, const T b) {
  // The "Midpoint" Morphism
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
};

/**
 * @concept IsDedekindComplete
 * @brief The "Smooth" Destination (R).
 * @details An Ordered Field where every non-empty subset that is
 *          bounded above has a supremum within the same species.
 * Wikipedia: Completeness of the real numbers
 */
export template <typename T>
concept IsDedekindComplete = IsOrderedField<T> && IsDense<T> && requires(T a) {
  // The "Supremum" Morphism:
  // The ability to find the 'limit' or 'ceiling' of a bounded set.
  { supremum_of(a) } -> std::same_as<T>;
};

}  // namespace dedekind::ontology
