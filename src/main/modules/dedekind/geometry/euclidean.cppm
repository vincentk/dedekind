/**
 * @file dedekind/geometry/euclidean.cppm
 * @brief The Study of Distance, Metrics, and Curvature.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Geometry: The Logic of Space.
 * @details This partition bridges Algebra and Topology by introducing the
 *          concept of a "Metric." It ensures that our numerical species
 *          can be measured not just as sets or fields, but as physical
 * points on a 1D, 2D, or n-dimensional manifold. Wikipedia: Geometry,
 * Metric space, Euclidean space
 */

module;

#include <concepts>
#include <cstddef>
#include <functional>

export module dedekind.geometry:euclidean;

import :inner_product;
import dedekind.algebra;

namespace dedekind::geometry {

/**
 * @concept IsMetricSpace
 * @brief A set equipped with a distance function (metric) d(a, b).
 * @details In our Naked Ontology, the metric projects any two points
 *          to a scalar field S, which must be an Ordered Field (the ruler).
 */
export template <typename T, typename S>
concept IsMetricSpace = requires(const T a, const T b) {
  // The Distance Morphism: d(a, b)
  { distance(a, b) } -> std::same_as<S>;
};

/**
 * @concept IsEuclideanSpace
 * @brief A Vector Space that is also a Metric Space where distance is the Norm.
 * @details Structural Constraint: distance(a, b) == norm(a - b).
 *          In the mirror, this is the "flat" geometry of R^n.
 */
export template <typename V, typename S>
concept IsEuclideanSpace = IsMetricSpace<V, S> && requires(const V v) {
  // The Norm (Magnitude) of a single vector.
  { norm(v) } -> std::same_as<S>;
  // Squared norm keeps Euclidean magnitude in polynomial form.
  { euclidean_norm_squared(v) } -> std::same_as<S>;
};

/**
 * @brief Canonical Euclidean distance induced by the norm.
 */
export template <dedekind::algebra::IsFloatingScalar F, std::size_t N>
constexpr F distance(const Vector<F, N>& a, const Vector<F, N>& b) {
  return norm(a - b);
}

/**
 * @brief Predicate factory over squared Euclidean norm.
 * @details Returns x |-> compare(euclidean_norm_squared(x), threshold).
 * Works for any carrier providing euclidean_norm_squared(x) via ADL.
 */
export template <typename Threshold, typename Compare = std::greater<>>
constexpr auto compare_euclidean_norm_squared(Threshold threshold,
                                              Compare compare = {}) {
  return [threshold, compare](const auto& x)
    requires requires {
      { euclidean_norm_squared(x) };
      { std::invoke(compare, euclidean_norm_squared(x), threshold) };
    }
  { return std::invoke(compare, euclidean_norm_squared(x), threshold); };
}

/**
 * @brief Predicate factory for points outside the closed Euclidean ball,
 *        using squared radius to avoid square roots.
 */
export template <typename RadiusSquared>
constexpr auto outside_closed_euclidean_ball_squared(
    RadiusSquared radius_squared) {
  return compare_euclidean_norm_squared(radius_squared, std::greater<>{});
}

}  // namespace dedekind::geometry
