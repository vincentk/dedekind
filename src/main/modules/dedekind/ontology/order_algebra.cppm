/**
 * @file ontology:order_algebra.cppm
 * @brief The Algebra of Ordered Sets, Rays, and Intervals.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Order Algebra: The Logic of Rays and Sums.
 * @details This partition defines the arithmetic of ordered structures.
 *          It introduces the Minkowski Sum (the addition of sets) and
 *          the properties of Rays (intervals bounded on one side).
 * Wikipedia: Minkowski addition, Ray (geometry), Interval arithmetic
 */

module;
#include <concepts>

export module dedekind.ontology:order_algebra;

import :mereology;
import :algebra;
import :order;

namespace dedekind::ontology {

/**
 * @concept IsMinkowskiSummable
 * @brief Species that support set-based addition.
 * @details A + B = { a + b : a ∈ A, b ∈ B }.
 */
export template <typename S>
concept IsMinkowskiSummable =
    IsSet<S, typename S::element_type> &&
    IsAbelianGroup<typename S::element_type> && requires(S a, S b) {
      { a + b } -> std::same_as<S>;
    };

/**
 * @concept IsRay
 * @brief A set representing all points greater than (or less than) a pivot.
 */
export template <typename R, typename T>
concept IsRay = IsSet<R, T> && IsTotallyOrdered<T> && requires(T pivot) {
  { R::upward_from(pivot) } -> std::same_as<R>;
  { R::downward_from(pivot) } -> std::same_as<R>;
};

}  // namespace dedekind::ontology
