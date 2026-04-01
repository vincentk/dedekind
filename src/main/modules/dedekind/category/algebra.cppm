/**
 * @file ontology:category.cppm
 * @partition :algebra
 * @brief Level 0.2: The Laws of Composition (Magmas, Monoids, and Groups).
 * 
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:algebra;

import :species;

namespace dedekind::category {

/** @concept IsMagma: A species closed under a binary operator. */
export template <typename T, typename Op>
concept IsMagma = requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};

/** @concept IsSemigroup: An associative Magma. */
export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && IsAssociative<T, Op>;

/** @concept IsMonoid: A Semigroup with an Identity. */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && HasIdentity<T, Op>;

/** @concept IsCommutativeMonoid: A Monoid where order is invariant. */
export template <typename T, typename Op>
concept IsCommutativeMonoid = IsMonoid<T, Op> && IsCommutative<T, Op>;

/** @concept IsGroup: A Monoid where every element is invertible. */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsInvertible<T, Op>;

/** @concept IsAbelianGroup: A Group where the law is commutative. */
export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutative<T, Op>;

} // namespace dedekind::category
