/**
 * @file ontology:algebra.cppm
 * @partition :algebra
 * @brief Level 3: The Rules of Harmony (Groups, Rings, and Fields).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Algebra: The Synthesis of Body and Action
 * This partition defines the "Soul" of the Dedekind species. While mereology
 * defines the notion of a "Set", algebra provides structure-preserving
 * operations.
 *
 * @details
 * We anchor the standard C++ arithmetic operators as formal Algebraic
 * Morphisms within the Dedekind Ontology:
 * - operator+ : The Additive Group Morphism (The Translation).
 * - operator* : The Multiplicative Morphism (The Scaling).
 * - operator- : The Inverse Morphism (The Symmetry).
 *
 * @build_order 4
 * @dependency :category, :mereology, :order
 *
 * @see dedekind.ontology:category (Level 0)
 * @see dedekind.ontology:mereology (Level 1)
 * @see dedekind.ontology:order (Level 1.5)
 *
 * Wikipedia: Abstract algebra, Group theory, Ring (mathematics), Field
 * (physics)
 */
module;

#include <compare>     // for std::strong_ordering
#include <concepts>    // for std::integral, std::floating_point
#include <functional>  // for std::plus, std::multiplies

export module dedekind.algebra:rings;

import dedekind.category;
import dedekind.order;
import dedekind.sets;

import :rings;
import :division;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sets;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is also commutative.
 * @details This is the foundation for Z, Q, and R.
 * Wikipedia: Commutative ring
 */
export template <typename T>
concept IsCommutativeRing =
    IsRing<T> && IsCommutativeMonoid<T, std::multiplies<T>>;

/**
 * @concept IsEuclidean
 * @brief A Commutative Ring with a division algorithm.
 * @details For any a, b (b ≠ 0), there exist q, r such that a = bq + r.
 *
 * Wikipedia: Euclidean domain
 */
export template <typename T>
concept IsEuclidean =
    IsDividableChain<T> && IsCommutativeRing<T> && requires(T a, T b) {
      // The Division Morphism
      { a / b } -> std::same_as<T>;
      // The Remainder Morphism (The "Scissors")
      { a % b } -> std::same_as<T>;

      // Euclidean Property: a = (a/b)*b + (a%b)
      // Note: In C++, we assume std::divides and std::modulus satisfy this.
    };

/**
 * @concept IsDivisionRing
 * @brief A Ring where every non-zero element has a multiplicative inverse.
 * @details This is the formal "Roadblock" for division by zero.
 */
export template <typename T>
concept IsDivisionRing = IsRing<T> && requires(T a, T b) {
  /**
   * @brief The Division Morphism.
   * Note: The "b != 0" requirement is a runtime contract,
   * not a compile-time type constraint.
   */
  { a / b } -> std::same_as<T>;
};

/**
 * @concept IsField
 * @brief The "Painless" Field: A Commutative Ring where every non-zero element
 *        has a multiplicative inverse (Division).
 * Wikipedia: Field (mathematics)
 */
export template <typename T>
concept IsField = IsCommutativeRing<T> && IsDivisionRing<T>;

/**
 * @concept IsAlgebraicallyClosed
 * @brief Semantic requirement for a Field where every polynomial has a root.
 * @details This is the "Soul" property required by Algebra_ℂ.
 */
export template <typename M>
concept IsAlgebraicallyClosed = IsField<M>;  // Refined by its use in Algebra_ℂ

/**
 * @concept IsBounded
 * @brief Theorem: Every Extensional species is Bounded (in our finite
 * universe).
 */
export template <typename S>
concept IsBounded = IsExtensional<S> || requires {
  { std::numeric_limits<typename S::Domain>::max() };
};

/**
 * @section Algebra: Actions and Scaling.
 * @concept IsScalableBy
 * @brief An additive species T that can be "stepped" by an index N.
 * @note This is the "Naked" engine of the Archimedean property.
 */
export template <typename T, typename N>
concept IsScalableBy = requires(T x, N n) {
  { x * n } -> std::same_as<T>;
};

// If T is a Ring, then '*' is legally defined.
template <typename T>
  requires IsSmallCategory<T, std::multiplies<T>>
constexpr T operator*(T a, T b) {
  return std::multiplies<T>{}(a, b);
}

// If T is a Boolean species, then '|' is a Categorical Join (Union).
template <typename T>
  requires IsAbelian<T, std::logical_or<T>>
constexpr T operator|(T a, T b) {
  return std::logical_or<T>{}(a, b);
}

}  // namespace dedekind::algebra
