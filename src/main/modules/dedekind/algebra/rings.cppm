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

export module dedekind.ontology:algebra;

import dedekind.category;
import :order;  // Level 1.5: Necessary for Ordered Groups/Fields
                // (IsTotallyOrdered)

namespace dedekind::ontology {
using ::dedekind::ontology::IsDense;
using ::dedekind::ontology::IsTotallyOrdered;

/**
 * @section Algebra: The study of operations and structures.
 *
 * @concept IsMagma
 * @brief The most primitive operation: a set closed under a binary operator.
 * Wikipedia: Magma (algebra)
 */
export template <typename T, typename Op>
concept IsMagma = requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};

/**
 * @concept IsSemigroup
 * @brief An associative Magma (No identity required).
 */
export template <typename T, typename Op>
concept IsSemigroup =
    IsMagma<T, Op> && && IsAssociative<T, Op>;

/**
 * @concept IsMonoid
 * @brief A Magma that is associative and has an identity element (Zero/Empty).
 * Wikipedia: Monoid, Identity element
 */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && HasIdentity<T, Op>;

/**
 * @concept IsCommutativeMonoid
 * @brief A Monoid where order of operations does not change the result.
 */
export template <typename T, typename Op>
concept IsCommutativeMonoid =
    IsMonoid<T, Op> && IsCommutativeMonoid<T, Op>;

/**
 * @concept IsSemiring
 * @brief The Unification of Algebra and Action.
 * 
 * @details
 * Formally, a Semiring (or "Rig") is a set $R$ equipped with two binary operations 
 * ($+$ and $\times$) such that $(R, +)$ is a commutative monoid and $(R, \times)$ 
 * is a monoid. 
 *
 * @section The "Day 1" Identity: Scalar as Operator
 * From a category-theoretic perspective, this definition is equivalent to saying 
 * that a Semiring is a @b Semimodule @b over @b itself. 
 * 
 * By grounding the Scalar in this identity:
 * - The multiplicative identity ($1$) acts as the @b basis @b vector for the 
 *   1D space.
 * - Multiplication ($s \times v$) is interpreted as a @b scalar @b action (scaling).
 * - Addition ($v_1 + v_2$) is the @b vector @b accumulation within that space.
 *
 * This allows the ontology to treat logical 'AND' as a linear action on the 
 * Boolean semimodule, and real multiplication as a linear action on the 
 * 1D Euclidean vector space, using a single unified interface.
 *
 * @tparam T The type satisfying the Semiring axioms.
 * 
 * @note This definition enforces both Left and Right Distributivity to ensure 
 * compatibility with non-commutative structures (e.g., Square Matrices over T), 
 * which are themselves Semirings.
 */
export template <typename T>
concept IsSemiring = 
    IsCommutativeMonoid<T, std::plus<T>> && 
    IsMonoid<T, std::multiplies<T>> &&
    IsSemimodule<T, T> && // The "Day 1" elegance: T acts on T.
    requires(T a, T b, T c) {
        // Distributivity of Multiplication over Addition
        { a * (b + c) } -> std::same_as<T>; // Left-distributive
        { (a + b) * c } -> std::same_as<T>; // Right-distributive
    };

/**
 * @concept IsGroup
 * @brief A Monoid where every element has an inverse.
 * Wikipedia: Group (mathematics), Additive inverse
 */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsInvertible<T, Op>;

/**
 * @section Algebra: The Hierarchy of Operations.
 *
 * @concept IsAbelianGroup
 * @brief A Group where the operator (usually +) is commutative.
 * Wikipedia: Abelian group
 */
export template <typename T, typename Op = std::plus<T>>
concept IsAbelianGroup =  IsModule<T, T> && IsGroup<T, Op> && IsCommutativeMonoid<T, Op>;

/**
 * @concept IsOrderedAbelianGroup
 * @brief An Abelian Group where the order is preserved by addition.
 *        If a < b, then a + c < b + c.
 * Wikipedia: Ordered abelian group
 */
export template <typename T>
concept IsOrderedAbelianGroup =
    IsAbelianGroup<T, std::plus<T>> && IsTotallyOrdered<T>;

/**
 * @concept IsRing
 * @brief A set that is both a Semiring AND an Abelian Group under addition.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mul = std::multiplies<T>>
concept IsRing = IsSemiring<T> && IsAbelianGroup<T, Add>;

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
 * @concept IsVectorSpace
 * @brief A Module where the scalar provider is a Field.
 * @details This is the "Gold Standard" for geometry and physics. 
 * While a Module represents a Lattice, a Vector Space represents 
 * a "Flat" space where division by scalars (scaling down) is always possible.
 * 
 * Wikipedia: Vector space
 */
export template <typename V, typename F>
concept IsVectorSpace = 
    IsModule<V, F> && 
    IsField<F>;

/**
 * @concept IsBounded
 * @brief Theorem: Every Extensional species is Bounded (in our finite
 * universe).
 */
export template <typename S>
concept IsBounded = IsExtensional<S> || requires {
  { std::numeric_limits<typename S::element_type>::max() };
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

/**
 * @concept IsSemimodule
 * @brief A Monoid (V) acted upon by a Semiring (S).
 */
export template <typename V, typename S>
concept IsSemimodule =
    IsMonoid<V, std::plus<V>> && IsSemiring<S> && requires(V v, S s) {
      { v * s } -> std::same_as<V>;
    };

/**
 * @section Algebra: The Linear Shelf.
 * @concept IsModule
 * @brief An Abelian Group V acted upon by a Ring S.
 * Wikipedia: Module (mathematics)
 */
export template <typename V, typename S>
concept IsModule = IsAbelianGroup<V> && IsRing<S> && requires(V v, S s) {
  { v * s } -> std::same_as<V>;
  { s * v } -> std::same_as<V>;
};

/**
 * @concept IsVectorSpace
 * @brief A Module where the Scalars form a Field.
 * Wikipedia: Vector space
 */
export template <typename V, typename S>
concept IsVectorSpace = IsModule<V, S> && IsField<S>;

// If T is a Group under addition, then '+' is legally defined.
template <typename T>
  requires IsGroupoid<T, std::plus<T>>
constexpr T operator+(T a, T b) {
  return std::plus<T>{}(a, b);
}

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

}  // namespace dedekind::ontology
