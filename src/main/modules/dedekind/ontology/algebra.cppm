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

import :category;   // Level 0: To define Morphisms (Monads, Functors, Identity)
import :mereology;  // Level 1: To define the Domain of Discourse (IsSet,
                    // IsPointed)
import :order;      // Level 1.5: Necessary for Ordered Groups/Fields
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
    IsMagma<T, Op> && requires { requires is_associative_v<T, Op>; };

/**
 * @concept IsMonoid
 * @brief A Magma that is associative and has an identity element (Zero/Empty).
 * Wikipedia: Monoid, Identity element
 */
export template <typename T, typename Op>
concept IsMonoid = IsMagma<T, Op> && requires {
  { identity_v<T, Op> } -> std::same_as<T>;
};

/**
 * @concept IsCommutativeMonoid
 * @brief A Monoid where order of operations does not change the result.
 */
export template <typename T, typename Op>
concept IsCommutativeMonoid =
    IsMonoid<T, Op> && requires { requires is_commutative_v<T, Op>; };

/**
 * @concept IsSemiring
 * @brief A set with two Monoids (Add, Mul) where Mul distributes over Add.
 * @note This is the home of 'bool' and 'Natural Numbers'.
 */
export template <typename T>
concept IsSemiring =
    IsMonoid<T, std::plus<T>> && IsMonoid<T, std::multiplies<T>> &&
    requires(T a, T b, T c) {
      { a * (b + c) } -> std::same_as<T>;  // Distributivity
    };

/**
 * @concept IsNumbers
 * @brief The Root Category for all Numerical Structures.
 *
 * @tparam M The Algebraic Structure (The "Rule").
 */
export template <typename M>
concept IsNumbers = IsSet<M>;

/**
 * @concept IsGroup
 * @brief A Monoid where every element has an inverse.
 * Wikipedia: Group (mathematics), Additive inverse
 */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && requires(T a) {
  { inverse<T, Op>(a) } -> std::same_as<T>;
};

/**
 * @section Algebra: The Hierarchy of Operations.
 *
 * @concept IsAbelianGroup
 * @brief A Group where the operator (usually +) is commutative.
 * Wikipedia: Abelian group
 */
export template <typename T, typename Op = std::plus<T>>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutativeMonoid<T, Op>;

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
 * @concept IsCyclic
 * @brief The "Clock" Soul: An Abelian Group that wraps after n steps.
 *
 * @details A structure is Cyclic if it is an Abelian Group where every element
 *          is invariant under the Modulus. This defines the topology of a
 *          Circle rather than a Line.
 *
 * @tparam T The coordinate species.
 *
 * @section Structural_Logic:
 * 1. Magnitude: The cardinality must be Finite and equal to the modulus.
 * 2. Symmetry: Every Cyclic group is Abelian (Commutative).
 * 3. Closure: The Successor Morphism (++x) eventually returns to the Identity.
 *
 * Wikipedia: Cyclic group, Modular arithmetic, Circle group
 */
export template <typename T>
concept IsCyclic = IsAbelianGroup<T, std::plus<T>> &&
                   IsFinite<typename T::cardinality_type> && requires(T a) {
                     /** @brief The Modulus: The circumference/order of the
                      * cycle. */
                     { T::modulus() } -> std::convertible_to<T>;

                     /** @brief The Remainder Morphism: x mod n. */
                     { a % T::modulus() } -> std::same_as<T>;

                     /** @brief Axiom: The set is its own remainder. */
                     requires(a % T::modulus() == a);

                     /** @brief Proof: The size of the set matches the cycle
                      * length. */
                     requires(T::cardinality() == T::modulus());
                   };

/**
 * @concept IsCyclicRing
 * @brief The "Painless" Modular Arithmetic (Z/nZ).
 *
 * @details A Cyclic Ring is the algebraic soul of a Finite Commutative Ring.
 *          By requiring IsCommutativeRing, we satisfy IsRing implicitly.
 */
export template <typename T>
concept IsCyclicRing = IsCommutativeRing<T> && IsCyclic<T>;

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
 * @concept IsOrderedField
 * @brief A Field where the algebraic operations preserve the Total Order.
 * @details Axiom 1: If a < b, then a + c < b + c.
 *          Axiom 2: If 0 < a and 0 < b, then 0 < ab.
 * Wikipedia: Ordered field
 */
export template <typename T>
concept IsOrderedField = IsField<T> && IsTotallyOrdered<T>;

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
