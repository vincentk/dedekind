/**
 * @file ontology:numbers.cppm
 * @brief Level 4: The Dictionary of Species (The Registry).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :numbers
 * @build_order 7
 * @dependency :algebra, :topology, :cardinalities
 *
 * @section Numbers: The Realization of the Soul
 * This partition is the final "Registry" of the ontology. It maps concrete
 * C++ types to their formal algebraic and topological identities.
 *
 * @details
 * We "Bless" the coordinate species by verifying their rungs on the ladder:
 * - IsNatural  : N (ℕ) - The Discrete Monoid.
 * - IsInteger  : Z (ℤ) - The Euclidean Group.
 * - IsRational : Q (ℚ) - The Countable Dense Field.
 * - IsReal     : R (ℝ) - The Continuous Dedekind-Complete Field.
 *
 * @section Structural_Mapping
 * This is where we perform the final 'Lifting'. We prove that 'int'
 * satisfies 'Group_ℤ' and that 'double' is a hardware-constrained
 * approximation of 'Field_ℝ'.
 *
 * @anchors C++ Fundamental Types: bool, char, int, long, float, double.
 *
 * Wikipedia: Number, Natural number, Integer, Rational number, Real number
 */
module;

#include <concepts>
#include <functional>

export module dedekind.ontology:numbers;

import :cardinalities;  // The foundation of magnitude

namespace dedekind::ontology {

export template <typename T>
concept IsAdditiveSpecies = requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
  { T::zero() } -> std::same_as<T>;
};

/**
 * @concept IsNatural
 * @brief N is a Pointed, Closed, Archimedean, Commutative Monoid
 *        under BOTH Addition and Multiplication.
 * Wikipedia: Semiring, Peano axioms
 */
export template <typename N>
concept IsNatural = IsAdditiveSpecies<N> && requires(N n, N m) {
  { N::successor(n) } -> std::same_as<N>;
};

export template <typename T>
concept IsReflectiveSpecies = IsAdditiveSpecies<T> && requires(T a) {
  { -a } -> std::same_as<T>;  // Additive Inverse
};

/**
 * @concept IsInteger
 * @brief The species where subtraction is finally a Total Morphism.
 * @details a - b is structurally defined as a + (-b).
 */
export template <typename Z>
concept IsInteger = IsReflectiveSpecies<Z> && requires(Z a, Z b) {
  { -a } -> std::same_as<Z>;     // Unary Inverse
  { a - b } -> std::same_as<Z>;  // Binary Subtraction
};

/**
 * @concept IsFieldSpecies
 * @brief The common "DNA" for all division-capable species (Q, R, C).
 * @details Provides the tools for multiplicative reflection.
 */
export template <typename T>
concept IsFieldSpecies = IsReflectiveSpecies<T> && requires(T a, T b) {
  { a / b } -> std::same_as<T>;
  { a.inverse() } -> std::same_as<T>;
  { T::one() } -> std::same_as<T>;
};

/**
 * @concept IsRational
 * @brief Q is a rational species constructed over the integer species Z.
 *
 * @tparam Q The Rational species (The "What").
 * @tparam Z The underlying Integer species.
 */
export template <typename Q, typename Z>
concept IsRational = IsFieldSpecies<Q> && IsInteger<Z> && requires(Q q, Z z) {
  /** @brief Projection: Proves Q is a ratio of Z. */
  { q.numerator() } -> std::same_as<Z>;
  { q.denominator() } -> std::same_as<Z>;
};

/**
 * @concept IsReal
 * @brief The species of the Continuum (R).
 * @details A Real species is a Dedekind-Complete, Archimedean Field.
 *          This is the "Smooth" destination of our numerical journey.
 * Wikipedia: Real number
 */
export template <typename R>
concept IsReal = IsFieldSpecies<R>;

/**
 * @concept IsComplex
 * @brief The coordinate species of the Complex Plane (The "What").
 *
 * @tparam C The Complex species (The Element).
 * @tparam R The underlying Real species (The "Ancestry").
 *
 * @details C is a Field Species that provides projections to its Real (R)
 *          and Imaginary (R) components.
 */
export template <typename C, typename R>
concept IsComplex = IsFieldSpecies<C> && IsReal<R> && requires(const C z, R r) {
  /** @brief Projections: C ↠ R */
  { z.real() } -> std::same_as<R>;
  { z.imag() } -> std::same_as<R>;

  /**
   * @brief Algebraic Morphism: √z
   * In C, every number has a square root within the species.
   */
  { z.sqrt() } -> std::same_as<C>;

  /** @brief Morphism: The Complex Conjugate z̄ */
  { z.conjugate() } -> std::same_as<C>;
};

};  // namespace dedekind::ontology
