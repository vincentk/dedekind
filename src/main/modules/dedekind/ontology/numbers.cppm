/**
 * @file ontology:number_systems.cppm
 * @brief Level 4: The Dictionary of Species (int, double, Rational, Real).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Number Systems: The Inductive and Algebraic Ladder.
 * @details This partition defines the requirements for N, Z, Q, and R.
 * Wikipedia: Natural number, Integer, Rational number, Real number
 *
 * @partition :numbers
 * @build_order 5
 * @details This partition defines the requirements for N, Z, Q, and R.
 * This contains the actual C++ types that serve as the coordinates for our
 * mathematical universe.
 * @anchors C++ Primitive Types: bool, char, int, uint64_t, float.
 * Wikipedia: Natural number, Integer, Rational number, Real number
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
