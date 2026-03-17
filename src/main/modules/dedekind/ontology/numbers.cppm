/**
 * @file ontology:number_systems.cppm
 * @brief The Hierarchy of Numerical Species.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Number Systems: The Inductive and Algebraic Ladder.
 * @details This partition defines the requirements for N, Z, Q, and R.
 * Wikipedia: Natural number, Integer, Rational number, Real number
 */

export module dedekind.ontology:numbers;

import :mereology;
import :topology;
import :algebra;

namespace dedekind::ontology {

/**
 * @concept IsNatural
 * @brief N is a Pointed, Closed, Archimedean, Commutative Monoid
 *        under BOTH Addition and Multiplication.
 * Wikipedia: Semiring, Peano axioms
 */
export template <typename N>
concept IsNatural = IsPointed<N> && IsClosedSet<N> && IsArchimedean<N> &&
                    IsMonoid<N, std::plus<N>> &&
                    IsMonoid<N, std::multiplies<N>> && requires(N n) {
                      { ++n } -> std::same_as<N&>;
                      { n.is_origin() } -> std::convertible_to<bool>;
                    };

/**
 * @concept IsInteger
 * @brief The species where subtraction is finally a Total Morphism.
 * @details a - b is structurally defined as a + (-b).
 */
export template <typename Z>
concept IsInteger = IsNatural<Z> && requires(Z a, Z b) {
  { -a } -> std::same_as<Z>;     // Unary Inverse
  { a - b } -> std::same_as<Z>;  // Binary Subtraction
};

/**
 * @concept IsRational
 * @brief The extension of Z to a Field (Q).
 * Wikipedia: Rational number
 * @details A Rational is a Dense Field that remains Archimedean.
 *          It is "Measured" by the Integers.
 */
export template <typename Q>
concept IsRational =
    IsOrderedField<Q> && IsDense<Q> && IsArchimedean<Q> && requires(Q q) {
      { q.numerator() } -> IsInteger;
      { q.denominator() } -> IsInteger;
    };

/**
 * @concept IsReal
 * @brief The species of the Continuum (R).
 * @details A Real species is a Dedekind-Complete, Archimedean Field.
 *          This is the "Smooth" destination of our numerical journey.
 * Wikipedia: Real number
 */
export template <typename R>
concept IsReal = IsDedekindComplete<R> && IsArchimedean<R>;

/**
 * @concept IsComplex
 * @brief The Algebraic Closure of the Continuum.
 * @details A species that is an Algebraically Closed Field.
 *          Every non-constant polynomial has a root within this species.
 * Wikipedia: Complex number, Fundamental theorem of algebra
 */
export template <typename C, typename R>
concept IsComplex = IsField<C> && requires(const C z) {
  // The "Naked" Projection to the Plane
  { z.real() } -> std::same_as<R>;
  { z.imag() } -> std::same_as<R>;
  { z.conjugate() } -> std::same_as<C>;
  requires IsReal<R> &&
               // Algebra: Complex numbers are NOT Totally Ordered!
               // (You can't say if 'i' is greater than '1').
               !IsTotallyOrdered<C>;
};
}  // namespace dedekind::ontology
