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
concept IsNatural = IsPointed<N> && 
                   IsClosedSet<N> && 
                   IsArchimedean<N> &&
                   IsMonoid<N, std::plus<N>> &&
                   IsMonoid<N, std::multiplies<N>> &&
                   requires(N n) {
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
    { -a } -> std::same_as<Z>;    // Unary Inverse
    { a - b } -> std::same_as<Z>; // Binary Subtraction
};

/**
 * @concept IsRational
 * @brief The extension of Z to a Field (Q).
 * Wikipedia: Rational number
 * @details A Rational is a Dense Field that remains Archimedean.
 *          It is "Measured" by the Integers.
 */
export template <typename Q>
concept IsRational = IsOrderedField<Q> && IsDense<Q> && IsArchimedean<Q> && 
                    requires(Q q) { 
    { q.numerator() }   -> IsInteger;
    { q.denominator() } -> IsInteger;
};
} // namespace dedekind::ontology
