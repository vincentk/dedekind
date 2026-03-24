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

import :mereology;      // For IsSet
import :cardinalities;  // For ℵ_0, ℶ_1
import :order;          // For IsArchimedean, IsDedekindComplete
import :algebra;        // For IsCommutativeMonoid, IsField, IsRing
import :morphologies;   // For IsOrderedField, IsEuclidean, IsCyclic

namespace dedekind::ontology {

/**
 * @concept IsNumbers
 * @brief The Root Category for all Numerical Structures.
 *
 * @tparam M The Algebraic Structure (The "Rule").
 * @tparam C The Cardinality (The "Magnitude").
 */
export template <typename M, typename C>
concept IsNumbers = IsSet<M> && IsCardinality<C> && requires {
  // This "locks" the structure to the ruler
  requires std::same_as<typename M::cardinality_type, C>;
};

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

/**
 * @concept Monoid_ℕ
 * @brief The Parametric Algebraic Soul of the Natural Numbers.
 *
 * @tparam M The Monoid structure.
 * @tparam E The underlying Element species.
 */
export template <typename M, typename E = typename M::element_type>
concept Monoid_ℕ = IsNumbers<M, ℵ_0> && IsNatural<E> && requires(const M& m) {
  // The structure must actually possess the claimed cardinality.
  { m.cardinality() } -> std::same_as<ℵ_0>;

  // The Soul: Structural Laws
  requires IsArchimedean<M>;
  requires IsCommutativeMonoid<M, std::plus<E>>;
  requires IsCommutativeMonoid<M, std::multiplies<E>>;
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
 * @concept Group_ℤ
 * @brief The Canonical Algebraic Soul of the Integers.
 *
 * @details ℤ is the uniquely determined Infinite Cyclic Group (under addition)
 *          that extends the Naturals with additive inverses.
 */
export template <typename M, typename E = typename M::element_type>
concept Group_ℤ = IsNumbers<M, ℵ_0> &&  // The Magnitude (Coded in)
                  IsInteger<E> &&       // The Species (The "What")
                  requires(const M& m) {
                    // The Soul: Group Axioms
                    // Note: Subtraction is now a Total Morphism.
                    requires IsOrderedAbelianGroup<M> && IsCommutativeRing<M> &&
                                 IsArchimedean<M> && IsEuclidean<M>;
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
 * @concept Field_ℚ
 * @brief The Canonical Algebraic Soul of the Rational Numbers.
 *
 * @details ℚ is defined as the unique Ordered, Dense, Archimedean Field
 *          constructed over an underlying Integer species. In our
 *          "Rules, not buckets" manifesto, this concept "blesses" a
 *          coordinate species (E) with the structural laws of the
 *          Rational Field (M).
 *
 * @tparam M The Field structure (The "Rule" or "Soul").
 * @tparam C The Magnitude (Strictly Aleph_0 for the universal field).
 * @tparam E The underlying Rational species (The "What" / Element).
 * @tparam Z The underlying Integer species (The "Ancestry" of E).
 *
 * @section Structural_Recursion:
 * This concept enforces that the Field is strictly Countable (Aleph_0)
 * and that every element E can be projected back to its Integer
 * components (Z), ensuring a verified path from N to Q.
 */
export template <typename M, typename E, typename Z>
concept Field_ℚ =
    IsNumbers<M, ℵ_0> && IsRational<E, Z> &&  // <--- The Relative Species Check
    requires(const M& m) {
      requires IsOrderedField<M>;
      requires IsDense<M>;
      requires IsArchimedean<M>;
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
 * @brief The Continuous Field property.
 * @details A species is Continuous if it possesses the cardinality
 *          of the power set of the naturals (Beth-1) and satisfies
 *          the Dedekind-Completeness axiom.
 *
 * This represents the "Smooth" transition where gaps in the
 * Rational field have been "filled" by the Dedekind Cut.
 */
export template <typename S>
concept IsContinuous =
    IsUncountable<typename S::cardinality_type> && IsDedekindComplete<S>;

/**
 * @concept IsDiscrete
 * @brief A space of isolated points (Z, N).
 * @details A species is Discrete if it is Countable but lacks a
 *          midpoint morphism between its elements. This property
 *          enables mathematical induction and successor-based logic.
 * @note Axiom: For a discrete set, there exists a "gap" between any
 *       two distinct elements.
 */
export template <typename S>
concept IsDiscrete = IsCountable<S> && !IsDense<S>;

/**
 * @concept Continuum_ℝ
 * @brief The Canonical "Blessing" of the Real Numbers.
 *
 * @tparam M The Field structure (The "Rule").
 * @tparam E The underlying Real species (The "Element").
 * @tparam Q The underlying Rational species (The "Ancestry").
 */
export template <typename M, typename E, typename Q>
concept Continuum_ℝ = IsNumbers<M, ℶ_1> && IsReal<E> && requires(const M& m) {
  /** @property IsDedekindComplete: The defining "Soul" of R. */
  requires IsDedekindComplete<M> && IsOrderedField<M> && IsArchimedean<M>;
};

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

/**
 * @concept Algebra_ℂ
 * @brief The Canonical "Blessing" of the Complex Numbers.
 *
 * @details ℂ is the Algebraically Closed Field over the Real Continuum.
 *          It inherits the Magnitude (Beth_1) but rejects the Order.
 */
export template <typename M, typename E, typename R>
concept Algebra_ℂ =
    IsNumbers<M, ℶ_1> && IsComplex<E, R> && requires(const M& m) {
      requires IsField<M>;

      /**
       * @property IsAlgebraicallyClosed
       * This soul is "pre-validated" by the species' ability to
       * solve quadratic roots via sqrt().
       */
      requires IsAlgebraicallyClosed<M>;

      /** @property !IsOrderedField: C is NOT totally ordered.
       */
      requires !IsOrderedField<M>;
    };

};  // namespace dedekind::ontology
