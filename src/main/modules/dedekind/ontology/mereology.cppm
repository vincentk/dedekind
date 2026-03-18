/**
 * @file ontology:mereology.cppm
 * @brief The Study of Parts, Wholes, and Boundaries.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Mereology: The Foundation of Sets and Lattices.
 * @details This partition defines the "Naked" requirements for existence and
 *          containment. It provides the axioms for IsSet, IsLattice, and the
 *          ternary relation of Betweenness. In our structuralist journey,
 *          Mereology ensures that "Rules, not buckets" govern how elements
 *          relate to the totalities they inhabit.
 * Wikipedia: Mereology, Set theory, Lattice (order)
 */
module;

#include <compare>
#include <concepts>
#include <functional>

export module dedekind.ontology:mereology;

/**
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::ontology {

/**
 * @concept IsPartiallyOrdered
 * @brief Elements that can be compared, but some may be "parallel."
 * Wikipedia: Partial order
 */
export template <typename T>
concept IsPartiallyOrdered = requires(const T a, const T b) {
  { a <= b } -> std::convertible_to<bool>;
  { a == b } -> std::convertible_to<bool>;
};

/**
 * @concept IsTotallyOrdered
 * @brief A Partial Order where every pair is comparable.
 * @details This is the "Ruler" for our 1D road trip.
 */
export template <typename T>
concept IsTotallyOrdered =
    IsPartiallyOrdered<T> && requires(const T a, const T b) {
      { a < b } -> std::convertible_to<bool>;
      { a <=> b } -> std::same_as<std::strong_ordering>;
      requires std::three_way_comparable<T>;  // The C++20 "Naked" Proof
    };

/**
 * @concept IsLinearOrder
 * @brief Synonym for Total Order in our 1D road trip.
 */
export template <typename T>
concept IsLinearOrder = IsTotallyOrdered<T>;

/**
 * @concept IsCardinality
 * @beief The formal measure of a Set's magnitude.
 *
 * @details Cardinality defines the "how many" of a species, moving from
 *          the Empty set (Zero) through Machine-finite spaces (N64)
 *          to the Transfinite (Aleph) and the Continuum (Beth).
 *          In our Structuralist Ontology, we use a type-hierarchy
 *          to allow the compiler to reason about the size of infinity
 *          at compile-time.
 *
 * @tparam C The Cardinality species.
 *
 * Wikipedia: Cardinality, Aleph number, Beth number
 */
export template <typename C>
concept IsCardinality = IsTotallyOrdered<C> && requires(C c) {
  /** @brief The Boundedness check: Is this measure representable in the
   * discrete grid? */
  { c.is_finite() } -> std::same_as<bool>;

  /** @brief cardinality of the corresponding power set.  */
  { power(c) } -> IsTotallyOrdered;
};

/**
 * @concept IsCountable
 * @brief A Cardinality that is either Finite or Aleph-0.
 */
export template <typename C>
concept IsCountable = IsCardinality<C> {};

/** @brief Any cardinality that is NOT countable is Uncountable. */
export template <typename C>
concept IsUncountable = IsCardinality<C> && !IsCountable<C>;

/** @brief The Axiomatic Order of Regions. 
    Theorem: Any Uncountable species is strictly greater than any Countable species. */
export template <IsCardinality L, IsCardinality R>
  requires (IsCountable<L> != IsCountable<R>)
constexpr std::strong_ordering operator<=>(const L&, const R&) {
  if constexpr (IsUncountable<L>) return std::strong_ordering::greater;
  else return std::strong_ordering::less;
}

/**
 * @concept IsFinite
 * @brief A Cardinality that models a terminating Natural Number.
 *        Axiom: All Finite cardinalities are Countable.
 */
export template <typename C>
concept IsFinite = IsCountable<C> && {
};

/** 
 * @brief Any cardinality that is NOT finite is Transfinite (a Limit).
 */
export template <typename C>
concept IsTransfinite = IsCardinality<C> && !IsFinite<C>;

/** @brief The Axiomatic Order of Finitude. 
    Theorem: Any Transfinite species is strictly greater than any Finite species. */
export template <IsCardinality L, IsCardinality R>
  requires (IsFinite<L> != IsFinite<R>)
constexpr std::strong_ordering operator<=>(const L&, const R&) {
  if constexpr (IsTransfinite<L>) return std::strong_ordering::greater;
  else return std::strong_ordering::less;
}

/** 
 * @concept IsAleph0
 * @brief The unique identity of the Countably Infinite.
 *        Axiom: It is the first Transfinite but remains Countable.
 *        In the Dedekind construction, this is the magnitude of the Rationals (Q).
 */
export template <typename C>
concept IsAleph0 = IsCountable<C> && IsTransfinite<C>;

/** @concept IsDiscrete
 *  @brief A space where every point has a neighborhood containing no other points.
 *  @details Axiom: All IsNatural species are Discrete. 
 *           The Rationals (IsAleph0) are also Discrete in their atoms. */
export template <typename S>
concept IsDiscrete = IsCountable<typename S::cardinality_type>;

/** @concept IsContinuous
 *  @brief A space that is dense and complete (no gaps).
 *  @details Theorem: A Dedekind Cut transforms a Discrete space into a Continuous one. */
export template <typename S>
concept IsContinuous = IsUncountable<typename S::cardinality_type> && IsComplete<S>;

/** 
 * @concept IsContinuum
 * @brief The magnitude of the Real Numbers (R).
 *        Axiom: It is the species of 2^ℵ₀.
 */
export template <typename C>
concept IsContinuum = IsUncountable<C> && requires(C c) {
    // Theorem: IsContinuum is the species of power(Aleph0)
};

/**
 * @concept IsSet
 * @brief The fundamental species of a Collection.
 *
 * @details A Set is defined by its membership function (contains) and its
 *          measure (cardinality). In our Structuralist approach, we require
 *          the species to expose its internal types to allow the compiler
 *          to verify morphisms across the Numerical Hierarchy.
 *
 * @tparam S The Set species (the container or expression).
 * @tparam T The Element species (the members).
 *
 * Wikipedia: Set (mathematics), Axiom of extensionality
 */
export template <typename S, typename T>
concept IsSet = requires(const S s, const T v) {
  /** @brief The Membership Morphism: x ∈ S */
  { s.contains(v) } -> std::convertible_to<bool>;

  /** @brief The "Anatomy" requirements for type-safe algebraic chaining. */
  typename S::element_type;
  typename S::cardinality_type;

  /** @brief The Measure Morphism: Returns the symbolic magnitude of the set. */
  { s.cardinality() } -> std::same_as<typename S::cardinality_type>;

  /** @brief Requirement: The cardinality must be a valid Ontological
   * Cardinality. */
  requires IsCardinality<typename S::cardinality_type>;
};

/**
 * @concept IsExtensional
 * @brief A Set species that is bounded and indexable.
 * Wikipedia: Extensionality, Axiom of extensionality
 */
export template <typename S>
concept IsExtensional =
    IsSet<S, typename S::element_type> && S::cardinality().is_finite();

/**
 * @section Mereology: Pointed Species.
 * @concept IsPointed
 * @brief A species that defines its own structural origin.
 * Wikipedia: Pointed space, Origin (mathematics)
 */
export template <typename T>
concept IsPointed = requires {
  { T::origin() } -> std::same_as<T>;
};

/**
 * @concept IsPointedSet
 * @brief A Set that has a designated "Origin" or "Identity" element.
 * Wikipedia: Pointed set
 */
export template <typename S, typename T>
concept IsPointedSet = IsSet<S, T> && IsPointed<T>;

/**
 * @concept IsMeetSemiLattice
 * @brief A refinement of IsSet that supports the algebraic
 *        structure of Meet (&).
 * Wikipedia: SemiLattice (order)
 */
export template <typename S>
concept IsMeetSemilattice = requires(S a, S b) {
  { a & b } -> std::same_as<S>;  // The Great Lower Bound
};

/**
 * @concept IsJoinSemiLattice
 * @brief A refinement of IsSet that supports the algebraic
 *        structure of Join (|).
 * Wikipedia: SemiLattice (order)
 */
export template <typename S>
concept IsJoinSemilattice = requires(S a, S b) {
  { a | b } -> std::same_as<S>;  // The Least Upper Bound
};

/**
 * @concept IsLattice
 * @brief A refinement of IsSet that supports the algebraic
 *        structure of Meet (&) and Join (|).
 * Wikipedia: Lattice (order), Absorption law
 */
export template <typename S>
concept IsLattice = IsMeetSemilattice<S> && IsJoinSemilattice<S>;

/**
 * @concept IsBoundedLattice
 * @brief A Lattice with a unique "Top" (1) and "Bottom" (0).
 * @details
 * 1. Meet(a, Bottom) = Bottom
 * 2. Join(a, Top) = Top
 * Wikipedia: Bounded lattice
 */
export template <typename S>
concept IsBoundedLattice = IsLattice<S> && requires(S s) {
  { s.lower_bound() } -> std::same_as<typename S::element_type>;  // The Bottom
  { s.upper_bound() } -> std::same_as<typename S::element_type>;  // The Top
};

}  // namespace dedekind::ontology
