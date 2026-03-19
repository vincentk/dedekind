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

import :cardinalities;
import :order;

/**
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::ontology {

/**
 * @brief The Mereological Part-Whole relation (sqsubseteq).
 * @details Returns true if S1 is a symbolic part of S2.
 *          Example: Integers <= Reals.
 */
export template <typename S1, typename S2>
concept IsPartOf = requires(S1 a, S2 b) {
  { a <= b } -> std::convertible_to<bool>;
};

/**
 * @brief The Parthood Relation (sqsubseteq).
 * @details A <= B returns true if A is a part of B.
 *          In our ontology, this is a "Sub-Species" check.
 *
 * @tparam Part The potential part.
 * @tparam Whole The encompassing whole.
 * @return constexpr bool True if Part is a sub-species or subset of Whole.
 */
template <typename Part, typename Whole>
  requires IsSpecies<Part> && IsSpecies<Whole>
constexpr auto operator<=(const Part& a, const Whole& b) -> bool {
  // 1. Structural Identity: Is Part a literal C++ sub-type of Whole?
  if constexpr (std::derived_from<Part, Whole>) return true;

  // 2. Cardinality Constraint: A part cannot be "larger" than its whole.
  if constexpr (Part::cardinality() > Whole::cardinality()) return false;

  // 3. Predicate Inclusion: Is the 'rule' of A a stricter version of B?
  // This is the "Symbolic" check we use for Dedekind Cuts.
  return proves_inclusion(a, b);
}

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
concept IsCountable = IsCardinality<C>;

/** @brief Any cardinality that is NOT countable is Uncountable. */
export template <typename C>
concept IsUncountable = IsCardinality<C> && !IsCountable<C>;

/** @brief The Axiomatic Order of Regions.
    Theorem: Any Uncountable species is strictly greater than any Countable
   species. */
export template <IsCardinality L, IsCardinality R>
  requires(IsCountable<L> != IsCountable<R>)
constexpr std::strong_ordering operator<=>(const L&, const R&) {
  if constexpr (IsUncountable<L>)
    return std::strong_ordering::greater;
  else
    return std::strong_ordering::less;
}

/**
 * @brief Any cardinality that is NOT finite is Transfinite (a Limit).
 */
export template <typename C>
concept IsTransfinite = IsCardinality<C>;

/**
 * @brief Mathematical finiteness defined via Countability.
 *
 * A species is Finite if it is Countable and its cardinality
 * is strictly less than Aleph-0. This property allows for
 * exact counting and symbolic reduction.
 *
 * @tparam C A cardinality species.
 */
template <typename C>
concept IsFinite = IsCountable<C> && !IsTransfinite<C>;

/** @brief The Axiomatic Order of Finitude.
    Theorem: Any Transfinite species is strictly greater than any Finite
   species. */
export template <IsCardinality L, IsCardinality R>
  requires(IsFinite<L> != IsFinite<R>)
constexpr std::strong_ordering operator<=>(const L&, const R&) {
  if constexpr (IsTransfinite<L>)
    return std::strong_ordering::greater;
  else
    return std::strong_ordering::less;
}

/**
 * @concept IsAleph0
 * @brief The unique identity of the Countably Infinite.
 *        Axiom: It is the first Transfinite but remains Countable.
 *        In the Dedekind construction, this is the magnitude of the Rationals
 * (Q).
 */
export template <typename C>
concept IsAleph0 = IsCountable<C> && IsTransfinite<C>;

/**
 * @brief Identifies a set that is physically representable in memory.
 *
 * A set is Extensional only if it is mathematically Finite. This
 * ensures that any set we attempt to iterate over or store as
 * a "bucket of data" has a terminating sequence.
 *
 * @tparam S A set species.
 */
export template <typename S>
concept IsExtensional =
    IsFinite<typename S::cardinality_type> && requires(S s) {
      /** @brief Computable upper bound for memory-safe allocations. */
      { s.upper_bound() } -> std::convertible_to<std::size_t>;
    };

/**
 * @brief The Existence of Extreme Bounds.
 *
 * This concept governs the species' ability to find a Supremum (Least Upper
 * Bound) or Infimum (Greatest Lower Bound). This is the functional requirement
 * for Dedekind Completeness.
 *
 * @tparam S A set species.
 */
template <typename S>
concept HasExtrema = requires(S s) {
  /** @brief Computes the least upper bound of a bounded subset. */
  { s.supremum() };
  /** @brief Computes the greatest lower bound of a bounded subset. */
  { s.infimum() };
};

/**
 * @brief The property of a Continuous field.
 *
 * A species is Continuous if its size is that of the Continuum (Beth-1)
 * and it satisfies the Dedekind-Completeness axiom (HasExtrema).
 * This distinguishes the "Smooth" Real line from the "Holey" Rational field.
 *
 * @tparam S A set species.
 */
template <typename S>
concept IsContinuous =
    IsUncountable<typename S::cardinality_type> && HasExtrema<S>;

}

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
 * @concept IsContinuous
 * @brief A space that is both dense and complete (R).
 * @details A species is Continuous if it possesses the cardinality
 *          of the Continuum (Beth-1) and satisfies the Dedekind-Completeness
 *          axiom.
 *
 * @theorem The Dedekind Cut transforms a Dense, Countable field (Q)
 *          into a Continuous one (R).
 */
export template <typename S>
concept IsContinuous = IsUncountable<S> && IsDedekindComplete<S>;
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
 * @brief The fundamental species of a Collection (The Rule).
 * 
 * @tparam S The Set species (the predicate/expression).
 * @tparam C The Cardinality (The Magnitude).
 * @tparam T The Element species (The "What").
 */
export template <typename S, typename C, typename T = typename S::element_type>
concept IsSet = 
    IsCardinality<C> &&
    requires(const S s, const T v) {
        /** @brief The Membership Predicate: x ∈ S */
        { s.contains(v) } -> std::convertible_to<bool>;

        /** @brief The "Anatomy" for type-safe chaining. */
        typename S::element_type;
        typename S::cardinality_type;

        /** @brief Magnitude Matching: The claim C must match the implementation. */
        { s.cardinality() } -> std::same_as<C>;
        requires std::same_as<C, typename S::cardinality_type>;
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

/**
 * @brief The Least Upper Bound (LUB) of a Species.
 * @details A refinement of IsJoinSemilattice that extends the Join (|)
 *          operation to entire bounded subsets.
 * @see Wikipedia: Completeness of the real numbers
 */
export template <typename T>
concept IsDedekindComplete =
    IsOrderedField<T> && IsDense<T> && IsLattice<T> && requires(T a) {
      /** @brief The Supremum Morphism: Finding the 'ceiling' of a bounded set.
       */
      { supremum_of(a) } -> std::same_as<T>;
    };

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

}  // namespace dedekind::ontology
