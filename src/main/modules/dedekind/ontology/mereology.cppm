/**
 * @file ontology:mereology.cppm
 * @brief Level 1: The Rules of Presence (Sets, Parts, and Membership).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :mereology
 * @build_order 2
 * @dependency :category
 *
 * @section Mereology: The Geometry of Composition
 * This partition defines the "Body" of our species. In the Dedekind
 * structuralist ontology, Mereology establishes the relationship between
 * 'Parts' and 'Wholes' before they are assigned a quantitative order.
 *
 * @details
 * This module defines the formal existence of containers:
 * - IsSet: The rule-based predicate for membership.
 * - UniversalSet: The Domain of Discourse (The Top element).
 * - Subset: The foundational 'inclusion' morphism.
 * - Union/Intersection: The Latticial operations (Join and Meet).
 *
 * @section Structural_Anchors
 * We anchor the C++ bitwise/logical operators here as Set Morphisms:
 * - operator&&, operator& : Intersection (The Meet).
 * - operator||, operator| : Union (The Join).
 * - operator! : Complement (The Remainder).
 *
 * Wikipedia: Mereology, Set theory, Mereotopology
 */
module;

#include <compare>
#include <concepts>
#include <functional>

export module dedekind.ontology:mereology;

import :category;

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
 * @concept IsProperPart
 * @brief The primitive binary relation: x < y (x is a part of y).
 * @details We define the syntax here. The "Soul" (transitivity, antisymmetry)
 *          is proven downstream in :order or :algebra.
 */
export template <typename Part, typename Whole>
concept IsProperPart = requires(const Part p, const Whole w) {
  /** @brief The Morphism: Is p a part of w? */
  { p.is_part_of(w) } -> std::convertible_to<bool>;
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
 * @concept IsSet
 * @brief The fundamental species of a Collection (The Rule).
 * 
 * In the Dedekind structuralist ontology, a Set is defined by its ability 
 * to provide a membership predicate (contains) and a declaration of its 
 * own magnitude (cardinality).
 *
 * @section Structural_Requirements
 * Every species satisfying IsSet must define:
 * - element_type: The species of the members (The "What").
 * - cardinality_type: The species of the magnitude (The "How Many").
 * - base_set_type: The underlying algebraic or mereological origin.
 *
 * @tparam S The Set implementation type being verified.
 */
export template <typename S>
concept IsSet = requires {
  /** @section Anatomy: Structural Requirements */
  typename S::element_type;
  typename S::cardinality_type;
  typename S::base_set_type;
} && requires(const S s, const typename S::element_type v) {
  /** @section Membership: The Predicate (x ∈ S) */
  { s.contains(v) } -> std::convertible_to<bool>;
  { s[v] } -> std::convertible_to<bool>;

  /** @section Magnitude: The Cardinality Hook */
  { s.cardinality() } -> std::same_as<typename S::cardinality_type>;

  /** @section Relation: The Underlying Structure */
  { s.base_set() } -> std::convertible_to<typename S::base_set_type>;
};

/**
 * @brief Identifies a set that is mathematically Finite
 * as opposed to an infinite or unbounded set (The "Conceptual Space").
 * @concept IsFinite
 */
export template <typename S>
concept IsFinite =
    IsSet<S> && requires(S s) {
      /** @brief True exactly if it is an extensional set. */
      { s.is_extensional() } -> std::convertible_to<bool>;
    };

/**
 * @brief Identifies a set that is physically representable in memory (the "Bucket of Data").
 *
 * A set is Extensional only if it is mathematically Finite. This
 * ensures that any set we attempt to iterate over or store as
 * a "bucket of data" has a terminating sequence.
 *
 * @tparam S A set species.
 */
export template <typename S>
concept IsExtensional =
    IsFinite<S> && requires(S s) {
      /** @brief Computable upper bound for memory-safe allocations. */
      { s.upper_bound() } -> std::convertible_to<std::size_t>;
    };

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
concept IsPointedSet = IsSet<S> && IsPointed<T>;

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

};  // namespace dedekind::ontology
