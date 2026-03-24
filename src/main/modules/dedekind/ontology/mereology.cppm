/**
 * @file ontology:mereology.cppm
 * @brief Level 1: The Rules of Presence (Topos-Aware Sets).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :mereology
 * @build_order 2
 * @dependency :logic, :category
 *
 * @section Mereology: The Geometry of Existence
 * This partition defines the "Body" of the Dedekind species. In the 
 * structuralist ontology, Mereology establishes the relationship between 
 * 'Parts' and 'Wholes' as a mapping between a Domain and a Logic.
 *
 * @details
 * Membership is defined as a morphism to a Subobject Classifier (Ω). 
 * By decoupling the "Body" from the "Truth," the same mereological laws 
 * are applied across different logical universes:
 * - IsSet: The universal rule-based predicate for membership.
 * - IsBooleanSet: The Classical {True, False} universe (The Binary Prime).
 * - IsKleeneSet: The Indeterminate {True, False, Unknown} universe.
 *
 * @section Structural_Anchors
 * Standard C++ operators are anchored here as Set Morphisms, 
 * lifting logical connectives into latticial operations:
 * - operator&&, operator& : Intersection (The Meet).
 * - operator||, operator| : Union (The Join).
 * - operator! : Complement (The Remainder).
 *
 * @tparam S The Set implementation type being verified.
 * @tparam L The Logic species (Ω) governing the membership predicate. 
 *           Defaults to ClassicalLogic for zero-overhead arithmetic.
 *
 * Wikipedia: Mereology, Subobject classifier, Topos theory
 */
module;

#include <compare>
#include <concepts>
#include <functional>

export module dedekind.ontology:mereology;

import :category;
import :logic;

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
 * A species fulfills IsSet if it provides a membership predicate (contains) 
 * and a declaration of its own magnitude (cardinality) relative to a 
 * specific Logic L. 
 *
 * @tparam S The Set implementation type being verified.
 * @tparam L The Logic species (The Subobject Classifier Ω) governing the 
 *           membership predicate. Defaults to ClassicalLogic (Binary).
 *
 * @section Structural_Requirements
 * Every species satisfying IsSet must define:
 * - element_type: The species of the members (The "What").
 * - cardinality_type: The species of the magnitude (The "How Many").
 * - base_set_type: The underlying algebraic or mereological origin.
 */
export template <typename S, typename L = ClassicalLogic>
concept IsSet = requires {
  /** @section Anatomy: Structural Requirements */
  typename S::element_type;
  typename S::cardinality_type;
} && requires(const S s, const typename S::element_type v) {
  /** 
   * @section Membership: The Predicate (x ∈ S) 
   * The truth value (ω) must match the expected Logic L.
   */
  { s.contains(v) } -> std::same_as<typename L::type>;

  /** @section Magnitude: The Scale */
  { s.cardinality() } -> std::same_as<typename S::cardinality_type>;
};

/** @section The_Canonical_Specializations */

/** @concept IsBooleanSet: Classical {True, False} */
export template <typename S>
concept IsBooleanSet =
    IsSet<S> && std::same_as<typename S::logic_species, ClassicalLogic>;

/** @concept IsKleeneSet: Indeterminate {True, False, Unknown} */
export template <typename S>
concept IsKleeneSet =
    IsSet<S> && std::same_as<typename S::logic_species, TernaryLogic>;

/**
 * @brief Identifies a set that is physically representable in memory (the
 * "Bucket of Data").
 *
 * A set is Extensional only if it is mathematically Finite. This
 * ensures that any set we attempt to iterate over or store as
 * a "bucket of data" has a terminating sequence.
 *
 * @tparam S A set species.
 */
export template <typename S>
concept IsExtensional = IsSet<S> && requires(S s) {
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
