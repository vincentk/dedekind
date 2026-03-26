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
export template <typename S1, typename S2, typename L = ClassicalLogic>
concept IsPartOf = requires(S1 a, S2 b) {
  { a <= b } -> std::same_as<typename L::type>;
};

/** @brief The Dual / Converse of the Part-Whole relation. */
export template <typename S1, typename S2, typename L = ClassicalLogic>
  requires IsPartOf<S1, S2, L>
constexpr bool operator>=(const S2& whole, const S1& part) {
  return part <= whole;  // The Converse Morphism
}

/**
 * @concept IsProperPart
 * @brief The primitive binary relation: x < y (x is a part of y).
 *
 * @details
 * Membership as a Morphism. In the Dedekind universe, the presence of
 * a part within a whole is defined by the functional application y(part).
 *
 * This concept anchors the syntax for the "Characteristic Function" (Ω = y(x)).
 * While the relational syntax (x < y) is established here, the structural
 * "Soul" of the relation—including transitivity and antisymmetry—is
 * formally proven downstream in :order or :algebra.
 *
 * @tparam Part The potential subobject or element.
 * @tparam Whole The containing mereological body or species.
 * @tparam L The Subobject Classifier (Ω) governing the set's logic.
 */
export template <typename Part, typename Whole, typename L = ClassicalLogic>
concept IsProperPart = requires(const Part p, const Whole w) {
  /** @brief The Characteristic Function: Ω = w(p) */
  { w(p) } -> std::same_as<typename L::type>;
};

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
 * @concept IsSystem
 * @brief The mereological framework for a "Space of Parts."
 * @details
 * A System is a Lattice where every element is a 'Whole' relative to the
 * underlying Species, but a 'Part' relative to the System itself.
 */
export template <typename S, typename Species, typename L = ClassicalLogic>
concept IsSystem = IsBoundedLattice<S> && requires {
  /** @brief The inhabitant of the system (The Body). */
  typename S::element_type;

  /**
   * @requirement The inhabitant is a 'Whole' for the Species.
   * This anchors membership as the Characteristic Morphism:
   * Body(Species::element)
   */
  requires IsProperPart<typename Species::element_type,
                        typename S::element_type, L>;

  /** @requirement All inhabitants share the same mereological context. */
  requires std::same_as<typename S::element_type::ambient_species, Species>;
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

/** @section The_Scale: The Logic of Magnitude */

export template <typename C>
concept IsCardinality = requires {
  { C::is_finite } -> std::convertible_to<bool>;
  { C::is_countable } -> std::convertible_to<bool>;
  typename C::power_type;
};

/** @concept IsCountable: Magnitude is at most Aleph_0. */
export template <typename C>
concept IsCountable = IsCardinality<C> && (C::is_countable == true);

/** @concept IsUncountable: Magnitude is strictly greater than Aleph_0. */
export template <typename C>
concept IsUncountable = IsCardinality<C> && !IsCountable<C>;

/** @concept IsFiniteMagnitude: Strictly terminating. */
export template <typename C>
concept IsFiniteMagnitude = IsCountable<C> && (C::is_finite == true);

/** @struct Finite: Hardware-bound magnitude. */
export struct Finite {
  static constexpr bool is_finite = true;
  static constexpr bool is_countable = true;

  auto operator<=>(const Finite&) const = default;

  using power_type = Finite;  // Finite sets always jump to other Finite sets.
};

/** @struct ℵ: The Transfinite Ladder. */
export template <std::size_t N>
struct ℵ {
  static constexpr bool is_finite = false;
  static constexpr bool is_countable = (N == 0);
  using power_type = ℵ<N + 1>;
};

export using ℵ_0 = ℵ<0>;  // Countable Infinity
using ℶ_1 = ℵ<1>;         // The Continuum (assuming GCH)

/** @section The_Body: The Logic of Presence */

/**
 * @concept IsSet
 * @brief The Universal Morphism of Presence.
 * @tparam L The Subobject Classifier (Ω). Defaults to ClassicalLogic.
 */
export template <typename S, typename L = ClassicalLogic>
concept IsSet = IsLattice<S> && requires {
  typename S::element_type;
  typename S::cardinality_type;
  requires IsCardinality<typename S::cardinality_type> &&
               IsProperPart<typename S::element_type, S, L>;
} && requires(const S s, const typename S::element_type v) {
  { !s } -> IsLattice;  // The Complement (Remainder)
  { s.cardinality() } -> std::same_as<typename S::cardinality_type>;
};

/** @section The_Extent: The Logic of Realization */

/**
 * @concept IsExtensional
 * @brief A set whose members are materialized or bounded in memory (The
 * "Bucket").
 *
 * @details In the structuralist ontology, Extensionality implies that
 *          membership is not merely a rule (λx. P(x)) but is constrained
 *          by a physical container with a terminable address space.
 *
 * @tparam S A set species.
 * @tparam L The Subobject Classifier (Ω). Defaults to ClassicalLogic.
 */
export template <typename S, typename L = ClassicalLogic>
concept IsExtensional = IsSet<S, L> && requires(const S s) {
  /** @section Magnitude: The Physical Proof */
  // An extensional set MUST claim a Finite cardinality type.
  requires(S::cardinality_type::is_finite == true);

  /** @section Termination: The Boundedness Proof */
  // Every extensional set must define a maximum capacity (upper_bound)
  // to ensure memory-safe allocations and finite iteration.
  { s.upper_bound() } -> std::convertible_to<std::size_t>;
};

/**
 * @concept IsIntentional
 * @brief A set defined by a "Rule" or "Predicate" (λx. P(x)).
 * @details These sets (like UniversalSet or EmptySet) are not stored;
 *          they are calculated. They may be Transfinite.
 */
export template <typename S, typename L = TernaryLogic>
concept IsIntentional = IsSet<S, L> && !IsExtensional<S, L>;

/**
 * @concept IsPointedSet
 * @brief A Set that has a designated "Origin" or "Identity" element.
 * Wikipedia: Pointed set
 */
export template <typename S, typename T>
concept IsPointedSet = IsSet<S> && requires(const S s) {
  /** @brief The Morphism: Retrieve the designated basepoint. */
  { s.origin() } -> std::same_as<typename S::element_type>;
};

};  // namespace dedekind::ontology
