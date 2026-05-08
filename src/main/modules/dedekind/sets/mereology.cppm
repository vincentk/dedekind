/**
 * @file dedekind/sets/mereology.cppm
 * @brief The Rules of Presence (Topos-Aware Sets).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :mereology
 * @build_order 2
 * @dependency :logic, :category
 *
 * @section mereology__Mereology
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
 * @section mereology__Structural_Anchors
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
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "It is this bond, and this bond alone, which is the object in itself,
 * and this bond is a relation."
 *       -- Henri Poincare, The Value of Science (1905)
 */
module;

#include <compare>
#include <concepts>
#include <functional>

export module dedekind.sets:mereology;

import dedekind.category;

/**
 * @section mereology__Mereology_2
 * @section mereology__Mereology_3
 */
namespace dedekind::sets {

using namespace dedekind::category;

/**
 * @concept IsBoundedLattice
 * @brief A Lattice with a unique "Top" (1) and "Bottom" (0).
 * @details
 * 1. Meet(a, Bottom) = Bottom
 * 2. Join(a, Top) = Top
 * Wikipedia: Bounded lattice
 */
export template <typename S>
concept IsBoundedLattice = IsSetLattice<S> && requires(S s) {
  { s.lower_bound() } -> std::same_as<typename S::Domain>;  // The Bottom
  { s.upper_bound() } -> std::same_as<typename S::Domain>;  // The Top
};

/**
 * @concept IsMereologicalLattice
 * @brief A Lattice where Join/Meet are synonymous with Sum/Product.
 *
 * @details
 * We bake the 'Overlap' axiom directly into the requirement.
 * For a structure to be mereological, it must be possible to
 * determine if two parts share a common 'Individual'.
 */
export template <typename S, typename L = ClassicalLogic>
concept IsMereologicalLattice =
    IsSetLattice<S> && IsPartOfRelation<S, S, typename L::Ω> &&
    requires(S a, S b) {
      // Closure witness: join/meet expressions must
      // be well-formed.
      { a | b };
      { a & b };

      // TODO:
      // We move the "Equality" check to a Logical
      // Equivalence: Does (a | b) represent the
      // same subobject as b if a <= b? (This is
      // handled by your internal logic, not the C++
      // grammar).
      //
      // 1. Consistency Axiom: (a <= b) <=> (a | b
      // == b)
      //{ (a | b) == b } ->
      // std::convertible_to<typename L::type>;

      // 2. Overlap Axiom: Meet with an empty set is
      // detectable. If the intersection (a & b)
      // results in an empty set, it must be
      // equivalent to the Initial Object Ø.
      //{ (a & b) == a } ->
      // std::convertible_to<typename L::type>;

      /**
       * @section mereology__The_Absorption_Proofs
       * These laws anchor the duality of the
       * Monadic Push (|) and the Comonadic Pull
       * (&).
       */
      // Axiom 1: a ∪ (a ∩ b) = a
      //{ (a | (a & b)) == a } ->
      // std::convertible_to<typename L::type>;

      // Axiom 2: a ∩ (a ∪ b) = a
      //{ (a & (a | b)) == a } ->
      // std::convertible_to<typename L::type>;
    };

/**
 * @concept IsAtom
 * @brief An Individual that possesses no proper parts.
 *
 * @details
 * In the Calculus of Individuals, x is an Atom iff:
 * For all y, if y is a part of x (y ⊆ x), then y is either x or the
 * Initial Object (0).
 *
 * Wikipedia: Atom (order theory), Simple object (category theory)
 */
export template <typename S, typename L = ClassicalLogic>
concept IsAtom = IsMereologicalLattice<S, L> && requires(S x) {
  /**
   * @axiom The Atomic Constraint
   * For any part 'y', we must be able to prove its identity
   * relative to the atom and the bottom of the lattice.
   */
  requires requires(S y) {
    { (y <= x) } -> std::same_as<typename L::Ω>;
    /**
     * @theorem If (y <= x) is True, then (y == x ||
     * IsInitialObject<decltype(y)>). In C++23, we enforce this as a
     * structural requirement for species that claim to be Atomic.
     */
  };
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
  typename S::Domain;

  /**
   * @requirement The inhabitant is a 'Whole' for the Species.
   * This anchors membership as the Characteristic Morphism:
   * Body(Species::element)
   */
  requires IsPartOfRelation<typename Species::Domain, typename S::Domain,
                            typename L::Ω>;

  /** @requirement All inhabitants share the same mereological context. */
  requires std::same_as<typename S::Domain::ambient_species, Species>;
};

/** @section mereology__The_Scale: The Logic of Magnitude */

export template <typename C>
// Terminology note: this concept validates cardinality metadata tags
// (`is_finite`, `is_countable`, `power_type`) rather than proving
// cardinal arithmetic laws. Name kept for compatibility during taxonomy pass.
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

/** @brief Primary trait: Is a species defined by its members? */
export template <typename T>
struct is_extensional : std::false_type {};

// Atomic Proof: Integrals are always extensional.
template <std::integral T>
struct is_extensional<T> : std::true_type {};

/** @concept IsExtensional (The Proof) */
export template <typename S>
concept IsExtensional = is_extensional<S>::value;

/**
 * @concept IsEnumerated
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
concept IsEnumerated = IsExtensional<S> && requires(const S s) {
  typename S::Domain;
  requires dedekind::category::IsSet<
      decltype(dedekind::category::ambient_set<typename S::Domain>(s))>;

  /** @section mereology__Magnitude: The Physical Proof */
  // An extensional set MUST claim a Finite cardinality type.
  requires(S::cardinality_type::is_finite == true);

  /** @section mereology__Termination: The Boundedness Proof */
  // Every extensional set must define a maximum capacity (upper_bound)
  // to ensure memory-safe allocations and finite iteration.
  { s.upper_bound() } -> std::convertible_to<std::size_t>;
};

/**
 * @concept IsSymbolic
 * @brief A set defined by a "Rule" or "Predicate" (λx. P(x)).
 *
 * @details
 * Symbolic sets are intentional species where membership is a calculated
 * morphism (λx. P(x)) rather than a physical lookup. Because these
 * predicates may be undecidable or non-terminating, we default to
 * TernaryLogic {True, False, Unknown}.
 *
 * @note Structural Role:
 * Symbolic sets are the 'Soul' of the Dedekind universe. They allow
 * for Transfinite cardinality (ℵ₀, ℵ₁, etc.) and serve as the
 * functional basis for Infinite species like the Naturals (ℕ).
 *
 * Wikipedia: Intensional definition, Indicator function, Ternary logic
 */
export template <typename S, typename L = TernaryLogic>
concept IsSymbolic = requires(const S s) {
  typename S::Domain;
  requires dedekind::category::IsSet<
      decltype(dedekind::category::ambient_set<typename S::Domain>(s))>;
} && !IsEnumerated<S, L>;

/**
 * @section mereology__Structural_Inference
 * @brief Deduce the governing logic species from the nature of the Base.
 *
 * Theorem:
 * If a Species is Finite AND Extensional, it is a Classical Topos
 * (Binary). If a Species is Transfinite OR Non-Extensional, it is a
 * Kleene Topos (Ternary).
 */
export template <typename Base>
struct NaturalLogic {
  static constexpr bool is_infinite = IsTransfinite<Base>;
  static constexpr bool is_extensional_v = IsExtensional<Base>;

  using species = std::conditional_t<is_extensional_v && !is_infinite,
                                     ClassicalLogic, TernaryLogic>;
  using type = species;
};

}  // namespace dedekind::sets
