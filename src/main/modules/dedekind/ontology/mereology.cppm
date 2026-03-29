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

import dedekind.category;
import :logic;
import :species;

/**
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::ontology {

using namespace dedekind::category;

/**
 * @brief The Mereological Part-Whole relation (sqsubseteq).
 * @details Returns true if S1 is a symbolic part of S2.
 *          Example: Integers <= Reals.
 */
export template <typename S1, typename S2, typename L = ClassicalLogic>
// FIXME Use the concept definitions from the :species.
// FIXME This is transitive, anticommutative...
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
concept IsMeetSemilattice =
    IsSemigroupoid<S, std::bit_and<S>> && IsIdempotent<S, std::bit_and<S>>;

/**
 * @concept IsJoinSemiLattice
 * @brief A refinement of IsSet that supports the algebraic
 *        structure of Join (|).
 * Wikipedia: SemiLattice (order)
 */
export template <typename S>
concept IsJoinSemilattice =
    IsSemigroupoid<S, std::bit_or<S>> && IsIdempotent<S, std::bit_or<S>>;

/**
 * @concept IsLattice
 * @brief A refinement of IsSet that supports the algebraic
 *        structure of Meet (&) and Join (|).
 * Wikipedia: Lattice (order), Absorption law
 */
export template <typename S>
// FIXME: this has more structure than semigroup on the individual operations.
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
    IsLattice<S> && IsPartOf<S, S, L> && requires(S a, S b) {
      // 1. Consistency Axiom: (a <= b) <=> (a | b == b)
      { (a | b) == b } -> std::convertible_to<typename L::type>;

      // 2. Overlap Axiom: Meet is detectable via Initiality
      requires requires { IsInitialObject<decltype(a & b)>; };

      /**
       * @section The_Absorption_Proofs
       * These laws anchor the duality of the Monadic Push (|)
       * and the Comonadic Pull (&).
       */
      // Axiom 1: a ∪ (a ∩ b) = a
      { (a | (a & b)) == a } -> std::convertible_to<typename L::type>;

      // Axiom 2: a ∩ (a ∪ b) = a
      { (a & (a | b)) == a } -> std::convertible_to<typename L::type>;
    };

/**
 * @concept IsExtensional
 * @brief The Axiom of Identity: Wholes are identical iff they have the same
 * parts.
 *
 * @details
 * This is the 'Soul' of Set Theory. It transforms a Lattice into a
 * recognizable 'Collection'.
 *
 * Theorem: (a == b) <=> (a <= b && b <= a)
 */
export template <typename S, typename L = ClassicalLogic>
concept IsExtensional = IsMereologicalLattice<S, L> && requires(S a, S b) {
  { (a == b) } -> std::convertible_to<typename L::type>;
  // The Proof: Equality is equivalent to Mutual Parthood.
  requires requires { (a <= b && b <= a) == (a == b); };
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
    { (y <= x) } -> std::same_as<typename L::type>;
    /**
     * @theorem If (y <= x) is True, then (y == x ||
     * IsInitialObject<decltype(y)>). In C++23, we enforce this as a structural
     * requirement for species that claim to be Atomic.
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
 * @tparam Ω The Subobject Classifier (Ω). Defaults to ClassicalLogic.
 */
export template <typename S, typename Ω = ClassicalLogic>
concept IsSet = IsMereologicalLattice<S, Ω> && requires {
  typename S::element_type;
  typename S::cardinality_type;
  requires IsCardinality<typename S::cardinality_type> &&
               IsProperPart<typename S::element_type, S, Ω>;
} && requires(const S s, const typename S::element_type v) {
  { !s } -> IsLattice;  // The Complement (Remainder)
  { s.cardinality() } -> std::same_as<typename S::cardinality_type>;

  // FIXME: power set, Cartesian product, etc. should be defined here as well.
  // FIXME: Maybe even the existential and universal quantifiers as morphisms to
  // Ω.
} && IsLattice<S>;

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
concept IsEnumerated = IsSet<S, L> && requires(const S s) {
  /** @section Magnitude: The Physical Proof */
  // An extensional set MUST claim a Finite cardinality type.
  requires(S::cardinality_type::is_finite == true);

  /** @section Termination: The Boundedness Proof */
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
concept IsSymbolic = IsSet<S, L> && !IsEnumerated<S, L>;

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
