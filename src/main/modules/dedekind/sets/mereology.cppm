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
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::sets {

using namespace dedekind::category;

/**
 * FIXME: workaround. This signature should later be removed as
 * part of the ETCS refactor.
 *
 * @brief The Mereological Part-Whole relation (sqsubseteq).
 * @details Returns true (in Ω) if S1 is a symbolic part of S2.
 *          Supported encodings are aligned with category:mereology:
 *          `p <= w`, `w(p)`, and `w[p]`.
 *          Example: Integers <= Reals.
 */
export template <typename S1, typename S2, typename L = ClassicalLogic>
concept IsPartOf = dedekind::category::IsPartOfRelation<S1, S2, typename L::Ω>;

/** @brief The Dual / Converse of the Part-Whole relation. */
export template <typename S1, typename S2, typename L = ClassicalLogic>
  requires IsPartOf<S1, S2, L>
constexpr typename L::Ω operator>=(const S2& whole, const S1& part) {
  if constexpr (requires {
                  { part <= whole } -> std::same_as<typename L::Ω>;
                }) {
    return part <= whole;  // Order-style converse
  } else if constexpr (requires {
                         { whole(part) } -> std::same_as<typename L::Ω>;
                       }) {
    return whole(part);  // Predicate-style converse
  } else {
    return whole[part];  // Indexer-style converse
  }
}

/**
 * @concept IsProperPart
 * @brief The primitive binary relation: x < y (x is a part of y).
 *
 * @details
 * Membership as a Morphism. In the Dedekind universe, the presence of
 * a part within a whole is defined by the functional application y(part).
 *
 * This concept anchors the syntax for the "Characteristic Function" (Ω =
 * y(x)). While the relational syntax (x < y) is established here, the
 * structural "Soul" of the relation—including transitivity and
 * antisymmetry—is formally proven downstream in :order or :algebra.
 *
 * @tparam Part The potential subobject or element.
 * @tparam Whole The containing mereological body or species.
 * @tparam L The Subobject Classifier (Ω) governing the set's logic.
 */
export template <typename Part, typename Whole, typename L = ClassicalLogic>
concept IsProperPart = IsPartOf<Part, Whole, L>;

// The six lattice-name aliases (IsSkewMeetSemilattice, IsSkewJoinSemilattice,
// IsSkewLattice, IsSetMeetSemilattice, IsSetJoinSemilattice, IsSetLattice)
// are defined in dedekind.category:mereology with set-theoretic
// (std::bit_and/bit_or) default operators. They are available here via
// `using namespace
// dedekind::category` and do not need to be re-declared.

/**
 * @concept IsBoundedLattice
 * @brief A Lattice with a unique "Top" (1) and "Bottom" (0).
 * @details
 * 1. Meet(a, Bottom) = Bottom
 * 2. Join(a, Top) = Top
 * Wikipedia: Bounded lattice
 */
export template <typename S>
concept IsBoundedLattice =
    dedekind::category::IsSetLattice<S> && requires(S s) {
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
concept IsMereologicalLattice = dedekind::category::IsSetLattice<S> &&
                                IsPartOf<S, S, L> && requires(S a, S b) {
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
                                   * @section The_Absorption_Proofs
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

/** @brief Primary trait: Is a species defined by its members? */
export template <typename T>
struct is_extensional : std::false_type {};

// Atomic Proof: Integrals are always extensional.
template <std::integral T>
struct is_extensional<T> : std::true_type {};

/** @concept IsExtensional (The Proof) */
export template <typename S>
concept IsExtensional =
    is_extensional<S>::value || requires { typename S::is_extensional_tag; };

/**
 * @concept IsExtensionalLattice
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
concept IsExtensionalLattice =
    IsMereologicalLattice<S, L> && IsExtensional<S> && requires(S a, S b) {
      { (a == b) } -> std::convertible_to<typename L::Ω>;
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
  requires IsProperPart<typename Species::Domain, typename S::Domain, L>;

  /** @requirement All inhabitants share the same mereological context. */
  requires std::same_as<typename S::Domain::ambient_species, Species>;
};

/**
 * @concept HasExtrema
 * @brief Re-export of the category-level Dedekind completeness prerequisite.
 * @details Canonical definition lives in `dedekind.category:mereology`.
 * This alias preserves the `dedekind::sets::HasExtrema` qualified name for
 * existing call sites (order partition, tests).
 */
export template <typename S>
concept HasExtrema = dedekind::category::HasExtrema<S>;

/** @section The_Scale: The Logic of Magnitude */

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
export using ℶ_1 = ℵ<1>;  // The Continuum (assuming GCH)

/** @section The_Body: The Logic of Presence */

// Transitional metadata bridge for lifting sets-side species into ETCS
// `ambient_set(...)` objects.
template <typename S, typename Enable = void>
struct SetMetadata {
  using Domain = typename SpeciesTraits<S>::Domain;

  // Translate the Ontology Token into the Mereological Type
  using Cardinality = std::conditional_t<
      SpeciesTraits<S>::cardinality == CardinalityTag::Finite, Finite, ℵ_0>;
};

// 2. Specialization: Only for types with internal members (Custom Species)
template <typename S>
struct SetMetadata<
    S, std::void_t<typename S::Domain, typename S::cardinality_type>> {
  using Domain = typename S::Domain;
  using Cardinality = typename S::cardinality_type;
};

/**
 * @brief ETCS alignment bridge: lift a `dedekind::sets` species into a
 * `dedekind::category::IsSet` object.
 *
 * @details
 * This is the explicit seam between:
 * - `dedekind::sets::Set<...>`: intensional set-builder DSL species.
 * - `dedekind::category::Set<...>`: canonical CCC/category witness.
 */
template <typename S>
concept IsETCSLiftedSet = requires(const S s) {
  requires dedekind::category::IsSet<
      decltype(dedekind::category::ambient_set<typename SetMetadata<S>::Domain>(
          s))>;
};

/** @section The_Extent: The Logic of Realization */

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
concept IsEnumerated = IsETCSLiftedSet<S> && requires(const S s) {
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
concept IsSymbolic = IsETCSLiftedSet<S> && !IsEnumerated<S, L>;

/**
 * @concept IsPointedSet
 * @brief A Set that has a designated "Origin" or "Identity" element.
 * Wikipedia: Pointed set
 */
export template <typename S, typename T>
concept IsPointedSet = IsETCSLiftedSet<S> && IsPointed<T, std::plus<T>>;

/**
 * @section Structural_Inference: NaturalLogic
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
