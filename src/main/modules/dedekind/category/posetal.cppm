/**
 * @file dedekind/category/posetal.cppm
 * @partition :posetal
 * @brief Posetal categories — categories derived from partial orders.
 *
 * @section posetal__Categorical_Definition
 * A Posetal Category is a category where for any two objects A and B, there is
 * **at most one** morphism from A to B. In this framework:
 * - Objects are elements of the set.
 * - Morphisms represent the relation (a ≤ b).
 * - Identity morphisms correspond to Reflexivity (a ≤ a).
 * - Composition corresponds to Transitivity (a ≤ b and b ≤ c implies a ≤ c).
 * - Skeletality in the category corresponds to Antisymmetry (a ≤ b and b ≤ a
 * implies a = b).
 *
 * @section posetal__Order_Structure
 * This structure corresponds to a **Partially Ordered Set (Poset)**. Unlike a
 * Preorder, a Posetal Category is skeletal, meaning isomorphic objects are
 * identical.
 *
 * Textbook defaults in this partition:
 * - Relation defaults to `std::less_equal<T>` (the canonical order witness).
 * - Logic defaults to `ClassicalLogic` (Boolean Ω).
 *
 * @quote
 * "In a sense, the most basic category is a partially ordered set;
 *  the arrows are just the instances of the order relation."
 *  — Saunders Mac Lane, *Categories for the Working Mathematician*
 *
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set
 * @see https://en.wikipedia.org/wiki/Preorder
 *
 * @tparam T The type of objects in the poset.
 * @tparam Rel The relation defining the order (the Morphism Generator).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Science, in other words, is a system of relations."
 *       -- Henri Poincare, The Value of Science (1905)
 */
module;

#include <algorithm>
#include <concepts>
#include <functional>
#include <utility>

export module dedekind.category:posetal;

import :logic;
import :mereology;

namespace dedekind::category {

/**
 * @concept IsPosetal
 * @brief A Category where morphisms are governed by a specific Logic Species
 * (Ω).
 *
 * This definition reifies the Poset as a skeletal category over a Topos L.
 * By default, it assumes Classical (Boolean) logic, but it can be
 * parameterized to support intuitionistic or fuzzy relations.
 *
 * @tparam T   The Domain (Objects).
 * @tparam Rel The Relation (Morphisms).
 * @tparam L   The Logic Species (The Subobject Classifier).
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = ClassicalLogic>
concept IsPosetal =
    IsPartialOrder<T, Rel, typename L::Ω> && requires(Rel rel, T a, T b) {
      // The relation must yield a result from the logical classifier
      { rel(a, b) } -> std::same_as<typename L::Ω>;
    };

/**
 * @concept IsTotallyOrderedPosetal
 * @brief Posetal refinement where the underlying order is total/linear.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = ClassicalLogic>
concept IsTotallyOrderedPosetal =
    IsPosetal<T, Rel, L> && IsTotalOrder<T, Rel, typename L::Ω>;

/**
 * @concept IsOrderMeetSemilattice
 * @brief Order-theoretic meet-semilattice as a commutative refinement.
 *
 * @details
 * In the module hierarchy, `:mereology` provides the upstream associative +
 * idempotent meet band. `:posetal` refines that structure with commutativity,
 * yielding the order-theoretic meet-semilattice notion.
 *
 * @note Naming history: an `IsOrderMeetSemilatticeSignature` shape mixin
 * was previously bundled into this concept's body; it was retired
 * because @c IsMereologicalMeetSemilattice transitively requires
 * @c IsMereologicalMeetMagma, which already checks the operator
 * surface (@c meet(a, b) @c -> @c convertible_to<T>).  Callsites that
 * need just the operator-surface check use the magma concept directly.
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsOrderMeetSemilattice =
    IsMereologicalMeetSemilattice<T, Meet> && IsCommutative<T, Meet>;

/**
 * @concept IsCertifiedOrderMeetSemilattice
 * @brief Trait-certified meet-semilattice (associative + idempotent +
 * commutative).
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsCertifiedOrderMeetSemilattice = IsOrderMeetSemilattice<T, Meet>;

/**
 * @concept IsOrderJoinSemilattice
 * @brief Order-theoretic join-semilattice as a commutative refinement.
 *
 * @details
 * In the module hierarchy, `:mereology` provides the upstream associative +
 * idempotent join band. `:posetal` refines that structure with commutativity,
 * yielding the order-theoretic join-semilattice notion.
 *
 * @note Naming history: an `IsOrderJoinSemilatticeSignature` shape mixin
 * was previously bundled into this concept's body; it was retired
 * because @c IsMereologicalJoinSemilattice transitively requires
 * @c IsMereologicalJoinMagma, which already checks the operator
 * surface (@c join(a, b) @c -> @c convertible_to<T>).  Callsites that
 * need just the operator-surface check use the magma concept directly.
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsOrderJoinSemilattice =
    IsMereologicalJoinSemilattice<T, Join> && IsCommutative<T, Join>;

/**
 * @concept IsCertifiedOrderJoinSemilattice
 * @brief Trait-certified join-semilattice (associative + idempotent +
 * commutative).
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsCertifiedOrderJoinSemilattice = IsOrderJoinSemilattice<T, Join>;

/**
 * @concept IsOrderLatticeOperations
 * @brief Order-theoretic lattice operations as commutative + absorptive
 * refinement over upstream mereological lattice operations.
 *
 * @note Naming history: this concept previously bundled an
 * @c IsOrderLatticeOperationsSignature signature mixin alongside the
 * semantic clauses, mixing syntax (operator surface check) with
 * semantics (axiomatic lattice claims).  The signature mixin was
 * removed (and the now-unused signature concept retired) because
 * @c IsOrderJoinSemilattice / @c IsOrderMeetSemilattice already imply
 * the operator surface through their upstream mereological magma
 * concepts (@c IsMereologicalJoinMagma / @c IsMereologicalMeetMagma).
 * The bundled concept here is purely semantic.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderLatticeOperations =
    IsMereologicalLatticeOperations<T, Join, Meet> &&
    IsOrderJoinSemilattice<T, Join> && IsOrderMeetSemilattice<T, Meet> &&
    IsAbsorptive<T, Join, Meet>;

/**
 * @concept IsCertifiedOrderLatticeOperations
 * @brief Trait-certified lattice operations (commutative semilattices +
 * absorption).
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsCertifiedOrderLatticeOperations =
    IsOrderLatticeOperations<T, Join, Meet>;

/**
 * @concept IsOrderDistributiveLatticeOperations
 * @brief Distributive lattice refinement over `IsOrderLatticeOperations`.
 *
 * @see https://en.wikipedia.org/wiki/Distributive_lattice
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderDistributiveLatticeOperations =
    IsOrderLatticeOperations<T, Join, Meet> && IsDistributive<T, Join, Meet> &&
    IsDistributive<T, Meet, Join>;

/**
 * @concept IsCertifiedOrderDistributiveLatticeOperations
 * @brief Trait-certified distributive lattice refinement.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsCertifiedOrderDistributiveLatticeOperations =
    IsOrderDistributiveLatticeOperations<T, Join, Meet> &&
    IsDistributive<T, Join, Meet> && IsDistributive<T, Meet, Join>;

using DefaultJoin = decltype(std::ranges::max);
using DefaultMeet = decltype(std::ranges::min);

/**
 * @concept IsPathProjection
 * @brief Projection contract used by `check_path` to map objects into an
 * orderable relation carrier.
 */
export template <typename Project, typename T, typename Rel, typename Ω>
concept IsPathProjection = requires(Project project, T x, Rel rel) {
  { project(x) };
  { rel(project(x), project(x)) } -> std::same_as<Ω>;
};

// Upstream/downstream alignment: posetal concepts refine mereological ones.
static_assert(IsOrderMeetSemilattice<int, DefaultMeet>);
static_assert(IsMereologicalMeetSemilattice<int, DefaultMeet>);
static_assert(IsOrderJoinSemilattice<int, DefaultJoin>);
static_assert(IsMereologicalJoinSemilattice<int, DefaultJoin>);
static_assert(IsOrderLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(IsMereologicalLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(
    IsOrderDistributiveLatticeOperations<int, DefaultJoin, DefaultMeet>);

// Compatibility aliases are intentionally locked to the refined concepts.
static_assert(IsCertifiedOrderMeetSemilattice<int, DefaultMeet>);
static_assert(IsCertifiedOrderMeetSemilattice<int, DefaultMeet> ==
              IsOrderMeetSemilattice<int, DefaultMeet>);
static_assert(IsCertifiedOrderJoinSemilattice<int, DefaultJoin>);
static_assert(IsCertifiedOrderJoinSemilattice<int, DefaultJoin> ==
              IsOrderJoinSemilattice<int, DefaultJoin>);
static_assert(IsCertifiedOrderLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(
    IsCertifiedOrderLatticeOperations<int, DefaultJoin, DefaultMeet> ==
    IsOrderLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(IsCertifiedOrderDistributiveLatticeOperations<int, DefaultJoin,
                                                            DefaultMeet>);
static_assert(
    IsCertifiedOrderDistributiveLatticeOperations<int, DefaultJoin,
                                                  DefaultMeet> ==
    IsOrderDistributiveLatticeOperations<int, DefaultJoin, DefaultMeet>);

/**
 * @brief Verify that a two-step path A→B→C exists in the posetal category.
 *
 * @details
 * This helper checks a two-edge witness `A→B` and `B→C` using the supplied
 * relation and projector. The result is expressed in the truth-value codomain
 * Ω of the chosen Logic species L, so it works uniformly for Classical,
 * Ternary, or any other pluggable logical universe — no `bool` hard-codes.
 *
 * If `Rel` also models a posetal relation for the projected carrier,
 * transitivity gives the direct edge `A→C` as a derived fact.
 *
 * @tparam T   Object type.
 * @tparam Rel Relation type used to witness each edge.
 * @tparam L   Logic species providing `AND` and the `Ω` codomain.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = ClassicalLogic, typename Project = std::identity>
  requires IsPathProjection<Project, T, Rel, typename L::Ω>
constexpr typename L::Ω check_path(T a, T b, T c, Project project = {}) {
  const auto rel = Rel{};
  // A two-step path exists iff both edges are present.
  // Transitivity (axiom of IsPosetal) guarantees the direct edge A→C.
  return L::AND(rel(project(a), project(b)), rel(project(b), project(c)));
}

static_assert(check_path<int, std::less_equal<int>>(1, 2, 3));
static_assert(!check_path<int, std::less_equal<int>>(3, 2, 1));

/**
 * @brief Opt-in drill-down projector for min/max result carriers.
 *
 * @details
 * This helper specializes the upstream `arrow_drill_down` projector from
 * `:mereology` to extract the `min` component used as an order witness.
 *
 * @tparam Whole Carrier type accepted by `arrow_drill_down(whole)` where the
 *         projected object exposes a `min` field.
 * @param whole The projected carrier.
 * @return Reference to the `min` component used as the relation witness.
 */
export template <typename Whole>
constexpr decltype(auto) arrow_drill_down_min(const Whole& whole)
  requires requires { arrow_drill_down(whole).min; }
{
  return arrow_drill_down(whole).min;
}

constexpr std::ranges::min_max_result<int> p1{1, 0};
constexpr std::ranges::min_max_result<int> p2{2, 0};
constexpr std::ranges::min_max_result<int> p3{3, 0};

static_assert(
    IsPathProjection<
        decltype(arrow_drill_down_min<const std::ranges::min_max_result<int>*>),
        const std::ranges::min_max_result<int>*, std::less_equal<int>, bool>);

static_assert(
    check_path<const std::ranges::min_max_result<int>*, std::less_equal<int>,
               ClassicalLogic>(
        &p1, &p2, &p3,
        arrow_drill_down_min<const std::ranges::min_max_result<int>*>),
    "Opt-in operator-> drill-down must preserve posetal path semantics.");

}  // namespace dedekind::category
