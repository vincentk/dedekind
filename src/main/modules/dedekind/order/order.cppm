/**
 * @file ontology:order.cppm
 * @brief Level 1.5: The Rules of Relation (Posets, Lattices, and Chains).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Order: The Geography of Sets
 * This partition establishes "Relative Positioning" before Magnitude.
 * Order is the prerequisite for Cardinality; we define "Comparison"
 * before we define "Count".
 *
 * @section Structural_Synthesis
 * - Pre-Order: Reflexive and Transitive.
 * - Directed Set: A Pre-Order with a Join-Semilattice (Convergence).
 * - Directed Poset: A Directed Set with Antisymmetry (Identity).
 * - Total Order: A Poset with no "Parallelism" (The Chain).
 *
 * Wikipedia: Order theory, Directed set, Partial order, Strict weak ordering
 *
 * @note "כל עגול אין לו ראשית, רק כרצון איש ואיש."
 *       — אברהם אבן עזרא, Hebrew Wikiquote.
 *       [Trans: "A circle has no beginning, except as each person chooses."]
 */
module;
#include <algorithm>
#include <concepts>
#include <functional>

export module dedekind.order;

import dedekind.category;
import dedekind.sets;

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

static_assert(dedekind::category::IsSet<
          decltype(dedekind::category::ambient_set<int>(Ω<int>{}))>,
        "dedekind.order is grounded on canonical ETCS set objects imported from dedekind.sets.");

/**
 * @concept IsPreOrdered
 * @brief The most basic relation: Reflexive and Transitive.
 */
export template <typename T, typename L = ClassicalLogic>
concept IsPreOrdered =
    IsPartOf<T, T, L> && is_reflexive_v<T, std::less_equal<>> &&
    is_transitive_v<T, std::less_equal<>>;

/**
 * @concept IsDirectedSet
 * @brief A Pre-ordered structure where any two elements have a common
 * successor.
 * @details Synthesized via the order Join-Semilattice (posetal
 *          `IsCertifiedOrderJoinSemilattice<D, Join>`). The `Join` parameter
 *          defaults to `std::ranges::max` but can be overridden to model
 *          directed sets under a different certified join operation.
 *          This is the "Ground" for all Nets and Sequences.
 */
export template <typename D, typename L = ClassicalLogic,
                 typename Join = decltype(std::ranges::max)>
concept IsDirectedSet =
    IsPreOrdered<D, L> &&
    dedekind::category::IsCertifiedOrderJoinSemilattice<D, Join>;

/**
 * @concept IsPartiallyOrdered
 * @brief A refinement of the Pre-Order that satisfies Antisymmetry (Identity).
 */
export template <typename T, typename L = ClassicalLogic>
concept IsPartiallyOrdered =
    IsPreOrdered<T, L> && is_antisymmetric_v<T, std::less_equal<>> &&
    requires(const T a, const T b) {
      { a == b } -> std::same_as<typename L::Ω>;
    };

/**
 * @concept IsDirectedPoset
 * @brief The "Convergent Identity": A Poset that is also a Directed Set.
 */
export template <typename T, typename L = ClassicalLogic>
concept IsDirectedPoset = IsPartiallyOrdered<T, L> && IsDirectedSet<T, L>;

/**
 * @concept IsStrictWeakOrder
 * @brief The standard C++ 'Compare' requirement (e.g., for std::sort).
 * @details A relation < where incomparability is transitive.
 */
export template <typename T>
concept IsStrictWeakOrder = std::strict_weak_order<std::less<T>, T, T>;

/**
 * @concept IsTotallyOrdered
 * @brief A refinement where every pair is comparable (The Chain).
 */
export template <typename T>
concept IsTotallyOrdered = IsPartiallyOrdered<T> && std::totally_ordered<T>;

/** @brief Synonym for Total Order. */
export template <typename T>
concept IsLinearOrder = IsTotallyOrdered<T>;

/**
 * @concept IsOrderMeetSemilattice
 * @brief Re-export the certified meet-semilattice stage from `:posetal`.
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsOrderMeetSemilattice =
    dedekind::category::IsCertifiedOrderMeetSemilattice<T, Meet>;

/**
 * @concept IsOrderJoinSemilattice
 * @brief Re-export the certified join-semilattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsOrderJoinSemilattice =
    dedekind::category::IsCertifiedOrderJoinSemilattice<T, Join>;

/**
 * @concept IsOrderLattice
 * @brief Re-export the certified lattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderLattice =
    dedekind::category::IsCertifiedOrderLatticeOperations<T, Join, Meet>;

/**
 * @concept IsOrderDistributiveLattice
 * @brief Re-export the certified distributive lattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderDistributiveLattice =
    dedekind::category::IsCertifiedOrderDistributiveLatticeOperations<T, Join,
                                                                      Meet>;

/**
 * @concept IsSuccessor
 * @brief The algebraic S: T -> T mapping via the Unit element.
 */
export template <typename T>
concept IsSuccessor = IsPartialMagma<T, std::plus<T>> &&
                      IsPointed<T, std::multiplies<T>> && requires(const T x) {
                        // The Successor Morphism: S(x) = x + 1
                        {
                          x + identity_v<T, std::multiplies<T>>
                        } -> std::same_as<T>;
                      };

/**
 * @concept IsArchimedean
 * @brief The scale axiom: Every element can be 'reached' by the Successor.
 */
export template <typename T>
concept IsArchimedean = IsPartiallyOrdered<T> && IsSuccessor<T>;

/** @brief A set where every point is isolated by a successor. */
template <typename T>
concept IsDiscrete = IsArchimedean<T> && requires(T x) {
  // The "Unit Gap" Axiom: There is no z such that x < z < x + 1
  // In C++, we represent this by the atomic nature of the increment.
  { x + T(1) };
};

/**
 * @concept IsDense
 * @brief A property where a midpoint always exists (a < c < b).
 */
export template <typename T>
concept IsDense =
    IsTotallyOrdered<T> && !IsDiscrete<T> && requires(const T a, const T b) {
      { (a + b) / 2 } -> std::convertible_to<T>;
    };

/**
 * @concept IsDividableChain
 * @brief A Totally Ordered species with a partitioning algorithm (/).
 */
export template <typename T>
concept IsDividableChain = IsTotallyOrdered<T> && requires(T a, T b) {
  { a / b } -> std::same_as<T>;
  { a % b } -> std::same_as<T>;
};

/**
 * @concept IsDedekindComplete
 * @brief The topological "Soul" of the Continuum (LUB property).
 */
export template <typename S>
concept IsDedekindComplete =
    IsTotallyOrdered<S> && IsDense<S> && dedekind::category::HasExtrema<S>;

}  // namespace dedekind::order
