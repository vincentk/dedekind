/**
 * @file dedekind/order/poset.cppm
 * @partition :poset
 * @brief Level 1.5: Relation concepts --- preorders, partial orders,
 *        directed sets, and chains.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Scope
 * The most basic concepts in `dedekind.order` live here: species
 * classified by the \emph{relation} they carry (reflexivity,
 * transitivity, antisymmetry, directedness, comparability).  Lattice
 * structures (meet/join/distributive) live in `:lattice`; density
 * and completeness profiles (Archimedean, Dedekind-complete, ...)
 * live in `:completeness`.
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

export module dedekind.order:poset;

import dedekind.category;
import dedekind.sets;

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

// Grounding assertion for the whole `dedekind.order` module: the
// order concepts are built on top of the ETCS set objects exported
// from `dedekind.sets`.  It fires once when any order partition is
// imported (via the umbrella), since `:poset` is the foundational
// partition the other order partitions build on.
static_assert(dedekind::category::IsSet<
                  decltype(dedekind::category::ambient_set<int>(Ω<int>{}))>,
              "dedekind.order is grounded on canonical ETCS set objects "
              "imported from dedekind.sets.");

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

/** @section Formal_Verification */

// int is the canonical totally ordered chain.
static_assert(IsTotallyOrdered<int>, "int must satisfy IsTotallyOrdered.");

// IsTotallyOrdered<double> is architecturally withheld: dedekind's
// is_reflexive_v<double, std::less_equal<>> is false by design because IEEE 754
// NaN violates reflexivity (NaN <= NaN is false). See species.cppm note.
// Use IsOrderedField<double> (archimedean.cppm) or std::totally_ordered<double>
// for the operational ordering witness.

}  // namespace dedekind::order
