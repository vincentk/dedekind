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
#include <compare>  // std::three_way_comparable for HasTotalOrderOperators
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

// IsStrictWeakOrder, IsTotallyOrdered, IsLinearOrder, HasTotalOrderOperators
// have been relocated to @c :order:total per #410 — the strictly-stronger
// total-order layer is now its own partition (mirrors @c :category:total
// which already houses the strict total-axiomatic content for algebra).
// They remain accessible via the @c dedekind.order umbrella; partition-
// scoped imports may need to add @c import @c :total directly.

/**
 * @concept HasPartialOrderOperators
 * @brief @b Pure @b syntactic @b shape: T supports the partial-order
 *        operators @c <, @c <=, @c >, @c >= with bool-convertible
 *        results.
 *
 * @details
 * Use this concept where a callsite needs the four relational operators
 * to compile and yield a value coercible to @c bool, but does @b not
 * want to bind to a particular axiomatic order (preorder, partial,
 * total).  No claim about reflexivity, antisymmetry, transitivity, or
 * comparability is made here; for those, use @c IsPreOrdered /
 * @c IsPartiallyOrdered / @c IsTotallyOrdered (the last in
 * @c :order:total).
 *
 * Sibling of @c dedekind::algebra::HasRingOperators (in @c
 * algebra:ring), @c dedekind::algebra::HasFieldOperators (in @c
 * algebra:field), and @c HasLatticeOperators (in @c order:lattice)
 * in the shape-concept family.  The split between @b shape and @b
 * axiom mirrors the literal-vs-strict tier introduced under PR #394.
 */
export template <typename T>
concept HasPartialOrderOperators = requires(const T a, const T b) {
  { a < b } -> std::convertible_to<bool>;
  { a <= b } -> std::convertible_to<bool>;
  { a > b } -> std::convertible_to<bool>;
  { a >= b } -> std::convertible_to<bool>;
};

/**
 * @concept HasPartialOrderOperatorsWith
 * @brief @b Heterogeneous shape: T and U can be compared via the
 *        four partial-order operators in @b both directions.
 *
 * @details
 * The cross-type relational-operator shape: requires @c <, @c <=,
 * @c >, @c >= to compile and yield bool-convertible results between
 * @c T and @c U values.  Names the surface needed for cross-carrier
 * comparisons — e.g.\ @c SignedCardinality @c < @c int (the
 * load-bearing path for the halfspace machinery's @c (x @c > @c
 * Pivot) substitution where @c x is the variant ℤ-proxy carrier and
 * @c Pivot is the @c int NTTP from @c bound<-21>).
 *
 * Sibling of the homogeneous @c HasPartialOrderOperators above; the
 * spaceship-aware total-order variant
 * @c HasTotalOrderOperatorsWith<T, U> lives in @c :order:total.  Per
 * #415 / cross-issue note on PR #422.
 */
export template <typename T, typename U>
concept HasPartialOrderOperatorsWith = requires(const T t, const U u) {
  { t < u } -> std::convertible_to<bool>;
  { t <= u } -> std::convertible_to<bool>;
  { t > u } -> std::convertible_to<bool>;
  { t >= u } -> std::convertible_to<bool>;
};

/** @section Formal_Verification */

// int / unsigned int / bool are the canonical carriers; the homogeneous
// partial-order shape fires on each.  Total-order witnesses live in
// @c :order:total.
static_assert(HasPartialOrderOperators<int>);
static_assert(HasPartialOrderOperators<unsigned int>);
static_assert(HasPartialOrderOperators<bool>);

// Heterogeneous partial-order shape on canonical std::integral pairs.
static_assert(HasPartialOrderOperatorsWith<int, int>);
static_assert(HasPartialOrderOperatorsWith<int, long>);
static_assert(HasPartialOrderOperatorsWith<long, int>);
static_assert(HasPartialOrderOperatorsWith<unsigned int, int>);

}  // namespace dedekind::order
