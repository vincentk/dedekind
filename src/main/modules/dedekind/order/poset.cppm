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
 * @c IsPartiallyOrdered / @c IsTotallyOrdered.
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
 * @concept HasTotalOrderOperators
 * @brief @b Pure @b syntactic @b shape: T supports the spaceship
 *        operator @c <=> alongside the four partial-order operators.
 *
 * @details
 * The C++20 three-way comparison shape: a @c <=> that returns one of
 * @c std::strong_ordering / @c std::weak_ordering / @c
 * std::partial_ordering, witnessing that @c T can act as a totally /
 * weakly ordered carrier at the @b operator level.  Composes @c
 * HasPartialOrderOperators with the spaceship so callsites can check
 * the full relational surface in one constraint.
 *
 * Note: a partial-ordering @c <=> still satisfies this shape (the
 * spaceship's three-way result type carries the order strength); for
 * an @b axiomatic total-order witness use @c IsTotallyOrdered, which
 * additionally requires @c std::totally_ordered (excluded middle on
 * comparability).  The shape vs.\ axiom split follows the @c
 * HasRingOperators / @c IsRing pattern from PR #394.
 */
export template <typename T>
concept HasTotalOrderOperators =
    HasPartialOrderOperators<T> && std::three_way_comparable<T>;

/** @section Formal_Verification */

// int / unsigned int / bool are the canonical totally-ordered carriers,
// with spaceship and the four partial-order operators.  Pin them as the
// canonical witnesses for the new shape concepts.
static_assert(HasPartialOrderOperators<int>);
static_assert(HasTotalOrderOperators<int>);
static_assert(HasPartialOrderOperators<unsigned int>);
static_assert(HasTotalOrderOperators<unsigned int>);
static_assert(HasPartialOrderOperators<bool>);
static_assert(HasTotalOrderOperators<bool>);

// int is the canonical totally ordered chain.
static_assert(IsTotallyOrdered<int>, "int must satisfy IsTotallyOrdered.");

// IsTotallyOrdered<double> is architecturally withheld: dedekind's
// is_reflexive_v<double, std::less_equal<>> is false by design because IEEE 754
// NaN violates reflexivity (NaN <= NaN is false). See species.cppm note.
// Use IsOrderedField<double> (archimedean.cppm) or std::totally_ordered<double>
// for the operational ordering witness.

}  // namespace dedekind::order
