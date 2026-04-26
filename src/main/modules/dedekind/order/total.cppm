/**
 * @file dedekind/order/total.cppm
 * @partition :total
 * @brief Level 1.5b: Total-order layer — strictly stronger than the
 *        partial-order content in @c :poset.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Scope
 * Concepts that genuinely require comparability of every pair (the chain
 * axiom): @c IsTotallyOrdered, @c IsLinearOrder, @c IsStrictWeakOrder,
 * the spaceship-aware shape concept @c HasTotalOrderOperators, and the
 * heterogeneous variant @c HasTotalOrderOperatorsWith<T, U>.  All of
 * these compose @c IsPartiallyOrdered / @c HasPartialOrderOperators
 * from @c :poset; the split lets generic code that needs only the
 * partial-order content not pull in the strictly-stronger total-order
 * machinery (cf.\ @c :category:total which already houses the strict
 * total-axiomatic content for algebra; this partition is its order
 * sibling).  Per #410.
 *
 * @section Heterogeneous_Comparison_Surface
 * @c HasTotalOrderOperatorsWith<T, U> — and its partial-order sibling
 * in @c :poset — fill the seam between the homogeneous @c <=>-shape
 * (which only describes @c T @c <=> @c T) and the real-world need to
 * compare a variant carrier (e.g.\ @c SignedCardinality) with an
 * integer NTTP literal (e.g.\ from @c bound<-21>).  Names the
 * cross-type shape that the halfspace machinery silently relies on.
 * Per #415 / cross-issue note on PR #422.
 *
 * Wikipedia: Total order, Strict weak ordering, Three-way comparison
 *
 * @note "אִם אֵין אֲנִי לִי, מִי לִי?  וּכְשֶׁאֲנִי לְעַצְמִי, מָה אֲנִי?
 *        וְאִם לֹא עַכְשָׁו, אֵימָתָי?"
 *       — Hillel the Elder, Pirkei Avot 1:14.
 *       [Trans: "If I am not for myself, who will be for me?  And being
 *        only for myself, what am I?  And if not now, when?"]
 */
module;
#include <compare>  // std::three_way_comparable for HasTotalOrderOperators
#include <concepts>
#include <functional>

export module dedekind.order:total;

import dedekind.category;
import :poset;

namespace dedekind::order {

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
 * @concept HasTotalOrderOperators
 * @brief @b Pure @b syntactic @b shape: T supports the spaceship
 *        operator @c <=> alongside the four partial-order operators
 *        (the latter return @c L::Ω).
 *
 * @details
 * The C++20 three-way comparison shape: a @c <=> that returns one of
 * @c std::strong_ordering / @c std::weak_ordering / @c
 * std::partial_ordering, witnessing that @c T can act as a totally /
 * weakly ordered carrier at the @b operator level.  Composes @c
 * HasPartialOrderOperators<T, L> with the spaceship so callsites can
 * check the full relational surface in one constraint.
 *
 * Note: a partial-ordering @c <=> still satisfies this shape (the
 * spaceship's three-way result type carries the order strength); for
 * an @b axiomatic total-order witness use @c IsTotallyOrdered, which
 * additionally requires @c std::totally_ordered (excluded middle on
 * comparability).  The shape vs.\ axiom split follows the @c
 * HasRingOperators / @c IsRing pattern from PR #394.  @c L defaults to
 * @c ClassicalLogic, mirroring @c HasPartialOrderOperators / @c
 * IsPreOrdered.
 */
export template <typename T, typename L = ClassicalLogic>
concept HasTotalOrderOperators =
    HasPartialOrderOperators<T, L> && std::three_way_comparable<T>;

/**
 * @concept HasTotalOrderOperatorsWith
 * @brief @b Heterogeneous shape: T and U can be compared via @c <=> in
 *        @b both directions, alongside the four partial-order
 *        operators (the latter return @c L::Ω).
 *
 * @details
 * The cross-type spaceship-comparison shape: composes
 * @c HasPartialOrderOperatorsWith<T, U, L> with @c
 * std::three_way_comparable_with<T, U>.  Names the surface needed for
 * cross-carrier comparisons — e.g.\ @c SignedCardinality @c <=> @c int
 * (the load-bearing path for the halfspace machinery's
 * @c (x @c > @c Pivot) substitution where @c x is a variant ℤ-proxy
 * and @c Pivot is the @c int NTTP from @c bound<-21>).
 *
 * Sibling of the homogeneous @c HasTotalOrderOperators above and
 * @c HasPartialOrderOperatorsWith in @c :poset.  @c L defaults to
 * @c ClassicalLogic.  Per #415 / cross-issue note on PR #422.
 */
export template <typename T, typename U, typename L = ClassicalLogic>
concept HasTotalOrderOperatorsWith = HasPartialOrderOperatorsWith<T, U, L> &&
                                     std::three_way_comparable_with<T, U>;

/** @section Formal_Verification */

// int / unsigned int / bool: canonical totally-ordered chains, spaceship
// + the four partial-order operators all fire.
static_assert(HasTotalOrderOperators<int>);
static_assert(HasTotalOrderOperators<unsigned int>);
static_assert(HasTotalOrderOperators<bool>);

// int is the canonical totally ordered chain (axiomatic).
static_assert(IsTotallyOrdered<int>, "int must satisfy IsTotallyOrdered.");

// IsTotallyOrdered<double> is architecturally withheld: dedekind's
// is_reflexive_v<double, std::less_equal<>> is false by design because IEEE 754
// NaN violates reflexivity (NaN <= NaN is false). See species.cppm note.
// Use IsOrderedField<double> (archimedean.cppm) or std::totally_ordered<double>
// for the operational ordering witness.

// Heterogeneous total-order shape on canonical std::integral pairs.
// Same-signedness pairs witness cleanly; cross-signedness is excluded
// here because @c std::three_way_comparable_with<unsigned, signed> is
// false (the library treats the implicit narrowing in @c unsigned @c
// <=> @c signed as ill-formed), even though the four partial-order
// operators @c <, @c <=, @c >, @c >= still compile (with their
// surprising signed→unsigned conversion semantics).  See the
// homogeneous partial-order witness section in @c :poset.
static_assert(HasTotalOrderOperatorsWith<int, int>);
static_assert(HasTotalOrderOperatorsWith<int, long>);
static_assert(HasTotalOrderOperatorsWith<long, int>);

}  // namespace dedekind::order
