/**
 * @file dedekind/order/completeness.cppm
 * @partition :completeness
 * @brief Density and completeness profiles of orders.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section completeness__Scope
 * These concepts classify \emph{how} an ordered carrier fills the
 * gaps between its elements.  Together they distinguish the
 * classical ℤ / ℚ / ℝ tower:
 *
 *   - successor-isolated points ←  ℤ  (private helper `IsDiscrete`)
 *   - `IsDense`                  ←  ℚ  (a midpoint always exists)
 *   - `IsDedekindComplete`       ←  ℝ  (every non-empty bounded set has sup)
 *
 * The public concepts exported from this partition are `IsSuccessor`,
 * `IsArchimedean`, `IsDense`, `IsDividableChain`, and
 * `IsDedekindComplete`.  `IsDiscrete` is a private helper used inside
 * `IsDense`; see the declaration for the rationale.
 *
 * Wikipedia: Archimedean property, Dense order, Dedekind completeness.
 *
 * @note "Eine geordnete Menge heißt vollständig, wenn jede nicht leere
 *        und nach oben beschränkte Teilmenge eine kleinste obere
 *        Schranke besitzt."
 *       [Trans: "An ordered set is called complete if every non-empty
 *        subset bounded above has a least upper bound."]
 *       — Felix Hausdorff, *Grundzüge der Mengenlehre* (Leipzig:
 *         Veit & Co., 1914), §6. Hausdorff's foundational text on
 *         set-theoretic order and topology codifies the
 *         supremum-completeness reading the project's `IsDedekindComplete`
 *         and `IsDense` concepts pin.
 */
module;
#include <concepts>
#include <functional>

export module dedekind.order:completeness;

import dedekind.category;
import :poset;
import :total;  // IsTotallyOrdered relocated here per #410

namespace dedekind::order {
using namespace dedekind::category;

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

/**
 * @brief A set where every point is isolated by a successor.
 * @details Intentionally \emph{not} exported: `IsDiscrete` is a private
 *          helper used only inside `IsDense` below (the negation
 *          `!IsDiscrete<T>` rules out the successor-isolated case).
 *          The concept has the same scope now as before the
 *          `:concepts` → `:poset`/`:lattice`/`:completeness` split;
 *          if a downstream call site for "is this carrier discrete?"
 *          appears, promote this declaration to `export` at that
 *          point rather than pre-emptively widening the API.
 */
template <typename T>
concept IsDiscrete = IsArchimedean<T> && requires(T x) {
  // The "Unit Gap" Axiom: There is no z such that x < z < x + 1
  // In C++, we represent this by the atomic nature of the increment.
  { x + T(1) };
};

/**
 * @concept IsDense
 * @brief A property where a midpoint always exists (a < c < b).
 *
 * @details The midpoint is computed as @c (a + b) / T{2} rather than
 * @c (a + b) / 2 so that the @b carrier itself supplies the value
 * @c 2 (via the @c T{2} structural constructor), rather than relying
 * on mixed-type @c T-vs-int arithmetic.  This matters for variant
 * exact carriers (@c SignedExtensionalCardinal<>, @c Rational<...> over
 * such) which do not implicitly inter-operate with literal @c int ---
 * forcing the carrier to construct its own @c 2 keeps the concept
 * honest across both built-in (@c int, @c double) and exact carriers.
 */
export template <typename T>
concept IsDense =
    IsTotallyOrdered<T> && !IsDiscrete<T> && requires(const T a, const T b) {
      { (a + b) / T{2} } -> std::convertible_to<T>;
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

/** @section completeness__Formal_Verification */

// int carries integer division and modulo, lifting it to IsDividableChain.
static_assert(
    IsDividableChain<int>,
    "int must satisfy IsDividableChain (integer division and modulo).");

// IsDiscrete<int> is architecturally withheld: IsDiscrete requires
// IsArchimedean which requires IsSuccessor which requires IsPartialMagma<int,
// std::plus<int>>. IsPartialMagma expects Op{}(std::pair<T,T>), but
// std::plus<int> takes two separate arguments. Signed addition is also not a
// Magma (no IsTotal: signed overflow is UB). See total.cppm: !IsMagma<int,
// std::plus<int>>.

}  // namespace dedekind::order
