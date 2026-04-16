/**
 * @file dedekind/numbers/mandelbrot.cppm
 * @partition :mandelbrot
 * @brief Core Mandelbrot recurrence helpers over complex carriers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Man muss immer umkehren."
 *       ("One must always invert.")
 *       -- Carl Gustav Jacob Jacobi
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.numbers:mandelbrot;

import dedekind.category;
import dedekind.geometry;
import dedekind.sequences;
import dedekind.sets;
import :complex;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::geometry;
using namespace dedekind::sequences;
using namespace dedekind::sets;

/**
 * @concept IsEscapeCriterion
 * @brief Abstractly specifies when an orbit point has "escaped" to infinity.
 *
 * An escape criterion is a callable that maps a complex number to a boolean:
 * true if the point exceeds the escape radius, false otherwise.
 *
 * Formally, it encodes a predicate P : C -> {true, false} such that
 * P(z) = true when the magnitude condition signals eventual divergence.
 */
template <typename EscapeCriterion, typename ComplexType>
concept IsEscapeCriterion =
    std::invocable<const std::decay_t<EscapeCriterion>&, const ComplexType&> &&
    std::same_as<std::invoke_result_t<const std::decay_t<EscapeCriterion>&,
                                      const ComplexType&>,
                 bool>;

/**
 * @brief Default escape criterion: Euclidean closed ball.
 *
 * Returns a predicate that is true when |z|^2 > escape_radius_squared.
 * This is the standard criterion for Mandelbrot dynamics: if |z| > 2,
 * the orbit escapes to infinity monotonically.
 */
export template <IsComplexScalar R>
constexpr auto euclidean_escape_radius_squared(R escape_radius_squared = R{4}) {
  return [escape_radius_squared](const Complex<R>& z) {
    return outside_closed_euclidean_ball_squared(escape_radius_squared)(z);
  };
}

export template <IsComplexScalar R>
using OrbitPath = Path<Complex<R>>;

export template <IsComplexScalar R>
constexpr auto mandelbrot_step(const Complex<R>& c) {
  return [c](const Complex<R>& z) { return (z * z) + c; };
}

export template <IsComplexScalar R>
constexpr auto mandelbrot_orbit(const Complex<R>& c) {
  const auto zero = partial_identity_v<Complex<R>, PartialAddComplex<R>>;
  return iterate(zero, mandelbrot_step(c));
}

/**
 * Lift an orbit-level criterion to a point-level predicate:
 *   c -> criterion(orbit(c)).
 */
export template <IsComplexScalar R, typename OrbitCriterion>
constexpr auto bounded(OrbitCriterion criterion) {
  auto orbit = arrow(mandelbrot_orbit<R>);
  auto classify = arrow(criterion);
  return orbit >> classify;
}

/**
 * Subobject view:
 *   M = { c in C | bounded(orbit(c)) }.
 */
export template <IsComplexScalar R, typename BoundedPredicate>
constexpr auto M(BoundedPredicate is_bounded) {
  auto c = var<ComplexesOf<R>>;
  return Set{c % ComplexesOf<R>{} | classify(is_bounded).χ};
}

/**
 * Test whether any point in the first max_iter steps of an orbit escapes.
 *
 * Mathematical intent:
 *   orbit_escapes(orbit, n, p) ≡ ∃ k ≤ n : p(z_k)
 *
 * The finiteness bound lives here — at the single place where we decide how
 * much evidence to inspect — rather than being threaded through every
 * intermediate function as a polymorphic Window functor.
 *
 * exists() short-circuits on the absorbing element (True), so this is
 * efficient even when the orbit escapes early.
 * Note: prefix length is max_iter + 1 to include z_max_iter (satisfying the ≤ n
 * semantics).
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr bool orbit_escapes(const OrbitPath<R>& orbit, std::size_t max_iter,
                             const EscapeCriterion& escape_criterion) {
  return exists(prefix(orbit, max_iter + 1),
                classify<Complex<R>>(escape_criterion).χ);
}

/**
 * @overload with default Euclidean escape radius (radius² = 4, i.e. |z| > 2).
 */
export template <IsComplexScalar R>
constexpr bool orbit_escapes(const OrbitPath<R>& orbit, std::size_t max_iter,
                             R escape_radius_squared = R{4}) {
  return orbit_escapes(orbit, max_iter,
                       euclidean_escape_radius_squared(escape_radius_squared));
}

/**
 * Finite-prefix orbit criterion used in executable approximations.
 *
 * Returns true iff the orbit does NOT escape within the first max_iter steps.
 *   prefix_bounded_N(orbit) ≡ ¬ ∃ k ≤ N : |z_k|² > r²
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr auto prefix_bounded_N(std::size_t max_iter,
                                EscapeCriterion escape_criterion) {
  return [max_iter, escape_criterion](const OrbitPath<R>& orbit) {
    return !orbit_escapes(orbit, max_iter, escape_criterion);
  };
}

/**
 * @overload with default Euclidean escape radius.
 */
export template <IsComplexScalar R>
constexpr auto prefix_bounded_N(std::size_t max_iter,
                                R escape_radius_squared = R{4}) {
  return prefix_bounded_N<R>(
      max_iter, euclidean_escape_radius_squared(escape_radius_squared));
}

/**
 * M_N = { c in C | prefix_bounded_N(orbit(c)) }.
 */
export template <IsComplexScalar R>
constexpr auto M_N(std::size_t max_iter, R escape_radius_squared = R{4}) {
  return M<R>(bounded<R>(prefix_bounded_N<R>(max_iter, escape_radius_squared)));
}

/**
 * Tower constructor:
 *   n |-> M_n where each stage is a computable finite-prefix approximation.
 */
export template <IsComplexScalar R>
constexpr auto M_tower(R escape_radius_squared = R{4}) {
  const auto criterion = euclidean_escape_radius_squared(escape_radius_squared);
  // Return a stage builder indexed by truncation budget n.
  return [criterion](std::size_t n) {
    // Bind the ambient parameter variable for set comprehension.
    auto c = var<ComplexesOf<R>>;
    // Build the orbit-level boundedness criterion: ¬∃k≤n : |z_k|²>r²
    auto orbit_bounded = [n, criterion](const OrbitPath<R>& orbit) {
      return !orbit_escapes(orbit, n, criterion);
    };
    // Lift the orbit-level predicate to a parameter-level predicate.
    auto parameter_bounded = bounded<R>(orbit_bounded);
    // Form the nth computable approximation set M_n.
    return Set{c % ComplexesOf<R>{} | classify(parameter_bounded).χ};
  };
}

}  // namespace dedekind::numbers
