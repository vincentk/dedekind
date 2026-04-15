/**
 * @file dedekind/numbers/mandelbrot.cppm
 * @partition :mandelbrot
 * @brief Core Mandelbrot recurrence helpers over complex carriers.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
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
 * Finite-prefix orbit criterion used in executable approximations:
 * prefix_bounded_N(orbit(c)).
 */
export template <IsComplexScalar R>
constexpr auto prefix_bounded_N(std::size_t max_iter,
                                R escape_radius_squared = R{4}) {
  return [max_iter, escape_radius_squared](const OrbitPath<R>& orbit) {
    const auto window = prefix(orbit, max_iter);
    return !exists(window,
                   outside_closed_euclidean_ball_squared(escape_radius_squared));
  };
}

/**
 * M_N = { c in C | prefix_bounded_N(orbit(c)) }.
 */
export template <IsComplexScalar R>
constexpr auto M_N(std::size_t max_iter, R escape_radius_squared = R{4}) {
  return M<R>(bounded<R>(prefix_bounded_N<R>(max_iter, escape_radius_squared)));
}

}  // namespace dedekind::numbers