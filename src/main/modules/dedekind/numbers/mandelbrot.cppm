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
import dedekind.sequences;
import dedekind.sets;
import :complex;

namespace dedekind::numbers {
using namespace dedekind::category;
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
  return iterate(Complex<R>{}, mandelbrot_step(c));
}

/**
 * The complete orbit for any point in the complex plane.
 */
export template <IsComplexScalar R>
constexpr auto mandelbrot_orbits() {
  return [](const Complex<R>& c) { return mandelbrot_orbit(c); };
}

/**
 * Lift an orbit-level criterion to a point-level predicate:
 *   c -> criterion(orbit(c)).
 */
export template <IsComplexScalar R, typename OrbitCriterion>
constexpr auto bounded(OrbitCriterion criterion) {
  auto orbit = arrow(mandelbrot_orbits<R>());
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
    return count_if(window, [escape_radius_squared](const Complex<R>& z) {
             const R re = z.real();
             const R im = z.imag();
             return ((re * re) + (im * im)) > escape_radius_squared;
           }) == 0;
  };
}

/**
 * M_N = { c in C | prefix_bounded_N(orbit(c)) }.
 */
export template <IsComplexScalar R>
constexpr auto M_N(std::size_t max_iter, R escape_radius_squared = R{4}) {
  return M<R>(bounded<R>(prefix_bounded_N<R>(max_iter, escape_radius_squared)));
}

/**
 * Backward-compatible alias for criterion-based Mandelbrot set construction.
 */
export template <IsComplexScalar R, typename OrbitCriterion>
constexpr auto mandelbrot_set(OrbitCriterion criterion) {
  const auto orbit_is_bounded = bounded<R>(std::move(criterion));
  return M<R>(orbit_is_bounded);
}

/**
 * Executable approximation of Mandelbrot membership over a finite prefix.
 * If no point in the first max_iter states escapes radius 2 (squared radius
 * 4), c is treated as a member.
 */
export template <IsComplexScalar R>
constexpr auto mandelbrot_set_prefix_bounded(std::size_t max_iter,
                                             R escape_radius_squared = R{4}) {
  return M_N<R>(max_iter, escape_radius_squared);
}

}  // namespace dedekind::numbers