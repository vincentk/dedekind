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

import dedekind.sequences;
import dedekind.sets;
import :complex;

namespace dedekind::numbers {
using namespace dedekind::sequences;
using namespace dedekind::sets;

export template <IsComplexScalar R>
constexpr auto mandelbrot_step(const Complex<R>& c) {
  return [c](const Complex<R>& z) { return (z * z) + c; };
}

export template <IsComplexScalar R>
constexpr auto mandelbrot_orbit(const Complex<R>& c) {
  return iterate(Complex<R>{}, mandelbrot_step(c));
}

/**
 * The complete orbit for every point in the complex plane.
 */
export template <IsComplexScalar R>
constexpr auto mandelbrot_orbits() {
  return [](const Complex<R>& c) { return mandelbrot_orbit(c); };
}

/**
 * Abstract Mandelbrot set constructor: keeps the mathematical definition
 * separate from the chosen orbit criterion.
 */
export template <IsComplexScalar R, typename OrbitCriterion>
  requires std::predicate<const OrbitCriterion&, const Path<Complex<R>>&>
constexpr auto mandelbrot_set(OrbitCriterion criterion) {
  using Species = ComplexesOf<R>;
  auto c = var<Species>;
  return Set{c % Species{} |
             [criterion = std::move(criterion)](const Complex<R>& parameter) {
               return std::invoke(criterion, mandelbrot_orbit(parameter));
             }};
}

/**
 * Finite-prefix orbit criterion used in executable approximations:
 * prefix_bounded_N(orbit(c)).
 */
export template <IsComplexScalar R>
constexpr auto prefix_bounded_N(std::size_t max_iter,
                                R escape_radius_squared = R{4}) {
  return [max_iter, escape_radius_squared](const Path<Complex<R>>& orbit) {
    const auto window = prefix(orbit, max_iter);
    return count_if(window, [escape_radius_squared](const Complex<R>& z) {
             const R re = z.real();
             const R im = z.imag();
             return ((re * re) + (im * im)) > escape_radius_squared;
           }) == 0;
  };
}

/**
 * Alias for M_N = { c in C | prefix_bounded_N(orbit(c)) }.
 */
export template <IsComplexScalar R>
constexpr auto mandelbrot_set_prefix_bounded_N(std::size_t max_iter,
                                               R escape_radius_squared = R{4}) {
  return mandelbrot_set<R>(
      prefix_bounded_N<R>(max_iter, escape_radius_squared));
}

/**
 * Executable approximation of Mandelbrot membership over a finite prefix.
 * If no point in the first max_iter states escapes radius 2 (squared radius
 * 4), c is treated as a member.
 */
export template <IsComplexScalar R>
constexpr auto mandelbrot_set_prefix_bounded(std::size_t max_iter,
                                             R escape_radius_squared = R{4}) {
  return mandelbrot_set_prefix_bounded_N<R>(max_iter, escape_radius_squared);
}

}  // namespace dedekind::numbers