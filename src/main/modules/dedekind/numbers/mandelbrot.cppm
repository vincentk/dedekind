/**
 * @file dedekind/numbers/mandelbrot.cppm
 * @partition :mandelbrot
 * @brief Core Mandelbrot recurrence helpers over complex carriers.
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
 * Windowed divergence counter:
 *   absorbing recurrence over indicators 1_{|z_n|^2 > r^2}.
 *
 * If divergence is seen at index n, the state stays divergent for all n+k,
 * reflecting the monotone "escaped" witness used by the boundedness test.
 */
export template <IsComplexScalar R, typename Window>
  requires std::invocable<const std::decay_t<Window>&, const OrbitPath<R>&> &&
           IsFiniteSequence<std::remove_cvref_t<std::invoke_result_t<
               const std::decay_t<Window>&, const OrbitPath<R>&>>>
constexpr auto divergence_count_in_window(Window&& window,
                                          R escape_radius_squared = R{4}) {
  return [window_fn = std::forward<Window>(window),
          escape_radius_squared](const OrbitPath<R>& orbit) {
    const auto selected_window = std::invoke(window_fn, orbit);
    const auto escaped =
        outside_closed_euclidean_ball_squared(escape_radius_squared);

    // Recurrence: s_{n+1} = s_n + (1 - min(1, s_n)) * I_n
    // This makes divergence an absorbing state once any witness is found.
    std::size_t running_sum = 0;
    for (std::size_t i = 0; i < selected_window.size(); ++i) {
      if (running_sum > 0) break;
      const auto indicator =
          static_cast<std::size_t>(std::invoke(escaped, selected_window.at(i)));
      running_sum = running_sum + indicator;
    }
    return running_sum;
  };
}

/**
 * Windowed orbit criterion:
 *   bounded iff the divergence counter over that window is zero.
 */
export template <IsComplexScalar R, typename Window>
  requires std::invocable<const std::decay_t<Window>&, const OrbitPath<R>&> &&
           IsFiniteSequence<std::remove_cvref_t<std::invoke_result_t<
               const std::decay_t<Window>&, const OrbitPath<R>&>>>
constexpr auto bounded_in_window(Window&& window,
                                 R escape_radius_squared = R{4}) {
  auto divergence_count =
      divergence_count_in_window<R>(std::forward<Window>(window),
                                    escape_radius_squared);
  return [divergence_count](const OrbitPath<R>& orbit) {
    return divergence_count(orbit) == 0;
  };
}

/**
 * Finite-prefix orbit criterion used in executable approximations:
 * prefix_bounded_N(orbit(c)).
 */
export template <IsComplexScalar R>
constexpr auto prefix_bounded_N(std::size_t max_iter,
                                R escape_radius_squared = R{4}) {
  auto prefix_window = [max_iter](const OrbitPath<R>& orbit) {
    return prefix(orbit, max_iter);
  };
  return bounded_in_window<R>(prefix_window, escape_radius_squared);
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
  // Fix the Mandelbrot recurrence once as c |-> orbit(c).
  auto phi = mandelbrot_orbit<R>;
  // Return a stage builder indexed by truncation budget n.
  return [phi, escape_radius_squared](std::size_t n) {
    // Bind the ambient parameter variable for set comprehension.
    auto c = var<ComplexesOf<R>>;
    // Select the first n orbit points as the finite evidence window.
    auto window = [n](const OrbitPath<R>& orbit) { return prefix(orbit, n); };
    // Build the orbit-level boundedness criterion on that window.
    auto orbit_bounded = bounded_in_window<R>(window, escape_radius_squared);
    // Lift the orbit-level predicate to a parameter-level predicate.
    auto parameter_bounded = bounded<R>(orbit_bounded);
    // Form the nth computable approximation set M_n.
    return Set{c % ComplexesOf<R>{} | classify(parameter_bounded).χ};
  };
}

}  // namespace dedekind::numbers