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
    std::same_as<
        std::invoke_result_t<const std::decay_t<EscapeCriterion>&, const ComplexType&>,
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
 * Windowed escape existence check via quantifier:
 *   exists(window, escaped_criterion) checks if any orbit point escapes.
 *
 * This refactors the hand-rolled divergence counter to use the sequences DSL
 * quantifier machinery, making the escape condition explicit, composable, and
 * aligned with the exists/forall quantifier interface.
 *
 * The escape criterion is now parametrized, allowing alternative divergence
 * theories and custom escape detection strategies.
 *
 * Mathematical intent:
 *   "orbit escapes" ≡ ∃ n : escaped_criterion(z_n)
 *
 * @param window A function that selects a finite subsequence from the orbit
 * @param escape_criterion A callable that returns true when divergence is detected
 */
export template <IsComplexScalar R, typename Window, typename EscapeCriterion>
  requires std::invocable<const std::decay_t<Window>&, const OrbitPath<R>&> &&
           IsFiniteSequence<std::remove_cvref_t<std::invoke_result_t<
               const std::decay_t<Window>&, const OrbitPath<R>&>>> &&
           std::invocable<const std::decay_t<EscapeCriterion>&, const Complex<R>&> &&
           std::same_as<std::invoke_result_t<const std::decay_t<EscapeCriterion>&, const Complex<R>&>, bool>
constexpr auto divergence_count_in_window(Window&& window,
                                          EscapeCriterion&& escape_criterion) {
  return [window_fn = std::forward<Window>(window),
          escape_fn = std::forward<EscapeCriterion>(escape_criterion)](const OrbitPath<R>& orbit) {
    const auto selected_window = std::invoke(window_fn, orbit);

    // Use exists quantifier: "does any point in the window escape?"
    // Lifting escape predicate to logical map via classify().
    // Explicitly specify the domain type Complex<R> to help template deduction.
    const bool any_escaped = exists(selected_window, classify<Complex<R>>(escape_fn).χ);

    // Return count for backward compatibility:
    //   count = 1 if any_escaped, 0 otherwise.
    return any_escaped ? std::size_t{1} : std::size_t{0};
  };
}

/**
 * @overload with default Euclidean escape radius
 * Backward-compatible overload: uses the standard radius-squared = 4 criterion.
 */
export template <IsComplexScalar R, typename Window>
  requires std::invocable<const std::decay_t<Window>&, const OrbitPath<R>&> &&
           IsFiniteSequence<std::remove_cvref_t<std::invoke_result_t<
               const std::decay_t<Window>&, const OrbitPath<R>&>>>
constexpr auto divergence_count_in_window(Window&& window,
                                          R escape_radius_squared) {
  return divergence_count_in_window<R>(std::forward<Window>(window),
                                       euclidean_escape_radius_squared(escape_radius_squared));
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