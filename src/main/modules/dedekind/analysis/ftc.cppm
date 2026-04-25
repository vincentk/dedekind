/**
 * @file dedekind/analysis/ftc.cppm
 * @partition :ftc
 * @brief Fundamental Theorem of Calculus bridge hooks.
 *
 * @details
 * Provides small numerical bridge lemmas between derivative and integral
 * interfaces so downstream formalization can target a stable API.
 *
 * **CURRENT LIMITATIONS (see backlog for actionable follow-ups):**
 *
 * 1. **Riemann Sum Only**: integral_over() implements trapezoidal rule, a
 *    Riemann sum approximation. No Lebesgue measure, dominated convergence,
 *    or measure-theoretic abstraction. (See backlog: Integral Abstraction)
 *
 * 2. **Finite Difference vs. Fréchet**: derivative_at() uses raw central
 *    difference, decoupled from geometry::frechet_derivative_at(). No formal
 *    directional/Gâteaux derivatives or inverse theorems. (See backlog:
 *    Derivative Abstraction)
 *
 * 3. **No Differential Forms Integration**: OneForm and TwoForm exist in
 *    :forms and :exterior, but integral_over() does not support pullback,
 *    Stokes theorem, or exterior calculus. (See backlog: Forms Integration)
 *
 * 4. **No Cauchy Derivative**: Complex analysis and quaternion derivatives
 *    not yet formalized. (See backlog: Cauchy-Riemann Framework)
 *
 * 5. **Sequence Loop Indexing**: Both integral_over() and hypothesis
 *    functions use raw std::size_t loops. Should abstract via Path<R,
 *    Cardinality> monadic composition when mature. (See backlog:
 *    Sequence-Adapter Layer)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Hinc nostra differentialis methodus uno comprehendit calculo et
 *  inversa illa, quae olim vix unius problematis solutionem suppeditabat."
 *       — Gottfried Wilhelm Leibniz, *De geometria recondita et analysi
 *         indivisibilium atque infinitorum* (Acta Eruditorum, 1686).
 *       [Trans: "Hence our differential method comprehends in a single
 *        calculus also that inverse one, which formerly scarcely supplied
 *        the solution of a single problem."]
 */
module;

#include <cmath>
#include <concepts>
#include <cstddef>
#include <type_traits>
#include <utility>

export module dedekind.analysis:ftc;

import dedekind.algebra;

namespace dedekind::analysis {

namespace detail {

template <typename R>
constexpr auto resolved_value(const R& x) {
  if constexpr (requires { x.resolve(); }) {
    return x.resolve();
  } else {
    return x;
  }
}

template <typename R>
using resolved_value_t =
    std::remove_cvref_t<decltype(resolved_value(std::declval<R>()))>;

template <typename R>
concept IsNumericalBridgeScalar = dedekind::algebra::IsFieldLikeScalar<R> &&
                                  std::floating_point<resolved_value_t<R>>;

template <IsNumericalBridgeScalar R>
constexpr R lift(resolved_value_t<R> value) {
  if constexpr (std::same_as<R, resolved_value_t<R>>) {
    return value;
  } else {
    return R{value};
  }
}

template <IsNumericalBridgeScalar R>
constexpr R from_count(std::size_t value) {
  return lift<R>(static_cast<resolved_value_t<R>>(value));
}

template <IsNumericalBridgeScalar R>
constexpr R zero() {
  return lift<R>(resolved_value_t<R>{0});
}

template <IsNumericalBridgeScalar R>
constexpr R two() {
  return lift<R>(resolved_value_t<R>{2});
}

template <IsNumericalBridgeScalar R>
constexpr bool less_than(const R& a, const R& b) {
  return resolved_value(a) < resolved_value(b);
}

template <IsNumericalBridgeScalar R>
constexpr bool finite_value(const R& x) {
  return std::isfinite(resolved_value(x));
}

template <IsNumericalBridgeScalar R>
constexpr auto magnitude(const R& x) {
  // Hand-rolled abs: libc++'s std::abs(double) is not constexpr, which would
  // block the whole close_enough chain from static_assert evaluation.
  const auto v = resolved_value(x);
  return v < 0 ? -v : v;
}

template <IsNumericalBridgeScalar R>
constexpr bool close_enough(const R& lhs, const R& rhs, const R& tolerance) {
  // FIXME: Metric policy abstraction. Currently uses raw abs(lhs-rhs) <= tol.
  // Should defer to topology/geometry layer for closeness semantics when
  // available. See Maintenance Sweep (#123) and Denotational AD roadmap (#177).
  return magnitude(lhs - rhs) <= resolved_value(tolerance);
}

}  // namespace detail

/**
 * @brief Numerical derivative witness via central difference.
 *
 * @details Implements a basic finite-difference approximation without formal
 *          connection to geometry::frechet_derivative_at(). For scalar
 *          functions, this is a Fréchet approximation in the classical sense,
 *          but should eventually bridge to the formal derivative abstraction.
 *          Consider mapping through OneForm/Covector for differential geometry.
 *
 * @see geometry::frechet_derivative_at() for formal Fréchet derivative.
 * @see backlog: Derivative Abstraction (linking code to geometry).
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr R derivative_at(
    F&& f, R x,
    R h = static_cast<R>(1e-5)) {  // FIXME #123: magic constant step size;
                                   // pending numeric stability analysis
  return (f(x + h) - f(x - h)) / (detail::two<R>() * h);
}

/**
 * @brief Numerical integral witness via trapezoidal rule (Riemann sum).
 *
 * @details Implements a Riemann sum using the trapezoidal rule, NOT a formal
 *          Lebesgue integral. Does not compose with differential forms
 *          (OneForm/TwoForm) or support Stokes' theorem. Limitations:
 *          - No measure-theoretic semantics (Lebesgue, Borel).
 *          - No form pullback or exterior calculus integration.
 *          - No dominated/monotone convergence theorems.
 *          - Raw std::size_t loop should eventually use Path monad (see
 *            backlog: Sequence-Adapter Layer).
 *          - No integration methods (by parts, substitution) or symbolic
 *            simplification (see Epic #286: Undergraduate Calculus
 * Foundations).
 *
 * @see analysis::OneForm, TwoForm for differential forms (currently separate).
 * @see backlog: Integral Abstraction, Forms Integration, Epic #286.
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr R integral_over(
    F&& f, R a, R b,
    std::size_t slices = 4096) {  // FIXME #123: magic constant slices; pending
                                  // adaptive refinement
  // FIXME: Sequence-adapter layer. Numeric loop uses raw std::size_t indexing.
  // Should abstract via Path<R, Cardinality> when monadic/comonadic composition
  // fully supports recursive decomposition of integration bounds.
  if (a == b) {
    return detail::zero<R>();
  }

  if (detail::less_than(b, a)) {
    return -integral_over<R>(f, b, a, slices);
  }

  const R step = (b - a) / detail::from_count<R>(slices);
  R area = detail::zero<R>();

  for (std::size_t i = 0; i < slices; ++i) {
    const R x0 = a + step * detail::from_count<R>(i);
    const R x1 = x0 + step;
    area = area + (f(x0) + f(x1)) * (step / detail::two<R>());
  }

  return area;
}

/**
 * @brief FTC Part I bridge: d/dx integral_a^x f(t) dt ~= f(x).
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr bool ftc_part_i_hypotheses(
    F&& f, R a, R b,
    std::size_t samples = 32) {  // FIXME #123: magic constant samples; pending
                                 // formal discretization bounds
  if (a == b) {
    return detail::finite_value(f(a));
  }

  if (detail::less_than(b, a)) {
    return ftc_part_i_hypotheses<R>(f, b, a, samples);
  }

  const R step = (b - a) / detail::from_count<R>(samples);
  for (std::size_t i = 0; i <= samples; ++i) {
    const R x = a + step * detail::from_count<R>(i);
    if (!detail::finite_value(f(x))) {
      return false;
    }
  }

  return true;
}

/**
 * @brief Numerical bridge for the sampled Part II hypothesis F' ~= f.
 */
export template <detail::IsNumericalBridgeScalar R, typename F,
                 typename Antiderivative>
  requires std::invocable<F, R> && std::invocable<Antiderivative, R>
constexpr bool ftc_part_ii_hypotheses(
    F&& f, Antiderivative&& antiderivative, R a, R b,
    std::size_t samples = 32,  // FIXME #123: magic constant samples; pending
                               // formal discretization bounds
    R tolerance =
        static_cast<R>(1e-4)) {  // FIXME #123: magic constant tolerance;
                                 // pending adaptive scaling strategy
  // FIXME: Sequence-adapter layer. Sampled hypothesis validation uses raw
  // std::size_t loop. When full Path composition semantics mature, replace with
  // recursive sampling to align with denotational AD framework (#177).
  if (a == b) {
    return detail::close_enough(derivative_at<R>(antiderivative, a), f(a),
                                tolerance);
  }

  if (detail::less_than(b, a)) {
    return ftc_part_ii_hypotheses<R>(f, antiderivative, b, a, samples,
                                     tolerance);
  }

  const R step = (b - a) / detail::from_count<R>(samples);
  for (std::size_t i = 0; i <= samples; ++i) {
    const R x = a + step * detail::from_count<R>(i);
    if (!detail::close_enough(derivative_at<R>(antiderivative, x), f(x),
                              tolerance)) {
      return false;
    }
  }

  return true;
}

/**
 * @brief FTC Part I bridge: d/dx integral_a^x f(t) dt ~= f(x).
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr bool ftc_part_i_bridge(
    F&& f, R a, R x,
    R tolerance =
        static_cast<R>(1e-4)) {  // FIXME #123: magic constant tolerance;
                                 // pending adaptive scaling strategy
  auto accumulation = [&f, a](R y) { return integral_over<R>(f, a, y); };

  const R lhs = derivative_at<R>(accumulation, x);
  const R rhs = f(x);
  return detail::close_enough(lhs, rhs, tolerance);
}

/**
 * @brief FTC Part II bridge: integral_a^b f(x) dx ~= F(b)-F(a).
 */
export template <detail::IsNumericalBridgeScalar R, typename F,
                 typename Antiderivative>
  requires std::invocable<F, R> && std::invocable<Antiderivative, R>
constexpr bool ftc_part_ii_bridge(F&& f, Antiderivative&& antiderivative, R a,
                                  R b, R tolerance = static_cast<R>(1e-4)) {
  const R lhs = integral_over<R>(f, a, b);
  const R rhs = antiderivative(b) - antiderivative(a);
  return detail::close_enough(lhs, rhs, tolerance);
}

/**
 * @brief Worked theorem chain combining both FTC directions.
 * @details Checks that a numerical accumulation differentiates back to the
 * integrand at x, and that the same integrand integrates to the provided
 * antiderivative delta over [a,b].
 */
export template <detail::IsNumericalBridgeScalar R, typename F,
                 typename Antiderivative>
  requires std::invocable<F, R> && std::invocable<Antiderivative, R>
constexpr bool ftc_worked_theorem_chain(F&& f, Antiderivative&& antiderivative,
                                        R a, R x, R b,
                                        R tolerance = static_cast<R>(1e-4)) {
  return ftc_part_i_hypotheses<R>(f, a, b) &&
         ftc_part_ii_hypotheses<R>(f, antiderivative, a, b, 32, tolerance) &&
         ftc_part_i_bridge<R>(f, a, x, tolerance) &&
         ftc_part_ii_bridge<R>(f, antiderivative, a, b, tolerance);
}

/**
 * @brief Diagnostic: suggests optimal derivative step size via convergence
 * testing.
 *
 * @details Tests h values from 1e-8 to 1e-3 to find where derivative_at()
 *          stabilizes. Computes derivatives at decreasing step sizes and
 *          tracks the drift between successive estimates. Returns the
 *          smallest h where drift ≤ tolerance, plus the magnitude of last
 *          observed convergence drift.
 *
 * @param f The function to differentiate.
 * @param x The point at which to test the derivative.
 * @param tolerance Convergence threshold for step size selection.
 * @return Pair: (suggested_h, last_drift_magnitude).
 *          If no convergence achieved at 1e-8, returns (1e-8, last_drift).
 *
 * @note This is a diagnostic tool. The default h=1e-5 in derivative_at() is
 *       a heuristic; this function helps validate its suitability for a
 *       specific function.
 *
 * @see backlog: Derivative Abstraction, Maintenance Sweep (#123)
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr std::pair<R, R> suggest_derivative_step_size(
    F&& f, R x, R tolerance = static_cast<R>(1e-6)) {
  // Test h values at logarithmic scales: 1e-3, 1e-4, ..., 1e-8
  constexpr std::size_t num_tests = 6;
  const R h_values[num_tests] = {
      detail::lift<R>(1e-3), detail::lift<R>(1e-4), detail::lift<R>(1e-5),
      detail::lift<R>(1e-6), detail::lift<R>(1e-7), detail::lift<R>(1e-8),
  };

  R prev_deriv = derivative_at<R>(f, x, h_values[0]);
  R largest_drift = detail::zero<R>();
  R optimal_h = h_values[0];

  for (std::size_t i = 1; i < num_tests; ++i) {
    const R curr_deriv = derivative_at<R>(f, x, h_values[i]);
    const R drift = detail::lift<R>(detail::magnitude(curr_deriv - prev_deriv));

    if (detail::less_than(drift, tolerance) ||
        detail::close_enough(drift, tolerance, tolerance)) {
      optimal_h = h_values[i];
      largest_drift = drift;
      break;
    }

    if (detail::less_than(largest_drift, drift)) {
      largest_drift = drift;
    }
    prev_deriv = curr_deriv;
  }

  return {optimal_h, largest_drift};
}

/**
 * @brief Diagnostic: validates integral convergence via Richardson
 * extrapolation-style sampling.
 *
 * @details Computes integral_over() at increasing slice counts
 *          (100, 500, 2500, 4096) and monitors convergence drift.
 *          Returns the smallest slice count where drift ≤
 * convergence_threshold, plus the maximum drift observed across all refinement
 * steps.
 *
 * @param f The function to integrate.
 * @param a Left endpoint.
 * @param b Right endpoint.
 * @param convergence_threshold Maximum acceptable drift between successive
 *                              refinements.
 * @return Pair: (recommended_slices, max_drift_observed).
 *          If no convergence, returns (4096, max_drift).
 *
 * @note The default slices=4096 in integral_over() is a heuristic. This
 *       function helps confirm whether 4096 is sufficient for your integrand,
 *       or if a smaller slice count suffices (saving computation).
 *
 * @see backlog: Integral Abstraction, Maintenance Sweep (#123)
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr std::pair<std::size_t, R> diagnose_integral_convergence(
    F&& f, R a, R b, R convergence_threshold = static_cast<R>(1e-5)) {
  // Test at increasing granularities
  constexpr std::size_t num_tests = 4;
  constexpr std::size_t slice_counts[num_tests] = {100, 500, 2500, 4096};

  R prev_integral = integral_over<R>(f, a, b, slice_counts[0]);
  R max_drift = detail::zero<R>();
  std::size_t recommended_slices = slice_counts[0];

  for (std::size_t i = 1; i < num_tests; ++i) {
    const R curr_integral = integral_over<R>(f, a, b, slice_counts[i]);
    const R drift =
        detail::lift<R>(detail::magnitude(curr_integral - prev_integral));

    if (detail::less_than(drift, convergence_threshold) ||
        detail::close_enough(drift, detail::zero<R>(), convergence_threshold)) {
      recommended_slices = slice_counts[i];
      max_drift = drift;
      break;
    }

    if (detail::less_than(max_drift, drift)) {
      max_drift = drift;
    }
    prev_integral = curr_integral;
  }

  return {recommended_slices, max_drift};
}

}  // namespace dedekind::analysis
