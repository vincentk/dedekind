/**
 * @file dedekind/analysis/ftc.cppm
 * @partition :ftc
 * @brief Fundamental Theorem of Calculus bridge hooks.
 *
 * @details
 * Provides small numerical bridge lemmas between derivative and integral
 * interfaces so downstream formalization can target a stable API.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "To understand is to bridge what appears separated."
 *       -- Emmy Noether, paraphrase
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
  return std::abs(resolved_value(x));
}

template <IsNumericalBridgeScalar R>
constexpr bool close_enough(const R& lhs, const R& rhs, const R& tolerance) {
  return magnitude(lhs - rhs) <= resolved_value(tolerance);
}

}  // namespace detail

/**
 * @brief Numerical derivative witness via central difference.
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr R derivative_at(F&& f, R x, R h = static_cast<R>(1e-5)) {
  return (f(x + h) - f(x - h)) / (detail::two<R>() * h);
}

/**
 * @brief Numerical integral witness via trapezoidal rule.
 */
export template <detail::IsNumericalBridgeScalar R, typename F>
  requires std::invocable<F, R>
constexpr R integral_over(F&& f, R a, R b, std::size_t slices = 4096) {
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
constexpr bool ftc_part_i_hypotheses(F&& f, R a, R b,
                                     std::size_t samples = 32) {
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
constexpr bool ftc_part_ii_hypotheses(F&& f, Antiderivative&& antiderivative,
                                      R a, R b, std::size_t samples = 32,
                                      R tolerance = static_cast<R>(1e-4)) {
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
constexpr bool ftc_part_i_bridge(F&& f, R a, R x,
                                 R tolerance = static_cast<R>(1e-4)) {
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

}  // namespace dedekind::analysis
