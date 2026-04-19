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

export module dedekind.analysis:ftc;

namespace dedekind::analysis {

/**
 * @brief Numerical derivative witness via central difference.
 */
export template <std::floating_point R, typename F>
  requires std::invocable<F, R>
constexpr R derivative_at(F&& f, R x, R h = static_cast<R>(1e-5)) {
  return (f(x + h) - f(x - h)) / (static_cast<R>(2) * h);
}

/**
 * @brief Numerical integral witness via trapezoidal rule.
 */
export template <std::floating_point R, typename F>
  requires std::invocable<F, R>
constexpr R integral_over(F&& f, R a, R b, std::size_t slices = 4096) {
  if (a == b) {
    return static_cast<R>(0);
  }

  if (b < a) {
    return -integral_over<R>(f, b, a, slices);
  }

  const R step = (b - a) / static_cast<R>(slices);
  R area = static_cast<R>(0);

  for (std::size_t i = 0; i < slices; ++i) {
    const R x0 = a + step * static_cast<R>(i);
    const R x1 = x0 + step;
    area += (f(x0) + f(x1)) * (step / static_cast<R>(2));
  }

  return area;
}

/**
 * @brief FTC Part I bridge: d/dx integral_a^x f(t) dt ~= f(x).
 */
export template <std::floating_point R, typename F>
  requires std::invocable<F, R>
constexpr bool ftc_part_i_bridge(F&& f, R a, R x,
                                 R tolerance = static_cast<R>(1e-4)) {
  auto accumulation = [&f, a](R y) { return integral_over<R>(f, a, y); };

  const R lhs = derivative_at<R>(accumulation, x);
  const R rhs = f(x);
  return std::abs(lhs - rhs) <= tolerance;
}

/**
 * @brief FTC Part II bridge: integral_a^b f(x) dx ~= F(b)-F(a).
 */
export template <std::floating_point R, typename F, typename Antiderivative>
  requires std::invocable<F, R> && std::invocable<Antiderivative, R>
constexpr bool ftc_part_ii_bridge(F&& f, Antiderivative&& antiderivative, R a,
                                  R b, R tolerance = static_cast<R>(1e-4)) {
  const R lhs = integral_over<R>(f, a, b);
  const R rhs = antiderivative(b) - antiderivative(a);
  return std::abs(lhs - rhs) <= tolerance;
}

/**
 * @brief Worked theorem chain combining both FTC directions.
 * @details Checks that a numerical accumulation differentiates back to the
 * integrand at x, and that the same integrand integrates to the provided
 * antiderivative delta over [a,b].
 */
export template <std::floating_point R, typename F, typename Antiderivative>
  requires std::invocable<F, R> && std::invocable<Antiderivative, R>
constexpr bool ftc_worked_theorem_chain(F&& f, Antiderivative&& antiderivative,
                                        R a, R x, R b,
                                        R tolerance = static_cast<R>(1e-4)) {
  return ftc_part_i_bridge<R>(f, a, x, tolerance) &&
         ftc_part_ii_bridge<R>(f, antiderivative, a, b, tolerance);
}

}  // namespace dedekind::analysis
