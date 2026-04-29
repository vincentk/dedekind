/**
 * @file dedekind/ieee/approx.cppm
 * @brief Approximation policies for the IEEE effect context.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Begin with the simplest examples."
 *       -- David Hilbert, quoted in Hilbert-Courant (1984)
 */
module;

#include <cmath>
#include <concepts>
#include <limits>
#include <utility>

export module dedekind.ieee.approx;

import dedekind.category;
import dedekind.ieee;

namespace dedekind::ieee {
using namespace dedekind::category;

/** @brief Region tags for simple runtime numeric diagnostics. */
export enum class NumericRegion { Regular, NearZero, Huge, NonFinite };

/** @brief Approximation envelope for an IEEE value. */
export template <std::floating_point F = double>
struct Approx {
  IEEE<F> value{};
  F abs_error{};
  F rel_error{};
  NumericRegion region = NumericRegion::Regular;
};

/** @brief Classify a scalar into a numeric region. */
export template <std::floating_point F = double>
constexpr NumericRegion classify_region(F x, F near_zero_scale = F{64},
                                        F huge_scale = F{0.5}) noexcept {
  if (!std::isfinite(x)) {
    return NumericRegion::NonFinite;
  }

  const F ax = std::fabs(x);
  const F near_zero = near_zero_scale * std::numeric_limits<F>::epsilon();
  const F huge = huge_scale * std::numeric_limits<F>::max();

  if (ax <= near_zero) {
    return NumericRegion::NearZero;
  }

  if (ax >= huge) {
    return NumericRegion::Huge;
  }

  return NumericRegion::Regular;
}

/** @brief Conservative local rounding proxy: eps*|x|. */
export template <std::floating_point F = double>
constexpr F roundoff_proxy(F x) noexcept {
  return std::numeric_limits<F>::epsilon() * std::fabs(x);
}

/**
 * @brief Category-grounded contract for propagation operation tokens.
 */
export template <typename Op, typename F>
concept IsIEEEPropagationOp =
    std::floating_point<F> && IsAssociative<IEEE<F>, Op> &&
    requires(const Op& op, const IEEE<F>& x, const IEEE<F>& y) {
      { op({x, y}) } -> std::same_as<IEEE<F>>;
      { Op::sensitivity(x, y) } -> std::same_as<std::pair<F, F>>;
    };

/** @brief Report policy: keep value and attach an error envelope. */
export template <std::floating_point F = double>
struct ReportErrorPolicy {
  constexpr Approx<F> operator()(const IEEE<F>& x) const noexcept {
    const F v = x.resolve();
    if (!std::isfinite(v)) {
      return Approx<F>{.value = x,
                       .abs_error = std::numeric_limits<F>::infinity(),
                       .rel_error = std::numeric_limits<F>::infinity(),
                       .region = NumericRegion::NonFinite};
    }
    const F abs = roundoff_proxy(v);
    const F denom = std::fabs(v) + std::numeric_limits<F>::min();
    return Approx<F>{.value = x,
                     .abs_error = abs,
                     .rel_error = abs / denom,
                     .region = classify_region(v)};
  }
};

/** @brief Ignore policy: run fast lane and drop all diagnostics. */
export template <std::floating_point F = double>
struct IgnoreErrorPolicy {
  constexpr IEEE<F> operator()(const IEEE<F>& x) const noexcept { return x; }
};

/** @brief Adaptive policy: respond to risky regions via ternary status. */
export template <std::floating_point F = double>
struct AdaptiveErrorPolicy {
  constexpr TernaryResult<Approx<F>> operator()(
      const IEEE<F>& x) const noexcept {
    const auto report = ReportErrorPolicy<F>{}(x);

    if (report.region == NumericRegion::NonFinite) {
      return TernaryResult<Approx<F>>{.status = Ternary::False,
                                      .value = report};
    }

    if (report.region == NumericRegion::NearZero ||
        report.region == NumericRegion::Huge) {
      return TernaryResult<Approx<F>>{.status = Ternary::Unknown,
                                      .value = report};
    }

    return TernaryResult<Approx<F>>{.status = Ternary::True, .value = report};
  }
};

/** @brief Propagate approximation error through a binary IEEE operation. */
export template <std::floating_point F, typename Op>
  requires IsIEEEPropagationOp<Op, F>
constexpr Approx<F> propagate(const Approx<F>& a, const Approx<F>& b,
                              const Op& op = {}) noexcept {
  const IEEE<F> result = op({a.value, b.value});
  const F rv = result.resolve();
  if (!std::isfinite(rv)) {
    return Approx<F>{.value = result,
                     .abs_error = std::numeric_limits<F>::infinity(),
                     .rel_error = std::numeric_limits<F>::infinity(),
                     .region = NumericRegion::NonFinite};
  }
  const auto [sa, sb] = Op::sensitivity(a.value, b.value);
  const F new_abs = std::fabs(sa) * a.abs_error + std::fabs(sb) * b.abs_error +
                    roundoff_proxy(rv);
  const F denom = std::fabs(rv) + std::numeric_limits<F>::min();
  return Approx<F>{.value = result,
                   .abs_error = new_abs,
                   .rel_error = new_abs / denom,
                   .region = classify_region(rv)};
}

/** @brief Demo: propagate error through IEEE addition. */
export template <std::floating_point F = double>
constexpr Approx<F> demo_propagate_add(const Approx<F>& a,
                                       const Approx<F>& b) noexcept {
  return propagate<F, IEEEAdd<F>>(a, b);
}

/** @brief Demo: propagate error through IEEE multiplication. */
export template <std::floating_point F = double>
constexpr Approx<F> demo_propagate_mul(const Approx<F>& a,
                                       const Approx<F>& b) noexcept {
  return propagate<F, IEEEMul<F>>(a, b);
}

export template <std::floating_point F = double>
constexpr IEEE<F> demo_add_ignore(const IEEE<F>& a, const IEEE<F>& b) noexcept {
  return IgnoreErrorPolicy<F>{}(a + b);
}

/** @brief Demo operation: IEEE add under report policy. */
export template <std::floating_point F = double>
constexpr Approx<F> demo_add_report(const IEEE<F>& a,
                                    const IEEE<F>& b) noexcept {
  return ReportErrorPolicy<F>{}(a + b);
}

/** @brief Demo operation: IEEE add under adaptive policy. */
export template <std::floating_point F = double>
constexpr TernaryResult<Approx<F>> demo_add_adaptive(
    const IEEE<F>& a, const IEEE<F>& b) noexcept {
  return AdaptiveErrorPolicy<F>{}(a + b);
}

}  // namespace dedekind::ieee
