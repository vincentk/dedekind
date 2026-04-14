/**
 * @file dedekind/numbers/approx.cppm
 * @module dedekind.numbers.approx
 * @brief Opt-in approximation policies for IEEE numerics.
 */
module;

#include <cmath>
#include <concepts>
#include <limits>

export module dedekind.numbers.approx;

import dedekind.category;
import dedekind.numbers;
import dedekind.numbers.ieee;

namespace dedekind::numbers {
using namespace dedekind::category;

/** @brief Region tags for simple runtime numeric diagnostics. */
export enum class NumericRegion { Regular, NearZero, Huge, NonFinite };

/** @brief Approximation envelope for a machine value. */
export template <std::floating_point F = machine_real_scalar>
struct Approx {
  IEEE<F> value{};
  F abs_error{};
  F rel_error{};
  NumericRegion region = NumericRegion::Regular;
};

/**
 * @brief Simple local diagnostics around origin, huge values and non-finite
 * values.
 */
export template <std::floating_point F = machine_real_scalar>
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

/** @brief Conservative local rounding proxy: eps * |x|. */
export template <std::floating_point F = machine_real_scalar>
constexpr F roundoff_proxy(F x) noexcept {
  return std::numeric_limits<F>::epsilon() * std::fabs(x);
}

/** @brief Report policy: keep value and attach an error envelope. */
export template <std::floating_point F = machine_real_scalar>
struct ReportErrorPolicy {
  constexpr Approx<F> operator()(const IEEE<F>& x) const noexcept {
    const F v = x.resolve();
    const F abs = roundoff_proxy(v);
    const F denom = std::fabs(v) + std::numeric_limits<F>::min();
    return Approx<F>{.value = x,
                     .abs_error = abs,
                     .rel_error = abs / denom,
                     .region = classify_region(v)};
  }
};

/** @brief Ignore policy: run fast lane and drop all diagnostics. */
export template <std::floating_point F = machine_real_scalar>
struct IgnoreErrorPolicy {
  constexpr IEEE<F> operator()(const IEEE<F>& x) const noexcept { return x; }
};

/**
 * @brief Adaptive policy: respond to risky regions via ternary status.
 *
 * False   -> non-finite result
 * Unknown -> near-zero or huge magnitude where model confidence is reduced
 * True    -> regular zone
 */
export template <std::floating_point F = machine_real_scalar>
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

/** @brief Demo operation: IEEE add under ignore policy. */
export template <std::floating_point F = machine_real_scalar>
constexpr IEEE<F> demo_add_ignore(const IEEE<F>& a, const IEEE<F>& b) noexcept {
  return IgnoreErrorPolicy<F>{}(a + b);
}

/** @brief Demo operation: IEEE add under report policy. */
export template <std::floating_point F = machine_real_scalar>
constexpr Approx<F> demo_add_report(const IEEE<F>& a,
                                    const IEEE<F>& b) noexcept {
  return ReportErrorPolicy<F>{}(a + b);
}

/** @brief Demo operation: IEEE add under adaptive policy. */
export template <std::floating_point F = machine_real_scalar>
constexpr TernaryResult<Approx<F>> demo_add_adaptive(
    const IEEE<F>& a, const IEEE<F>& b) noexcept {
  return AdaptiveErrorPolicy<F>{}(a + b);
}

}  // namespace dedekind::numbers
