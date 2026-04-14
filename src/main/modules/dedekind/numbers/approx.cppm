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

/** @brief Region tags for simple runtime numeric diagnostics.
 *
 * Classifies a value into qualitative zones used by the adaptive policy
 * to distinguish numerically safe regions from pathological ones.
 *
 * @ref Higham, N.J. (2002). "Accuracy and Stability of Numerical Algorithms"
 *      (2nd ed.). SIAM. §2 (sources of rounding error and overflow).
 */
export enum class NumericRegion { Regular, NearZero, Huge, NonFinite };

/** @brief Approximation envelope for a machine float.
 *
 * Carries the computed IEEE value together with absolute and relative
 * error bounds and a region tag. The error bounds are first-order
 * estimates — not guaranteed enclosures (see interval arithmetic for
 * the latter).
 *
 * @ref Ku, H.H. (1966). "Notes on the use of propagation of error
 *      formulas". J. Research NBS 70C(4):262–263.
 *      doi:10.6028/jres.070C.025
 */
export template <std::floating_point F = machine_real_scalar>
struct Approx {
  IEEE<F> value{};
  F abs_error{};
  F rel_error{};
  NumericRegion region = NumericRegion::Regular;
};

/**
 * @brief Classify a scalar into a numeric region.
 *
 * Uses two threshold scales relative to machine epsilon and max:
 * - NearZero: |x| ≤ near_zero_scale · ε  (cancellation danger zone)
 * - Huge:     |x| ≥ huge_scale · max     (overflow proximity zone)
 * - NonFinite: x is NaN or ±inf
 *
 * The default scale factors (64, 0.5) are conservative but not
 * standardized; they can be tuned per application.
 *
 * @ref Higham, N.J. (2002). §2.2 – §2.7 for condition numbers and
 *      catastrophic cancellation near zero.
 * @ref IEEE Std 754-2019 §5 for classification of special values.
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

/**
 * @brief Conservative local rounding proxy: ε·|x|.
 *
 * This is the standard first-order model of IEEE rounding error on a
 * single operation: fl(x op y) = (x op y)(1 + δ), |δ| ≤ u, where
 * u = ε/2 is the unit roundoff. The proxy eps * |result| is an
 * upper bound on the local contribution |x op y|·u ≤ eps·|result|.
 *
 * @ref Higham, N.J. (2002). Theorem 2.2 (standard model of
 *      floating-point arithmetic). SIAM.
 * @ref Rump, S.M. (2010). "Verification methods: Rigorous results
 *      using floating-point arithmetic". Acta Numerica 19:287–449.
 *      doi:10.1017/S096249291000005X
 */
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

/**
 * @brief Propagate approximation error through a binary IEEE operation.
 *
 * Applies the first-order Gaussian (linear) error propagation rule:
 *
 *   abs_error_out ≤ |∂f/∂a| · abs_error_a
 *                 + |∂f/∂b| · abs_error_b
 *                 + ε · |f(a, b)|
 *
 * The last term is the new local rounding error of this operation,
 * estimated via roundoff_proxy. The partial derivatives are provided
 * by @c Op::sensitivity(), which is declared exact by fiat for primitive
 * IEEE operations — avoiding any circular dependency on arithmetic.
 *
 * This resolves the bootstrapping problem: sensitivities for @c IEEEAdd
 * are pure compile-time constants (1, 1), and those for @c IEEEMul are
 * operand reads, not new computations. The propagation chain never
 * recurses into the arithmetic being estimated.
 *
 * @tparam Op  Operation token exposing both @c operator()(pair) and a
 *             static @c sensitivity(a, b) -> pair<F,F>.
 *
 * @ref Gauss, C.F. (1823). Theoria combinationis observationum
 *      erroribus minimis obnoxiae. Göttingen. (§1–2, law of error
 *      propagation.)
 * @ref Ku, H.H. (1966). "Notes on the use of propagation of error
 *      formulas". J. Research NBS 70C(4):262–263.
 *      doi:10.6028/jres.070C.025
 * @ref Goodman, L.A. (1960). "On the Exact Variance of Products".
 *      J. Amer. Statist. Assoc. 55(292):708–713. doi:10.2307/2281592
 */
export template <std::floating_point F, typename Op>
constexpr Approx<F> propagate(const Approx<F>& a, const Approx<F>& b,
                              const Op& op = {}) noexcept {
  const IEEE<F> result = op({a.value, b.value});
  const auto [sa, sb] = Op::sensitivity(a.value, b.value);
  const F new_abs = std::fabs(sa) * a.abs_error + std::fabs(sb) * b.abs_error +
                    roundoff_proxy(result.resolve());
  const F rv = result.resolve();
  const F denom = std::fabs(rv) + std::numeric_limits<F>::min();
  return Approx<F>{.value = result,
                   .abs_error = new_abs,
                   .rel_error = new_abs / denom,
                   .region = classify_region(rv)};
}

/** @brief Demo: propagate error through IEEE addition. */
export template <std::floating_point F = machine_real_scalar>
constexpr Approx<F> demo_propagate_add(const Approx<F>& a,
                                       const Approx<F>& b) noexcept {
  return propagate<F, IEEEAdd<F>>(a, b);
}

/** @brief Demo: propagate error through IEEE multiplication. */
export template <std::floating_point F = machine_real_scalar>
constexpr Approx<F> demo_propagate_mul(const Approx<F>& a,
                                       const Approx<F>& b) noexcept {
  return propagate<F, IEEEMul<F>>(a, b);
}

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
