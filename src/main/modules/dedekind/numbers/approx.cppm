/**
 * @file dedekind/numbers/approx.cppm
 * @module dedekind.numbers.approx
 * @brief Compatibility bridge for IEEE approximation policies.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "In dedekind.numbers.approx, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
 */
module;

#include <concepts>

export module dedekind.numbers.approx;

import dedekind.category;
import dedekind.ieee.approx;
import dedekind.numbers;
import dedekind.numbers.ieee;

namespace dedekind::numbers {
using namespace dedekind::category;

export using dedekind::ieee::NumericRegion;

export template <std::floating_point F = machine_real_scalar>
using Approx = dedekind::ieee::Approx<F>;

export template <typename Op, typename F>
concept IsIEEEPropagationOp = dedekind::ieee::IsIEEEPropagationOp<Op, F>;

export template <std::floating_point F = machine_real_scalar>
constexpr NumericRegion classify_region(F x, F near_zero_scale = F{64},
                                        F huge_scale = F{0.5}) noexcept {
  return dedekind::ieee::classify_region<F>(x, near_zero_scale, huge_scale);
}

export template <std::floating_point F = machine_real_scalar>
constexpr F roundoff_proxy(F x) noexcept {
  return dedekind::ieee::roundoff_proxy<F>(x);
}

export template <std::floating_point F = machine_real_scalar>
using ReportErrorPolicy = dedekind::ieee::ReportErrorPolicy<F>;

export template <std::floating_point F = machine_real_scalar>
using IgnoreErrorPolicy = dedekind::ieee::IgnoreErrorPolicy<F>;

export template <std::floating_point F = machine_real_scalar>
using AdaptiveErrorPolicy = dedekind::ieee::AdaptiveErrorPolicy<F>;

export using dedekind::ieee::propagate;
export using dedekind::ieee::demo_propagate_add;
export using dedekind::ieee::demo_propagate_mul;
export using dedekind::ieee::demo_add_ignore;
export using dedekind::ieee::demo_add_report;
export using dedekind::ieee::demo_add_adaptive;

}  // namespace dedekind::numbers
