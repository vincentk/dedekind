/**
 * @file dedekind/numbers/ieee.cppm
 * @module dedekind.numbers.ieee
 * @brief Bridge from numerical carriers into the upstream IEEE core module.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Нельзя быть математиком, не будучи в то же время поэтом в душе."
 *       ("It is impossible to be a mathematician without being a poet in
 * soul.")
 *       -- Софья Ковалевская (Sofya Kovalevskaya)
 */
module;

#include <concepts>

export module dedekind.numbers.ieee;

import dedekind.ieee;
import dedekind.numbers;

namespace dedekind::numbers {
export using dedekind::ieee::ieee_bind;
export using dedekind::ieee::ieee_map;

export template <std::floating_point F = machine_real_scalar>
using IEEE = dedekind::ieee::IEEE<F>;

export template <std::floating_point F = machine_real_scalar>
using IEEEAdd = dedekind::ieee::IEEEAdd<F>;

export template <std::floating_point F = machine_real_scalar>
using IEEEMul = dedekind::ieee::IEEEMul<F>;

export template <std::floating_point F = machine_real_scalar>
constexpr IEEE<F> ieee_unit(F value) noexcept {
  return dedekind::ieee::ieee_unit<F>(value);
}

/** @brief Explicit entry from the honest lane into IEEE fast lane. */
export template <std::floating_point F = machine_real_scalar>
constexpr IEEE<F> assume_ieee(const Real<F>& r) noexcept {
  return IEEE<F>{r.resolve()};
}

/** @brief Explicit exit from IEEE fast lane into the honest lane carrier. */
export template <std::floating_point F = machine_real_scalar>
constexpr Real<F> discharge_ieee(const IEEE<F>& x) noexcept {
  return Real<F>{x.resolve()};
}

}  // namespace dedekind::numbers
