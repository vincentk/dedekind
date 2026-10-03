/**
 * @file dedekind/numbers/ieee.cppm
 * @brief Bridge from numerical carriers into the upstream IEEE core module.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "La mathematique est l'art de donner le meme nom a des choses
 * differentes."
 *       ("Mathematics is the art of giving the same name to different
 * things.")
 *       -- Henri Poincare
 */
module;

#include <concepts>
#include <optional>

export module dedekind.numbers.ieee;

import dedekind.ieee;
import dedekind.morphologies; // 𝕃<F>, the honest lane
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

/** @brief Explicit entry from the honest lane (the finite floats @c 𝕃<F>)
 *  into the IEEE fast lane. */
export template <std::floating_point F = machine_real_scalar>
constexpr IEEE<F> assume_ieee(const dedekind::morphologies::𝕃<F>& r) noexcept {
  return IEEE<F>{r.value()};
}

/** @brief Explicit exit from the IEEE fast lane into the honest lane:
 *  @c nullopt when the fast lane produced NaN or +/-inf. */
export template <std::floating_point F = machine_real_scalar>
constexpr std::optional<dedekind::morphologies::𝕃<F>> discharge_ieee(
    const IEEE<F>& x) noexcept {
  return dedekind::morphologies::try_safe_float(x.resolve());
}

}  // namespace dedekind::numbers
