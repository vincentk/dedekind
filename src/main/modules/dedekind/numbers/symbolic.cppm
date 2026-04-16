/**
 * @file dedekind/numbers/symbolic.cppm
 * @partition :symbolic
 * @brief Module interface in the dedekind hierarchy.
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
#include <cmath>
#include <concepts>

/**
 * @file dedekind/numbers/symbolic.cppm
 * @partition :symbolic
 * @brief Level 9.2: The Language of the Continuum.
 */

export module dedekind.numbers:symbolic;

import :real;
import :complex;
import dedekind.category;
import dedekind.sets;

namespace dedekind::numbers {
using namespace dedekind::category;

export template <typename Q>
constexpr auto Sqrt2_Symbolic() {
  const dedekind::sets::Ω<Q, TernaryLogic> universe{};
  // Lower-cut prototype encoded as an ETCS subobject over Q.
  return ambient_set<Q>([universe](const Q& q) {
    if constexpr (std::floating_point<Q>) {
      if (std::isnan(q)) {
        return Ternary::Unknown;
      }
    }
    const auto in_cut =
        (q * q < static_cast<Q>(2)) ? Ternary::True : Ternary::False;
    return TernaryLogic::AND(universe(q), in_cut);
  });
}

/** @section Transcendental_Anchors */

/**
 * @brief Trait to mark a species as Transcendental (π, e).
 * @details Defaults to false for base fields (Q).
 */
export template <typename T>
inline constexpr bool is_transcendental_v = false;

/**
 * @brief The Transcendental Set (𝕋).
 * @details { x ∈ ℝ | x is not a root of any rational polynomial }.
 */
export template <typename R>
  requires std::regular<R>
constexpr auto TranscendentalSet() {
  const dedekind::sets::Ω<R> universe{};
  return ambient_set<R>([universe](const R& x) constexpr {
    return universe(x) && is_transcendental_v<R>;
  });
}

}  // namespace dedekind::numbers
