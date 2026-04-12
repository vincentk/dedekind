module;
#include <concepts>

/**
 * @file dedekind/numbers/symbolic.cppm
 * @partition :symbolic
 * @brief Level 9.2: The Language of the Continuum.
 */

export module dedekind.numbers:symbolic;

import :real;
import :complex;
import dedekind.sets;

namespace dedekind::numbers {
using namespace dedekind::sets;

export template <typename Q>
constexpr auto Sqrt2_Symbolic() {
  auto x = var<Ω<Q>>;
  // Lower Dedekind cut prototype: { q in Q | q^2 < 2 }.
  return Set{x % Ω<Q>{} | [](const Q& q) { return q * q < static_cast<Q>(2); }};
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
  auto x = var<Ω<R>>;
  return Set{x % Ω<R>{} |
             [](const R&) constexpr { return is_transcendental_v<R>; }};
}

}  // namespace dedekind::numbers
