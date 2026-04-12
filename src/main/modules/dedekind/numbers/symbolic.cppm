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


namespace dedekind::numbers {

export template <typename Q>
constexpr auto Sqrt2_Symbolic() {
  return Real<Q>{static_cast<Q>(1.41421356237)};
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
  return [](const R&) constexpr { return is_transcendental_v<R>; };
}

}  // namespace dedekind::numbers
