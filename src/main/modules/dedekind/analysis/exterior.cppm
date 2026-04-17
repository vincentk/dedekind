/**
 * @file dedekind/analysis/exterior.cppm
 * @partition :exterior
 * @brief Level 11.6: The Wedge Product and Symplectic Geometry.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "C'est par la logique qu'on prouve, c'est par l'intuition qu'on
 * invente."
 *       -- Henri Poincaré, *Science et méthode* (1908), Livre I, Chapitre I
 *
 *       English translation: "It is by logic that we prove, but by intuition
 *       that we discover."
 */
module;

#include <concepts>
#include <functional>

export module dedekind.analysis:exterior;

import :forms;
import dedekind.geometry;

namespace dedekind::analysis {

using namespace dedekind::geometry;

/**
 * @class TwoForm
 * @brief An antisymmetric bilinear form ω(u, v) = -ω(v, u).
 * @details Represents a "Surface Sensor" in Phase Space.
 */
export template <std::floating_point F, std::size_t N>
struct TwoForm {
  // Represented as an antisymmetric matrix or a collection of basis pairs.
  // For simplicity in 2D/Symplectic: ω = dp ∧ dq
  F magnitude;

  /** @section The_Symplectic_Action */
  constexpr F operator()(const Vector<F, N>& u, const Vector<F, N>& v) const {
    // ω(u, v) = u1*v2 - u2*v1 (The determinant/signed area)
    return magnitude * (u[0] * v[1] - u[1] * v[0]);
  }
};

/**
 * @brief The Wedge Product (∧): α ∧ β
 * @details For 1-forms, α ∧ β = α ⊗ β - β ⊗ α.
 */
export template <std::floating_point F, std::size_t N>
constexpr auto wedge(const OneForm<F, N>& a, const OneForm<F, N>& b) {
  // In 2D, this results in a TwoForm scaled by the exterior components
  return TwoForm<F, N>{(a.components[0] * b.components[1]) -
                       (a.components[1] * b.components[0])};
}

}  // namespace dedekind::analysis
