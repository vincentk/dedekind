/**
 * @file dedekind/analysis/hamilton.cppm
 * @partition :hamilton
 * @brief Level 4: The Principle of Least Action (Hamiltonian Dynamics).
 *
 * @quote "The variation of the definite integral of the difference between
 * the kinetic and potential energies is zero." — Sir William Rowan Hamilton
 *
 * @section Hamiltonian: The Flow of the Species
 * This partition defines the generator of motion \( \mathcal{H} \). In the
 * Dedekind Category, the Hamiltonian is the Morphism that maps the Phase
 * Space (Position and Momentum) to the Energy Scalar Field.
 *
 * @details
 * Following the Poisson formulation:
 * - IsHamiltonian: A mapping \(\mathcal{H}: \mathcal{S} \to \mathbb{F}\).
 * - Poisson Bracket: The Lie Algebra structure defining the flow.
 * - Conservation: The proof that \(\{f, \mathcal{H}\} = 0\) for invariants.
 *
 * @build_order 9
 * @dependency :algebra, :geometry, :sequences
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
#include <cstddef>

export module dedekind.analysis:hamilton;

import dedekind.category;
import dedekind.algebra;
import dedekind.geometry;

namespace dedekind::analysis {
using namespace dedekind::algebra;
using namespace dedekind::geometry;

/**
 * @concept IsHamiltonian
 * @brief Formal verification of the energy mapping.
 */
export template <typename H, typename S, typename F>
concept IsHamiltonian = requires(H h, S s) {
  { h.energy(s) } -> std::same_as<F>;
};

/**
 * @brief The Poisson Bracket: The fundamental operation of Classical Mechanics.
 * @details {f, g} = Σ (∂f/∂q ∂g/∂p - ∂f/∂p ∂g/∂q)
 */
export template <std::floating_point R, std::size_t N>
constexpr R poisson_bracket(auto&& f, auto&& g, const Vector<R, N>& state) {
  static_assert(
      N == 2,
      "This poisson_bracket finite-difference overload only supports a "
      "single canonical pair (q, p), so N must be 2.");

  const R eps = static_cast<R>(1e-6);

  auto bump = [&](std::size_t idx, R delta) {
    Vector<R, N> s = state;
    s[idx] += delta;
    return s;
  };

  const R df_dq =
      (f(bump(0, eps)) - f(bump(0, -eps))) / (static_cast<R>(2) * eps);
  const R df_dp =
      (f(bump(1, eps)) - f(bump(1, -eps))) / (static_cast<R>(2) * eps);
  const R dg_dq =
      (g(bump(0, eps)) - g(bump(0, -eps))) / (static_cast<R>(2) * eps);
  const R dg_dp =
      (g(bump(1, eps)) - g(bump(1, -eps))) / (static_cast<R>(2) * eps);

  return (df_dq * dg_dp) - (df_dp * dg_dq);
}

/**
 * @concept IsPoissonAlgebra
 * @brief A Ring equipped with a bracket satisfying the Jacobi Identity.
 */
export template <typename A>
concept IsPoissonAlgebra = IsRing<A> && requires(A f, A g) {
  { bracket(f, g) } -> std::same_as<A>;
};

}  // namespace dedekind::analysis
