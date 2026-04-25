/**
 * @file dedekind/analysis/hamilton.cppm
 * @partition :hamilton
 * @brief Level 4: The Principle of Least Action (Hamiltonian Dynamics).
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
 * @note **Transcendental Functions (Intensional Use):**
 * Current use of std::cos/std::sin in harmonic_oscillator_closed_form() is
 * intentional and serves as a guide post for future IsTranscendental<>
 * abstraction (see GitHub issue #344). We preserve explicit std:: prefix to
 * highlight the extensional-to-intensional bridging needed for symbolic
 * calculus. Do not refactor these calls until the transcendental abstraction
 * is formalized. See harmonic_oscillator_closed_form() docstring (line ~104).
 *
 * @build_order 9
 * @dependency :algebra, :geometry, :sequences
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "The variation of the definite integral of the difference between
 * the kinetic and potential energies is zero." — Sir William Rowan Hamilton
 *
 */
module;

#include <cmath>
#include <concepts>
#include <cstddef>

export module dedekind.analysis:hamilton;

import dedekind.category;
import dedekind.algebra;
import dedekind.geometry;
import dedekind.sequences;
import :ftc;

namespace dedekind::analysis {
using namespace dedekind::algebra;
using namespace dedekind::geometry;
using namespace dedekind::sequences;

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

  const R ε = static_cast<R>(
      1e-6);  // FIXME #123: magic constant for finite-difference;
              // pending numeric stability analysis

  auto bump = [&](std::size_t idx, R δ) {
    Vector<R, N> s = state;
    s[idx] += δ;
    return s;
  };

  // Partial derivatives via derivative_at (from :ftc), which implements the
  // central-difference formula (f(x+h)-f(x-h))/(2h). Each coordinate is
  // treated as a scalar perturbation around zero displacement.
  const R df_dq = derivative_at<R>([&](R d) { return f(bump(0, d)); }, R{0}, ε);
  const R df_dp = derivative_at<R>([&](R d) { return f(bump(1, d)); }, R{0}, ε);
  const R dg_dq = derivative_at<R>([&](R d) { return g(bump(0, d)); }, R{0}, ε);
  const R dg_dp = derivative_at<R>([&](R d) { return g(bump(1, d)); }, R{0}, ε);

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

/** @brief Canonical 1D phase-space state (q, p). */
export template <std::floating_point R>
using PhasePoint = Vector<R, 2>;

/**
 * @brief Closed-form harmonic-oscillator flow at time t.
 * @details
 * For H(q,p) = 1/2 (p^2 + ω^2 q^2) with unit mass:
 *   q(t) = q0 cos(ωt) + (p0/ω) sin(ωt)
 *   p(t) = p0 cos(ωt) - ω q0 sin(ωt)
 *
 * @note The direct use of std::cos() and std::sin() here is intentional—it
 * marks the boundary between extensional (runtime) and intensional (symbolic)
 * computation. Once transcendental function abstraction (GitHub #344) is
 * formalized, this function will serve as a template for integrating the
 * IsTranscendental<> concept. Do not refactor before that design is complete.
 */
export template <std::floating_point R>
constexpr PhasePoint<R> harmonic_oscillator_closed_form(R q0, R p0, R t,
                                                        R ω = R{1}) {
  const R wt = ω * t;
  const R c = std::cos(wt);
  const R s = std::sin(wt);
  return PhasePoint<R>{q0 * c + (p0 / ω) * s, p0 * c - (ω * q0) * s};
}

/**
 * @brief Continuum-indexed Hamiltonian trajectory as a Curve.
 */
export template <std::floating_point R>
constexpr auto harmonic_oscillator_curve(R q0, R p0, R ω = R{1}) {
  auto flow = [q0, p0, ω](R t) {
    return harmonic_oscillator_closed_form<R>(q0, p0, t, ω);
  };
  return Curve<R, PhasePoint<R>>{flow};
}

/**
 * @brief Discrete Hamiltonian trajectory via leapfrog/Verlet (infinite path).
 * @details
 * The update is the symplectic kick-drift-kick form used in N-body style
 * benchmark loops: preserve qualitative energy behavior over long horizons.
 */
export template <std::floating_point R>
constexpr auto harmonic_oscillator_leapfrog_path(R q0, R p0, R dt, R ω = R{1}) {
  auto trajectory = [q0, p0, dt, ω](std::size_t n) {
    R q = q0;
    R p = p0;
    const R half = R{0.5};
    const R k = ω * ω;

    for (std::size_t i = 0; i < n; ++i) {
      const R p_half = p - half * dt * (k * q);
      q += dt * p_half;
      p = p_half - half * dt * (k * q);
    }
    return PhasePoint<R>{q, p};
  };

  return Path<PhasePoint<R>>{trajectory};
}

export template <std::floating_point R>
constexpr auto harmonic_oscillator_leapfrog_finite_path(R q0, R p0, R dt,
                                                        std::size_t steps,
                                                        R ω = R{1}) {
  auto infinite = harmonic_oscillator_leapfrog_path<R>(q0, p0, dt, ω);
  return prefix(infinite, steps);
}

/**
 * @brief Concrete Hamiltonian carrier for the 1-D harmonic oscillator.
 *
 * @details H(q, p) = ½ (p² + ω² q²) with unit mass.  Provides the
 * minimal `.energy(state) -> R` surface required by @c IsHamiltonian.
 * Pinned here so the concept is witnessed by a concrete carrier and
 * not just a contract in the abstract.
 */
export template <std::floating_point R>
struct HarmonicOscillator {
  R ω = R{1};

  constexpr R energy(const PhasePoint<R>& state) const {
    const R q = state[0];
    const R p = state[1];
    return R{0.5} * (p * p + ω * ω * q * q);
  }
};

/** @section Formal_Verification */
static_assert(IsCurve<decltype(harmonic_oscillator_curve<double>(1.0, 0.0))>,
              "Closed-form harmonic trajectory must be a curve.");

static_assert(
    IsHamiltonian<HarmonicOscillator<double>, PhasePoint<double>, double>,
    "HarmonicOscillator<double> must satisfy IsHamiltonian: "
    "energy(state) ∈ ℝ.");

static_assert(IsSequence<decltype(harmonic_oscillator_leapfrog_path<double>(
                  1.0, 0.0, 0.01))>,
              "Leapfrog trajectory must be a discrete sequence path.");

static_assert(
    IsFiniteSequence<decltype(harmonic_oscillator_leapfrog_finite_path<double>(
        1.0, 0.0, 0.01, 32))>,
    "Dedicated finite-horizon leapfrog trajectory must be finite.");

}  // namespace dedekind::analysis
