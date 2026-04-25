#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <numbers>
import dedekind.analysis;
import dedekind.geometry;

using namespace dedekind::analysis;
using namespace dedekind::geometry;

TEST_CASE("Analysis: Symplectic Geometry", "[analysis][exterior]") {
  using ℝ = double;
  using Vec2 = Vector<ℝ, 2>;

  // Define basis 1-forms: dq = (1, 0), dp = (0, 1)
  OneForm<ℝ, 2> dq{{1.0, 0.0}};
  OneForm<ℝ, 2> dp{{0.0, 1.0}};

  SECTION("Wedge Product: The 2-Form") {
    // ω = dp ∧ dq
    auto omega = wedge(dp, dq);

    Vec2 u{1.0, 0.0};  // Along q
    Vec2 v{0.0, 1.0};  // Along p

    /**
     * @proof The area spanned by unit vectors in p and q is 1.
     * ω(u, v) = -1 (due to dp ∧ dq orientation)
     */
    REQUIRE(omega(u, v) == -1.0);
    REQUIRE(omega(v, u) == 1.0);  // Antisymmetry check
  }

  SECTION("Hamiltonian Flow Area Preservation") {
    /**
     * @test In a simple harmonic oscillator, the 'area'
     * in phase space (p, q) is preserved during the rotation.
     */
    auto omega = wedge(dp, dq);
    Vec2 state_t0{1.0, 0.0};
    Vec2 state_t1{0.0, 1.0};  // After 1/4 cycle

    // The "Action" or area between these states relative to origin
    REQUIRE(std::abs(omega(state_t0, state_t1)) == 1.0);
  }
}

#include <catch2/catch_test_macros.hpp>
import dedekind.analysis;
import dedekind.geometry;

using namespace dedekind::analysis;
using namespace dedekind::geometry;

TEST_CASE("Analysis: Hamiltonian Observables", "[analysis][hamilton]") {
  using ℝ = double;
  using Vec2 = Vector<ℝ, 2>;  // (q, p)

  // H = 0.5 * (p² + q²) -> Harmonic Oscillator
  auto H = [](auto state) {
    auto q = state[0];
    auto p = state[1];
    return 0.5 * (p * p + q * q);
  };

  SECTION("Conservation: {H, H} = 0") {
    Vec2 current_state{1.0, 1.0};

    ℝ bracket_val = poisson_bracket(H, H, current_state);

    /** @proof Any observable's bracket with itself is zero (Antisymmetry). */
    REQUIRE(std::abs(bracket_val) < 0.000001);
  }

  SECTION("Equation of Motion: {q, H} = p") {
    auto q_obs = [](auto state) { return state[0]; };
    Vec2 current_state{1.0, 2.0};  // q=1, p=2

    // dq/dt = {q, H}
    ℝ dq_dt = poisson_bracket(q_obs, H, current_state);

    // For H = 0.5(p²+q²), dq/dt = p = 2.0
    REQUIRE(std::abs(dq_dt - 2.0) < 1e-3);
  }

  SECTION("Closed-form curve: quarter period rotation") {
    const auto gamma = harmonic_oscillator_curve<ℝ>(1.0, 0.0);
    const auto at_quarter_turn = gamma.at(std::numbers::pi_v<ℝ> / 2.0);

    REQUIRE(std::abs(at_quarter_turn[0]) < 1e-12);
    REQUIRE(std::abs(at_quarter_turn[1] + 1.0) < 1e-12);
  }

  SECTION("Leapfrog path: matches closed-form over short horizon") {
    constexpr ℝ dt = 0.001;
    const auto path = harmonic_oscillator_leapfrog_path<ℝ>(1.0, 0.0, dt);

    const auto numeric = path.at(1000);  // t = 1.0
    const auto exact = harmonic_oscillator_closed_form<ℝ>(1.0, 0.0, 1.0);

    REQUIRE(std::abs(numeric[0] - exact[0]) < 1e-3);
    REQUIRE(std::abs(numeric[1] - exact[1]) < 1e-3);
  }

  SECTION("Finite leapfrog path: approximate energy conservation") {
    constexpr ℝ dt = 0.01;
    const auto states =
        harmonic_oscillator_leapfrog_finite_path<ℝ>(1.0, 0.0, dt, 500);

    const auto energy = [](const Vec2& s) {
      const ℝ q = s[0];
      const ℝ p = s[1];
      return 0.5 * (p * p + q * q);
    };

    const ℝ e0 = energy(states.at(0));
    const ℝ eN = energy(states.at(states.size() - 1));

    REQUIRE(std::abs(eN - e0) < 2e-2);
  }
}

TEST_CASE("Analysis: HarmonicOscillator carrier satisfies IsHamiltonian (#388)",
          "[analysis][hamilton][concept]") {
  // The HarmonicOscillator<R> carrier introduced in :hamilton is the
  // concrete witness for IsHamiltonian.  H(q, p) = ½ (p² + ω² q²).
  // This test exercises the .energy() member at runtime, complementing
  // the static_assert(IsHamiltonian<...>) pinned next to the carrier.
  using ℝ = double;
  using Phase = Vector<ℝ, 2>;

  SECTION("ω = 1: H(q=1, p=0) = ½") {
    const HarmonicOscillator<ℝ> H{1.0};
    REQUIRE(std::abs(H.energy(Phase{1.0, 0.0}) - 0.5) < 1e-12);
  }

  SECTION("ω = 1: H(q=0, p=1) = ½") {
    const HarmonicOscillator<ℝ> H{1.0};
    REQUIRE(std::abs(H.energy(Phase{0.0, 1.0}) - 0.5) < 1e-12);
  }

  SECTION("ω = 2: H(q=1, p=0) = 2 (kinetic-free, ω² q² / 2 = 4 / 2)") {
    const HarmonicOscillator<ℝ> H{2.0};
    REQUIRE(std::abs(H.energy(Phase{1.0, 0.0}) - 2.0) < 1e-12);
  }

  SECTION("ω = 1: total energy is conserved along the closed-form flow") {
    // Sample energy at four different times along the H-flow; values
    // must agree to within floating-point round-off (the closed-form
    // expression is exact in the symbolic q, p parametrisation).
    const HarmonicOscillator<ℝ> H{1.0};
    const auto curve = harmonic_oscillator_curve<ℝ>(1.0, 0.0, 1.0);
    const ℝ e0 = H.energy(curve(0.0));
    const ℝ e1 = H.energy(curve(0.5));
    const ℝ e2 = H.energy(curve(1.0));
    const ℝ e3 = H.energy(curve(2.5));
    REQUIRE(std::abs(e1 - e0) < 1e-12);
    REQUIRE(std::abs(e2 - e0) < 1e-12);
    REQUIRE(std::abs(e3 - e0) < 1e-12);
  }
}
