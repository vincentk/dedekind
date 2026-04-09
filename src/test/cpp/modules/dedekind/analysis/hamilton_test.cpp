#include <catch2/catch_test_macros.hpp>
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
    REQUIRE(dq_dt == 2.0);
  }
}
