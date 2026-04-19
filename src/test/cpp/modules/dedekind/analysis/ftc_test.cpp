#include <catch2/catch_test_macros.hpp>
#include <cmath>

import dedekind.analysis;
import dedekind.ieee;

using namespace dedekind::analysis;

TEST_CASE("Analysis: Fundamental Theorem of Calculus Bridges",
          "[analysis][ftc]") {
  using Real = double;

  SECTION("Part I bridge: derivative of accumulation matches integrand") {
    const auto integrand = [](Real x) { return 2.0 * x; };
    REQUIRE(ftc_part_i_bridge<Real>(integrand, 0.0, 1.5, 1e-3));
  }

  SECTION("Part II bridge: integral equals antiderivative delta") {
    const auto integrand = [](Real x) { return 2.0 * x; };
    const auto antiderivative = [](Real x) { return x * x; };

    REQUIRE(
        ftc_part_ii_bridge<Real>(integrand, antiderivative, -1.0, 3.0, 1e-3));
  }

  SECTION("Derivative witness approximates slope for smooth function") {
    const auto cubic = [](Real x) { return x * x * x; };
    const Real slope = derivative_at<Real>(cubic, 2.0);
    REQUIRE(std::abs(slope - 12.0) < 1e-3);
  }

  SECTION("Integral witness approximates area for quadratic") {
    const auto quadratic = [](Real x) { return x * x; };
    const Real area = integral_over<Real>(quadratic, 0.0, 1.0);
    REQUIRE(std::abs(area - (1.0 / 3.0)) < 1e-4);
  }

  SECTION("Integral witness handles orientation and degenerate intervals") {
    const auto quadratic = [](Real x) { return x * x; };

    const Real forward = integral_over<Real>(quadratic, 0.0, 1.0);
    const Real reverse = integral_over<Real>(quadratic, 1.0, 0.0);
    REQUIRE(std::abs(forward + reverse) < 1e-8);

    const Real degenerate = integral_over<Real>(quadratic, 2.0, 2.0);
    REQUIRE(std::abs(degenerate) < 1e-12);
  }

  SECTION("Part I and Part II bridges hold for constant integrand") {
    const auto integrand = [](Real) { return 3.0; };
    const auto antiderivative = [](Real x) { return 3.0 * x; };

    REQUIRE(ftc_part_i_bridge<Real>(integrand, -2.0, 0.75, 1e-4));
    REQUIRE(
        ftc_part_ii_bridge<Real>(integrand, antiderivative, -2.0, 4.0, 1e-4));
  }

  SECTION("Explicit FTC hypotheses hold for matching polynomial data") {
    const auto integrand = [](Real x) { return 2.0 * x; };
    const auto antiderivative = [](Real x) { return x * x; };

    REQUIRE(ftc_part_i_hypotheses<Real>(integrand, -1.0, 3.0));
    REQUIRE(ftc_part_ii_hypotheses<Real>(integrand, antiderivative, -1.0, 3.0,
                                         32, 1e-3));
  }

  SECTION("Part II hypotheses reject mismatched antiderivative") {
    const auto integrand = [](Real x) { return 2.0 * x; };
    const auto wrong_antiderivative = [](Real x) { return x * x + x; };

    REQUIRE_FALSE(ftc_part_ii_hypotheses<Real>(integrand, wrong_antiderivative,
                                               -1.0, 3.0, 32, 1e-3));
  }

  SECTION("Worked theorem chain combines derivative and integral witnesses") {
    const auto integrand = [](Real x) { return 2.0 * x; };
    const auto antiderivative = [](Real x) { return x * x; };

    REQUIRE(ftc_worked_theorem_chain<Real>(integrand, antiderivative, -1.0, 1.5,
                                           3.0, 1e-3));
  }

  SECTION("Worked theorem chain accepts IEEE realization carriers") {
    using IEEEReal = dedekind::ieee::IEEE<double>;
    const auto integrand = [](IEEEReal x) { return IEEEReal{2.0} * x; };
    const auto antiderivative = [](IEEEReal x) { return x * x; };

    REQUIRE(ftc_worked_theorem_chain<IEEEReal>(integrand, antiderivative,
                                               IEEEReal{-1.0}, IEEEReal{1.5},
                                               IEEEReal{3.0}, IEEEReal{1e-3}));
  }
}

TEST_CASE("Analysis: Derivative and Integral Diagnostics",
          "[analysis][ftc][diagnostics]") {
  using Real = double;

  SECTION("suggest_derivative_step_size validates h=1e-5 default") {
    // Test cubic polynomial: f(x) = x^3, f'(x) = 3x^2
    const auto cubic = [](Real x) { return x * x * x; };

    const auto [suggested_h, drift] =
        suggest_derivative_step_size<Real>(cubic, 2.0, 1e-6);

    // For a smooth cubic, h should stabilize at a fine scale (1e-6 or 1e-7)
    // and the drift should be very small
    REQUIRE(suggested_h > 0);
    REQUIRE(drift >= 0);
    REQUIRE(drift < 1e-4);  // Convergence should be tight for smooth function
  }

  SECTION("suggest_derivative_step_size finds step size quickly") {
    // Simple quadratic: f(x) = x^2, f'(x) = 2x
    const auto quadratic = [](Real x) { return x * x; };

    const auto [suggested_h, drift] =
        suggest_derivative_step_size<Real>(quadratic, 1.0, 1e-6);

    // h should be finite and positive
    REQUIRE(suggested_h > 1e-8);
    REQUIRE(suggested_h <= 1e-3);
  }

  SECTION("diagnose_integral_convergence validates slice count heuristic") {
    // Simple quadratic: ∫_0^1 x^2 dx = 1/3
    const auto quadratic = [](Real x) { return x * x; };

    const auto [recommended_slices, max_drift] =
        diagnose_integral_convergence<Real>(quadratic, 0.0, 1.0, 1e-5);

    // For a smooth quadratic, 500 or 2500 slices should suffice;
    // drift between refinements should be tiny
    REQUIRE(recommended_slices >= 100);
    REQUIRE(recommended_slices <= 4096);
    REQUIRE(max_drift >= 0);
    REQUIRE(max_drift < 1e-3);
  }

  SECTION("diagnose_integral_convergence shows convergence pattern") {
    // Linear: ∫_0^2 x dx = 2
    const auto linear = [](Real x) { return x; };

    const auto [recommended_slices, max_drift] =
        diagnose_integral_convergence<Real>(linear, 0.0, 2.0, 1e-6);

    // Linear function should converge very quickly
    REQUIRE(recommended_slices <= 500);
    REQUIRE(max_drift < 1e-4);
  }

  SECTION("diagnose_integral_convergence handles constant function") {
    // Constant: ∫_0^3 5 dx = 15
    const auto constant = [](Real) { return 5.0; };

    const auto [recommended_slices, max_drift] =
        diagnose_integral_convergence<Real>(constant, 0.0, 3.0, 1e-8);

    // Constant should be nearly exact at any scale
    REQUIRE(recommended_slices >= 100);
    REQUIRE(max_drift < 1e-7);
  }
}
