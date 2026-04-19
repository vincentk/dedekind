#include <catch2/catch_test_macros.hpp>
#include <cmath>

import dedekind.analysis;

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

  SECTION("Worked theorem chain combines derivative and integral witnesses") {
    const auto integrand = [](Real x) { return 2.0 * x; };
    const auto antiderivative = [](Real x) { return x * x; };

    REQUIRE(ftc_worked_theorem_chain<Real>(integrand, antiderivative, -1.0, 1.5,
                                           3.0, 1e-3));
  }
}
