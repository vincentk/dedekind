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
}
