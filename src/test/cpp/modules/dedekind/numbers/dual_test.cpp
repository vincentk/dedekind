#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

import dedekind.numbers;

using namespace dedekind::numbers;

TEST_CASE("Analysis: Dual Numbers and Differentiation", "[numbers][dual]") {
  using Scalar = double;
  using DualValue = Dual<Scalar>;

  SECTION("Automatic Differentiation: f(x) = x²") {
    // Seed: x = 3, dx = 1
    DualValue x{3.0, 1.0};

    // Compute f(x) = x * x
    DualValue res = x * x;

    // f(3) = 9
    REQUIRE(res.value() == 9.0);
    // f'(3) = 2*x = 6
    REQUIRE(res.derivative() == 6.0);
  }
}
