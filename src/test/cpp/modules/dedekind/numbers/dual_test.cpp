#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

import dedekind.numbers;

using namespace dedekind::numbers;

TEST_CASE("Analysis: Dual Numbers and Differentiation", "[numbers][dual]") {
  using R = double;
  using D = Dual<R>;

  SECTION("Automatic Differentiation: f(x) = x²") {
    // Seed: x = 3, dx = 1
    D x{3.0, 1.0};

    // Compute f(x) = x * x
    D res = x * x;

    // f(3) = 9
    REQUIRE(res.value() == 9.0);
    // f'(3) = 2*x = 6
    REQUIRE(res.derivative() == 6.0);
  }
}
