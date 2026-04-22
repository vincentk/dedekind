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

  SECTION("Subtraction: (a + bε) - (c + dε)") {
    DualValue a{5.0, 3.0};
    DualValue b{2.0, 1.0};
    DualValue res = a - b;
    REQUIRE(res.value() == 3.0);
    REQUIRE(res.derivative() == 2.0);
  }

  SECTION("Unary negation: -(a + bε) = -a - bε") {
    DualValue a{4.0, -1.0};
    DualValue res = -a;
    REQUIRE(res.value() == -4.0);
    REQUIRE(res.derivative() == 1.0);
  }

  SECTION("Inverse: (a + bε)⁻¹ = 1/a - (b/a²)ε") {
    // (2 + 1ε)⁻¹ = 0.5 - 0.25ε
    DualValue a{2.0, 1.0};
    DualValue inv = a.inverse();
    REQUIRE(inv.value() == 0.5);
    REQUIRE(inv.derivative() == -0.25);
  }

  SECTION("Division: AD rule d/dx(1/x)|_{x=2} = -1/4") {
    // f(x) = 1/x, f'(x) = -1/x²
    // Seed x = 2, dx = 1: (2 + 1ε) / (2 + 1ε) would give 1, test 1/(x):
    DualValue one{1.0, 0.0};
    DualValue x{2.0, 1.0};
    DualValue res = one / x;
    REQUIRE(res.value() == 0.5);
    REQUIRE(res.derivative() == -0.25);
  }
}
