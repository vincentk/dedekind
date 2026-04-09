#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

import dedekind.numbers;
import dedekind.sequences;

using namespace dedekind::numbers;

TEST_CASE("Numbers: Construction of Transcendental Constants",
          "[numbers][reals]") {
  SECTION("Archimedean Resolution: Sqrt(2)") {
    auto root2 = Sqrt2();
    double val = root2.resolve();

    // Check against the machine's intrinsic sqrt(2)
    REQUIRE_THAT(val, Catch::Matchers::WithinRel(1.41421356, 0.00001));
  }

  SECTION("Taylor Convergence: Euler's Number (e)") {
    auto e = E();
    double val = e.resolve();

    // Resolve at the path's horizon
    REQUIRE_THAT(val, Catch::Matchers::WithinRel(2.71828182, 0.00001));
  }

  SECTION("Nilakantha Convergence: Pi (π)") {
    auto pi = Pi();
    double val = pi.resolve();

    REQUIRE_THAT(val, Catch::Matchers::WithinRel(3.14159265, 0.00001));
  }

  SECTION("Algebraic Synthesis: e + pi") {
    /**
     * @test Functorial addition of two transcendental paths.
     * Real + Real -> Real (via path summation).
     */
    auto sum = E() + Pi();
    double val = sum.resolve();

    REQUIRE_THAT(val, Catch::Matchers::WithinRel(5.85987448, 0.00001));
  }
}
