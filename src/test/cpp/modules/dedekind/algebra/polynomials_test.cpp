#include <catch2/catch_test_macros.hpp>
import dedekind.algebra;

using namespace dedekind::algebra;

TEST_CASE("Algebra: Polynomial Morphisms", "[algebra][polynomial]") {
  using ℤ = int;

  // p(x) = x² - 2
  Polynomial<ℤ> p({-2, 0, 1});

  SECTION("Evaluation in Z") {
    REQUIRE(p(0) == -2);
    REQUIRE(p(2) == 2);
  }

  SECTION("Root Identification (Sqrt2 Proxy)") {
    double root = 1.41421356;
    // p(√2) ≈ 0
    REQUIRE(std::abs(p(root)) < 0.0001);
  }

  SECTION("Axiomatic Ring Structure") { static_assert(IsRing<decltype(p)>); }
}
