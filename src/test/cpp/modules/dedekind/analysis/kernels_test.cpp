#include <catch2/catch_test_macros.hpp>

import dedekind.analysis;

using namespace dedekind::analysis;

TEST_CASE("Analysis: Kernel Methods", "[analysis][rkhs]") {
  using R = double;
  GaussianKernel<R> k{1.0};

  SECTION("Kernel symmetry and concept") {
    REQUIRE(k(1.0, 2.0) == k(2.0, 1.0));
    static_assert(IsKernel<decltype(k), R, R>);
  }

  SECTION("Evaluation behavior") {
    REQUIRE(k(0.0, 0.0) == 1.0);
    REQUIRE(k(0.0, 10.0) < k(0.0, 1.0));
    REQUIRE(k(2.0) == k(0.0, 2.0));
    REQUIRE(k[2.0] == k(0.0, 2.0));
    REQUIRE(k.at(0) == 1.0);
  }
}
