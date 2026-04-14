#include <catch2/catch_test_macros.hpp>

import dedekind.analysis;
import dedekind.geometry;

using namespace dedekind::analysis;
using namespace dedekind::geometry;

TEST_CASE("Analysis: OneForm and Covector", "[analysis][forms][covector]") {
  using R = double;
  using Vec2 = Vector<R, 2>;

  OneForm<R, 2> w{{3.0, -1.0}};
  Vec2 v{2.0, 4.0};

  SECTION("OneForm evaluates via dot product") {
    // w(v) = 3*2 + (-1)*4 = 2.0
    REQUIRE(w(v) == 2.0);
  }

  SECTION("to_covector agrees with OneForm evaluation") {
    auto cov = to_covector(w);
    // cov(v) returns Vector<R,1>; its single entry must match w(v)
    REQUIRE(cov(v)[0] == w(v));
  }

  SECTION("to_covector round-trips component-wise") {
    auto cov = to_covector(w);
    REQUIRE(cov.coefficient(0, 0) == w.components[0]);
    REQUIRE(cov.coefficient(0, 1) == w.components[1]);
  }
}
