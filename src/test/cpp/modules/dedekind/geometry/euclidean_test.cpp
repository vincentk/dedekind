#include <catch2/catch_test_macros.hpp>

import dedekind.geometry;

using namespace dedekind::geometry;

TEST_CASE("Geometry: Euclidean metric structure", "[geometry][euclidean]") {
  using R = double;
  using Vec2 = Vector<R, 2>;

  SECTION("Euclidean vectors satisfy metric and Euclidean concepts") {
    static_assert(IsMetricSpace<Vec2, R>);
    static_assert(IsEuclideanSpace<Vec2, R>);
  }

  SECTION("Distance matches the induced L2 norm") {
    Vec2 a{0.0, 0.0};
    Vec2 b{3.0, 4.0};

    REQUIRE(distance(a, b) == 5.0);
    REQUIRE(distance(a, b) == norm(b - a));
  }

  SECTION("Distance is symmetric") {
    Vec2 a{1.0, -2.0};
    Vec2 b{4.0, 2.0};

    REQUIRE(distance(a, b) == distance(b, a));
  }

  SECTION("Distance from a point to itself is zero") {
    Vec2 a{2.0, -1.0};

    REQUIRE(distance(a, a) == 0.0);
  }
}
