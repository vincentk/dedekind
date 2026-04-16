#include <catch2/catch_test_macros.hpp>
import dedekind.geometry;

using namespace dedekind::geometry;

TEST_CASE("Geometry: Affine Rational Space", "[geometry][affine]") {
  using R = double;
  using Vec2 = Vector<R, 2>;

  SECTION("Affine concept witness") { static_assert(IsAffine<Vec2, R>); }

  SECTION("Rational Scaling") {
    Vec2 v{1.0, 1.0};
    R scale = 0.5;

    auto res = v * scale;

    REQUIRE(res[0] == 0.5);
    REQUIRE(res[1] == 0.5);
  }

  SECTION("Linear Combination") {
    Vec2 a{0.5, 0.0};
    Vec2 b{0.5, 1.0};

    auto c = a + b;
    REQUIRE(c[0] == 1.0);
    REQUIRE(c[1] == 1.0);
  }
}
