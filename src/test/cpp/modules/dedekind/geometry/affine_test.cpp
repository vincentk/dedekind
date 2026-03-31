#include <catch2/catch_test_macros.hpp>
import dedekind.geometry;
import dedekind.numbers;

using namespace dedekind::geometry;
using namespace dedekind::numbers;

TEST_CASE("Geometry: Affine Rational Space", "[geometry][affine]") {
  using ℚ = Rational<int>;
  using Vec2 = Vector<ℚ, 2>;

  SECTION("Rational Scaling") {
    // v = (1, 1), scale = 1/2
    Vec2 v{ℚ(1, 1), ℚ(1, 1)};
    ℚ scale(1, 2);

    auto res = v * scale;

    // Result: (1/2, 1/2)
    REQUIRE(res[0].num() == 1);
    REQUIRE(res[0].den() == 2);
  }

  SECTION("Linear Combination") {
    Vec2 a{ℚ(1, 2), ℚ(0, 1)};
    Vec2 b{ℚ(1, 2), ℚ(1, 1)};

    auto c = a + b;  // (1/2 + 1/2, 0 + 1) = (1, 1)
    REQUIRE(c[0].num() == 1);
    REQUIRE(c[1].num() == 1);
  }
}
