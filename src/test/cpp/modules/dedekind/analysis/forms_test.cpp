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

  SECTION("from_covector is the inverse view of to_covector") {
    auto cov = to_covector(w);
    auto round_trip = from_covector(cov);

    REQUIRE(round_trip.components == w.components);
    REQUIRE(round_trip(v) == w(v));
  }

  SECTION("Scalar-valued differentials admit a OneForm interpretation") {
    const auto quadratic = make_differentiable_map<R, 2, 1>(
        [](const Vec2& x) { return Vector<R, 1>{x[0] * x[0] + 3.0 * x[1]}; },
        [](const Vec2& x) {
          Covector<R, 2> cov;
          cov.set_coefficient(0, 0, 2.0 * x[0]);
          cov.set_coefficient(0, 1, 3.0);
          return cov;
        });

    const Vec2 point{2.0, -1.0};
    const Vec2 direction{0.5, 2.0};
    const auto cov = differential_at(quadratic, point);
    const auto one_form = from_covector(cov);

    REQUIRE(one_form.components[0] == 4.0);
    REQUIRE(one_form.components[1] == 3.0);
    REQUIRE(one_form(direction) == cov(direction)[0]);
  }
}
