#include <catch2/catch_test_macros.hpp>
#include <limits>

import dedekind.category;
import dedekind.numbers;
import dedekind.numbers.ieee;
import dedekind.numbers.approx;

using namespace dedekind::category;
using namespace dedekind::numbers;

TEST_CASE("Approx policy demos: ignore/report/adaptive", "[numbers][approx]") {
  const auto a = ieee_unit(0.1);
  const auto b = ieee_unit(0.2);

  const auto ignored = demo_add_ignore(a, b);
  CHECK(ignored.resolve() != 0.3);

  const auto reported = demo_add_report(a, b);
  CHECK(reported.abs_error >= 0.0);
  CHECK(reported.rel_error >= 0.0);

  const auto adaptive = demo_add_adaptive(a, b);
  CHECK(adaptive.status == Ternary::True);
}

TEST_CASE("Adaptive policy reacts near origin", "[numbers][approx]") {
  const auto tiny = ieee_unit(std::numeric_limits<double>::epsilon());
  const auto adaptive = demo_add_adaptive(tiny, ieee_unit(0.0));

  CHECK(adaptive.status == Ternary::Unknown);
  CHECK(adaptive.value.region == NumericRegion::NearZero);
}

TEST_CASE("Adaptive policy reacts for huge magnitude", "[numbers][approx]") {
  const auto huge = ieee_unit(std::numeric_limits<double>::max());
  const auto adaptive = demo_add_adaptive(huge, ieee_unit(0.0));

  CHECK(adaptive.status == Ternary::Unknown);
  CHECK(adaptive.value.region == NumericRegion::Huge);
}

TEST_CASE("Adaptive policy marks non-finite as false", "[numbers][approx]") {
  const auto inf = ieee_unit(std::numeric_limits<double>::infinity());
  const auto adaptive = demo_add_adaptive(inf, ieee_unit(0.0));

  CHECK(adaptive.status == Ternary::False);
  CHECK(adaptive.value.region == NumericRegion::NonFinite);
}
