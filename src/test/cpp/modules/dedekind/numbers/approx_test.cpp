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

// --- Gaussian error propagation via sensitivity (Ku 1966) ---

TEST_CASE("propagate: add accumulates errors from both inputs",
          "[numbers][approx]") {
  // a = 1.0 with error 0.01, b = 2.0 with error 0.02
  const Approx<double> a{ieee_unit(1.0), 0.01, 0.01};
  const Approx<double> b{ieee_unit(2.0), 0.02, 0.01};

  const auto result = demo_propagate_add(a, b);

  // sensitivity of add is (1,1): expected contribution >= 0.01 + 0.02 = 0.03
  CHECK(result.abs_error >= 0.03);
  CHECK(result.value.resolve() == 3.0);
  CHECK(result.region == NumericRegion::Regular);
}

TEST_CASE("propagate: multiply scales error by operand magnitude (Ku 1966)",
          "[numbers][approx]") {
  // a = 3.0 with error 0.01, b = 4.0 with error 0.02
  // sensitivity of mul at (3,4): (b=4, a=3)
  // abs_error_out >= 4*0.01 + 3*0.02 = 0.04 + 0.06 = 0.10
  const Approx<double> a{ieee_unit(3.0), 0.01, 0.0};
  const Approx<double> b{ieee_unit(4.0), 0.02, 0.0};

  const auto result = demo_propagate_mul(a, b);

  CHECK(result.abs_error >= 0.10);
  CHECK(result.value.resolve() == 12.0);
}

TEST_CASE("propagate: zero-error inputs yield only local rounding contribution",
          "[numbers][approx]") {
  const Approx<double> a{ieee_unit(1.0), 0.0, 0.0};
  const Approx<double> b{ieee_unit(2.0), 0.0, 0.0};

  const auto result = demo_propagate_add(a, b);

  // With no prior error, only roundoff_proxy(3.0) = eps*3.0 is added
  const double local_eps = std::numeric_limits<double>::epsilon() * 3.0;
  CHECK(result.abs_error == local_eps);
}
