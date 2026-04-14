#include <catch2/catch_test_macros.hpp>
#include <cmath>
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

TEST_CASE("corner case: division by zero is flagged as non-finite",
          "[numbers][approx][corner]") {
  const auto one = ieee_unit(1.0);
  const auto zero = ieee_unit(0.0);

  const auto quotient = one / zero;
  const auto adaptive = AdaptiveErrorPolicy<double>{}(quotient);

  CHECK_FALSE(std::isfinite(quotient.resolve()));
  CHECK(adaptive.status == Ternary::False);
  CHECK(adaptive.value.region == NumericRegion::NonFinite);
}

TEST_CASE("corner case: tiny multiplication underflow remains diagnosable",
          "[numbers][approx][corner]") {
  const double tiny = std::numeric_limits<double>::denorm_min();
  const Approx<double> a{ieee_unit(tiny), tiny, 0.0};
  const Approx<double> b{ieee_unit(tiny), tiny, 0.0};

  const auto result = demo_propagate_mul(a, b);
  const auto adaptive = AdaptiveErrorPolicy<double>{}(result.value);

  CHECK(std::isfinite(result.value.resolve()));
  CHECK(result.value.resolve() >= 0.0);
  CHECK(result.region == NumericRegion::NearZero);
  CHECK(adaptive.status == Ternary::Unknown);
}

TEST_CASE("corner case: high local sensitivity amplifies propagated error",
          "[numbers][approx][corner]") {
  const Approx<double> noisy{ieee_unit(1.0), 1e-12, 0.0};
  const Approx<double> low_gain{ieee_unit(1.0), 0.0, 0.0};
  const Approx<double> high_gain{ieee_unit(1e12), 0.0, 0.0};

  const auto low = demo_propagate_mul(noisy, low_gain);
  const auto high = demo_propagate_mul(noisy, high_gain);

  // For multiplication, |∂(a*b)/∂a| = |b|. Large |b| should strongly amplify
  // inherited error from a.
  CHECK(high.abs_error > low.abs_error * 1e9);
  CHECK(std::isfinite(high.abs_error));
}

TEST_CASE("corner case: region boundaries behave as expected",
          "[numbers][approx][corner]") {
  const double eps = std::numeric_limits<double>::epsilon();
  const double maxv = std::numeric_limits<double>::max();

  const double near_zero_boundary = 64.0 * eps;
  const double huge_boundary = 0.5 * maxv;

  CHECK(classify_region(near_zero_boundary) == NumericRegion::NearZero);
  CHECK(classify_region(std::nextafter(near_zero_boundary,
                                       std::numeric_limits<double>::infinity())) ==
        NumericRegion::Regular);

  CHECK(classify_region(huge_boundary) == NumericRegion::Huge);
  CHECK(classify_region(std::nextafter(huge_boundary, 0.0)) ==
        NumericRegion::Regular);
}
