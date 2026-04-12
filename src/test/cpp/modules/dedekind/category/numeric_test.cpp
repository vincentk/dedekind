/** @file test/cpp/modules/dedekind/category/numeric_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <limits>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Numeric: overflow strategy diagnostics",
          "[category][numeric][diag]") {
  SECTION("Strategy flag is available") {
    STATIC_CHECK(numeric_uses_builtin_overflow_checks ||
                 !numeric_uses_builtin_overflow_checks);
  }

  SECTION("Strategy flag matches compiler family expectation") {
#if defined(__GNUC__) || defined(__clang__)
    STATIC_CHECK(numeric_uses_builtin_overflow_checks);
#else
    STATIC_CHECK_FALSE(numeric_uses_builtin_overflow_checks);
#endif
  }
}

TEST_CASE("Numeric: NaN holes and user boundaries", "[category][numeric]") {
  SECTION("NaN hole is surfaced as Unknown") {
    const double nan = std::numeric_limits<double>::quiet_NaN();
    const auto status = classify_numeric(nan, NaNHolePolicy<double>{});
    CHECK(status == Ternary::Unknown);
  }

  SECTION("User-provided interval policy marks outside support") {
    constexpr IntervalBoundaryPolicy<int> support{-10, 10};
    CHECK(classify_numeric(4, support) == Ternary::True);
    CHECK(classify_numeric(12, support) == Ternary::Unknown);
  }

  SECTION("Certified integer addition consults policy") {
    constexpr IntervalBoundaryPolicy<int> support{-10, 10};

    const auto ok = certify_add(3, 4, support);
    CHECK(ok.status == Ternary::True);
    CHECK(ok.value == 7);

    const auto outside = certify_add(8, 5, support);
    CHECK(outside.status == Ternary::Unknown);
  }

  SECTION("Unsigned full-machine policy remains inside support") {
    const auto witness =
        certify_add(std::numeric_limits<unsigned int>::max(), 1u,
                    FullMachineBoundaryPolicy<unsigned int>{});
    CHECK(witness.status == Ternary::True);
  }

  SECTION("Certified multiply detects signed overflow") {
    constexpr IntervalBoundaryPolicy<int> support{-1000, 1000};
    const auto overflow =
        certify_mul(std::numeric_limits<int>::max(), 2, support);
    CHECK(overflow.status == Ternary::Unknown);
  }

  SECTION("Certified integer division surfaces false/unknown honestly") {
    constexpr FullMachineBoundaryPolicy<int> support{};

    const auto by_zero = certify_div(10, 0, support);
    CHECK(by_zero.status == Ternary::False);

    const auto min_overflow =
        certify_div(std::numeric_limits<int>::min(), -1, support);
    CHECK(min_overflow.status == Ternary::Unknown);
  }
}
