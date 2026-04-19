#include <catch2/catch_test_macros.hpp>

import dedekind.sequences;

using namespace dedekind::sequences;

TEST_CASE("Sequences: Convergence Tests", "[sequences][convergence]") {
  using Real = double;

  SECTION("Partial-sum convergence predicate distinguishes p-series") {
    const auto p2 = p_series_terms<Real>(2.0);
    const auto p1 = p_series_terms<Real>(1.0);

    REQUIRE(converges_series_partial_sums(p2, 1e-3));
    REQUIRE_FALSE(converges_series_partial_sums(p1, 1e-3));
  }

  SECTION("Ratio test accepts convergent geometric series") {
    const auto geometric = geometric_series_terms<Real>(0.5);
    REQUIRE(ratio_test_converges(geometric));
  }

  SECTION("Ratio test rejects divergent geometric series") {
    const auto geometric = geometric_series_terms<Real>(1.1);
    REQUIRE_FALSE(ratio_test_converges(geometric));
  }

  SECTION("Root test accepts convergent geometric series") {
    const auto geometric = geometric_series_terms<Real>(0.5);
    REQUIRE(root_test_converges(geometric));
  }

  SECTION("Root test rejects divergent geometric series") {
    const auto geometric = geometric_series_terms<Real>(1.1);
    REQUIRE_FALSE(root_test_converges(geometric));
  }

  SECTION("Comparison test validates bounded non-negative candidate") {
    const auto candidate = geometric_series_terms<Real>(0.25);
    const auto upper_bound = geometric_series_terms<Real>(0.5);
    REQUIRE(comparison_test_converges(candidate, upper_bound, 3000, 1e-3));
  }

  SECTION("Comparison test rejects invalid ordering") {
    const auto candidate = geometric_series_terms<Real>(0.5);
    const auto upper_bound = geometric_series_terms<Real>(0.25);
    REQUIRE_FALSE(comparison_test_converges(candidate, upper_bound, 3000, 1e-3));
  }
}
