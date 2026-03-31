#include <catch2/catch_test_macros.hpp>

import dedekind.sequences;
import dedekind.topology;
import dedekind.category;

using namespace dedekind::sequences;
using namespace dedekind::topology;
using namespace dedekind::category;

TEST_CASE("Sequences: Cauchy Convergence", "[sequences][cauchy]") {
  using ℝ = double;

  // The classic harmonic sequence (convergent/Cauchy)
  Path<ℝ> harmonic{[](std::size_t n) { return 1.0 / (n + 1.0); }};

  SECTION("Axiomatic Discovery") {
    static_assert(IsCauchy<decltype(harmonic)>);
    static_assert(IsConvergent<decltype(harmonic)>);
  }

  SECTION("Metric Verification") {
    // Sample two points far out in the path
    ℝ s_1000 = harmonic.at(1000);
    ℝ s_2000 = harmonic.at(2000);

    // The distance between them must be approaching zero
    REQUIRE(std::abs(s_1000 - s_2000) < 0.001);
  }
}
