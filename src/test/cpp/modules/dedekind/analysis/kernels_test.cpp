#include <catch2/catch_test_macros.hpp>
import dedekind.analysis;
import dedekind.geometry;

using namespace dedekind::analysis;
using namespace dedekind::geometry;

TEST_CASE("Analysis: Kernel Methods", "[analysis][rkhs]") {
  using ℝ = double;
  GaussianKernel<ℝ> k{1.0};

  SECTION("Kernel Symmetry") {
    // K(a, b) == K(b, a)
    REQUIRE(k(1.0, 2.0) == k(2.0, 1.0));
    static_assert(IsKernel<decltype(k), ℝ, ℝ>);
  }

  SECTION("Functional Evaluation") {
    /**
     * @proof In an RKHS, the kernel represents the "Similarity"
     * between two points in the feature space.
     */
    ℝ similarity = k(0.0, 0.0);  // Maximum similarity
    REQUIRE(similarity == 1.0);

    ℝ far_similarity = k(0.0, 100.0);
    REQUIRE(far_similarity < 0.0001);
  }
}
