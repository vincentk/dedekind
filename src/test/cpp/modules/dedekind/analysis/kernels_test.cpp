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

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.analysis;
import dedekind.geometry;
import dedekind.sequences;
import dedekind.category;

using namespace dedekind::analysis;
using namespace dedekind::geometry;
using namespace dedekind::sequences;
using namespace dedekind::category;

TEST_CASE("Analysis: Kernel Ontology & RKHS", "[analysis][rkhs][ontology]") {
  using ℝ = double;
  GaussianKernel<ℝ> k{1.0};  // Sigma = 1.0

  SECTION("Categorical Logic") {
    // Verify it is a Morphism (Domain -> Codomain)
    static_assert(IsMorphism<decltype(k), dedekind::sets::RealLine, ℝ>);

    // Verify it is a Sequence (Path) even if it's infinite
    static_assert(IsSequence<decltype(k)>);

    // Verify it is NOT finite (it's a path over the continuum)
    static_assert(!IsFiniteSequence<decltype(k)>);

    // Verify the high-level Kernel concept
    static_assert(IsKernel<decltype(k), ℝ, ℝ>);
  }

  SECTION("Symmetry & Evaluation") {
    /** @proof K(a, b) == K(b, a) */
    REQUIRE(k(1.5, 2.5) == k(2.5, 1.5));

    /** @proof The Path evaluation at the origin */
    REQUIRE(k[0.0] == 1.0);  // k[x] maps to k(0, x)
  }

  SECTION("Hilbert Space Properties") {
    // Maximum similarity at distance 0
    REQUIRE(k(5.0, 5.0) == 1.0);

    // Decay property of the Gaussian Path
    REQUIRE(k(0.0, 10.0) < k(0.0, 1.0));
    REQUIRE(k(0.0, 50.0) < 1e-10);
  }
}
