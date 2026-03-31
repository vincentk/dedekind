#include <catch2/catch_test_macros.hpp>
import dedekind.geometry;
import dedekind.numbers;

using namespace dedekind::geometry;
using namespace dedekind::numbers;

TEST_CASE("Geometry: The Hilbert Horizon", "[geometry][hilbert]") {
  using ℝ = double;
  using Vec3 = Vector<ℝ, 3>;

  SECTION("Orthogonality: Zero Inner Product") {
    Vec3 x{1.0, 0.0, 0.0};
    Vec3 y{0.0, 1.0, 0.0};

    // Two vectors are orthogonal if their dot product is zero
    REQUIRE(dot(x, y) == 0.0);

    static_assert(IsInnerProductSpace<Vec3, ℝ>);
  }

  SECTION("Euclidean Metrics: Distance and Norm") {
    Vec3 v{3.0, 4.0, 0.0};

    // The induced norm: ||(3,4,0)|| = sqrt(3^2 + 4^2) = 5
    REQUIRE(norm(v) == 5.0);
  }

  SECTION("Hilbert Completeness Proof") {
    /**
     * @proof Any finite-dimensional real vector space is a Hilbert space.
     */
    static_assert(
        IsHilbertSpace<Vec3, ℝ>,
        "Axiom Failure: Finite real spaces must satisfy Hilbert completeness.");
  }
}
/**
 * @concept IsIdentityMorphism
 * @brief The "Self-Loop" of the Category (id).
 * @details For any object A, there exists a unique morphism id : A -> A.
 *          This morphism must act as a neutral element for composition.
 */