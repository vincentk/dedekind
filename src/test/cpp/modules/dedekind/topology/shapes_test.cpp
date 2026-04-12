#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.order;
import dedekind.topology;

using namespace dedekind::topology;

TEST_CASE("Topology: Rules of Continuity Coverage", "[topology][continuity]") {
  using ℝ = int;
  using UnitRay = Ray<ℝ, Direction::Upward>;
  using UnitInterval = Interval<ℝ>;

  SECTION("The Skin and Body: IsOpen Verification") {
    // Rays and Open Intervals must carry the is_open_tag
    static_assert(IsOpen<UnitRay>, "Topology: Ray must be an Open set.");
    static_assert(IsOpen<UnitInterval>,
                  "Topology: Interval must be an Open set.");

    // Verify they are recognized as Convex (No holes)
    static_assert(IsConvex<UnitRay>);
    static_assert(IsConvex<UnitInterval>);
  }

  SECTION("Neighborhoods: The Space Around a Point") {
    UnitInterval neighborhood(0, 2);
    ℝ point = 1;

    /**
     * @requirement IsNeighborhood
     * 1. Must be a Set.
     * 2. Must be Open (is_open_tag exists).
     * 3. Must contain the point.
     */
    static_assert(IsNeighborhood<UnitInterval, ℝ>,
                  "Topology: Interval must satisfy the Neighborhood concept.");

    REQUIRE(neighborhood(point) == dedekind::category::ClassicalLogic::True);
  }

  SECTION("Morphological Shapes: Half-Spaces & Molecules") {
    // Verify Ray satisfies the 'Naked Boundary' (IsHalfSpace)
    static_assert(IsHalfSpace<UnitRay>,
                  "Topology: Ray must expose its bound and is_ray_tag.");

    // Verify Interval satisfies the 'Synthesis' (IsInterval)
    static_assert(IsInterval<UnitInterval>,
                  "Topology: Interval must be a molecule of two Half-Spaces.");

    // Check structural requirements for Interval components
    static_assert(IsHalfSpace<typename UnitInterval::lower_ray_type>);
    static_assert(IsHalfSpace<typename UnitInterval::upper_ray_type>);
  }

  SECTION("Intersection Laws: The Convex Magma") {
    static_assert(is_convex_v<UnitRay>);
    static_assert((std::same_as<decltype(std::declval<UnitRay>() &
                                         std::declval<UnitRay>()),
                                UnitRay>));
  }
}
