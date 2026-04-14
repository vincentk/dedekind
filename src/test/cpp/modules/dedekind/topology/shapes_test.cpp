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
  using ClosedUnitRay = Ray<ℝ, Direction::Upward, Boundary::Closed>;
  using ClosedUnitInterval = Interval<ℝ, Boundary::Closed, Boundary::Closed>;
  using LeftClosedInterval = Interval<ℝ, Boundary::Closed, Boundary::Open>;

  SECTION("The Skin and Body: IsOpen Verification") {
    // Rays and Open Intervals must carry the is_open_tag
    static_assert(IsOpen<UnitRay>, "Topology: Ray must be an Open set.");
    static_assert(IsOpen<UnitInterval>,
                  "Topology: Interval must be an Open set.");
    static_assert(IsClosed<ClosedUnitRay>,
                  "Topology: closed ray must satisfy IsClosed.");
    static_assert(IsClosed<ClosedUnitInterval>,
                  "Topology: closed interval must satisfy IsClosed.");
    static_assert(!IsOpen<ClosedUnitInterval>,
                  "Topology: closed interval should not satisfy IsOpen.");

    // Verify they are recognized as Convex (No holes)
    static_assert(IsConvex<UnitRay>);
    static_assert(IsConvex<UnitInterval>);
    static_assert(IsConvex<ClosedUnitInterval>);
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

  SECTION("HalfSpace: the general runtime-direction ray") {
    using HS = HalfSpace<ℝ>;

    // HalfSpace satisfies both IsHalfSpace and IsRay
    static_assert(IsHalfSpace<HS>);
    static_assert(IsRay<HS, ℝ>);
    static_assert(IsConvex<HS>);

    // Factory methods produce both orientations from a single type
    constexpr auto up = HS::upward_from(3);
    constexpr auto down = HS::downward_from(3);

    // { x | x > 3 }
    REQUIRE(!up(2));
    REQUIRE(!up(3));  // Open boundary
    REQUIRE(up(4));

    // { x | x < 3 }
    REQUIRE(down(2));
    REQUIRE(!down(3));  // Open boundary
    REQUIRE(!down(4));

    // Construct from a compile-time Ray
    UnitRay compile_time_ray{5};
    HS runtime_ray{compile_time_ray};
    REQUIRE(!runtime_ray(4));
    REQUIRE(runtime_ray(6));

    // Intersection tightens the bound
    auto hs1 = HS::upward_from(2);
    auto hs2 = HS::upward_from(5);
    auto inter = hs1 & hs2;
    REQUIRE(!inter(4));
    REQUIRE(inter(6));

    using ClosedHS = HalfSpace<ℝ, Boundary::Closed>;
    static_assert(IsHalfSpace<ClosedHS>);
    static_assert(IsClosed<ClosedHS>);
    constexpr auto closed_up = ClosedHS::upward_from(3);
    static_assert(closed_up(3) == dedekind::category::ClassicalLogic::True);
  }

  SECTION("Intersection Laws: The Convex Magma") {
    static_assert(is_convex_v<UnitRay>);
    static_assert((std::same_as<decltype(std::declval<UnitRay>() &
                                         std::declval<UnitRay>()),
                                UnitRay>));
  }

  SECTION("Boundary semantics for open and closed intervals") {
    UnitInterval open_interval(0, 3);
    ClosedUnitInterval closed_interval(0, 3);
    LeftClosedInterval left_closed_interval(0, 3);

    CHECK(open_interval(0) == dedekind::category::ClassicalLogic::False);
    CHECK(open_interval(1) == dedekind::category::ClassicalLogic::True);
    CHECK(open_interval(3) == dedekind::category::ClassicalLogic::False);

    CHECK(closed_interval(0) == dedekind::category::ClassicalLogic::True);
    CHECK(closed_interval(3) == dedekind::category::ClassicalLogic::True);
    CHECK(closed_interval(4) == dedekind::category::ClassicalLogic::False);

    CHECK(left_closed_interval(0) == dedekind::category::ClassicalLogic::True);
    CHECK(left_closed_interval(3) == dedekind::category::ClassicalLogic::False);

    constexpr ClosedUnitInterval constexpr_closed(0, 2);
    static_assert(constexpr_closed(0) ==
                  dedekind::category::ClassicalLogic::True);
    static_assert(constexpr_closed(2) ==
                  dedekind::category::ClassicalLogic::True);
    static_assert(constexpr_closed(3) ==
                  dedekind::category::ClassicalLogic::False);
  }

  SECTION("Intervals compose as predicates in set-builder notation") {
    using namespace dedekind::category;
    using namespace dedekind::sets;

    auto x = var<Ω<int>>;
    UnitInterval open_mid(0, 3);

    auto in_open_mid = Set{x % Ω<int>{} | [open_mid](const int& value) {
      return open_mid(value);
    }};

    CHECK(in_open_mid(1) == Ternary::True);
    CHECK(in_open_mid(0) == Ternary::False);
    CHECK(in_open_mid(3) == Ternary::False);
  }
}
