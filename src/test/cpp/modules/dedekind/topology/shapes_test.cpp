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
    // ℝ is int here, a DISCRETE carrier, so #905 structural inference
    // (HasDiscreteCarrier) makes EVERY shape clopen: open shapes satisfy
    // IsClosed too, and closed shapes satisfy IsOpen too.  The boundary tag no
    // longer under-reports on int (the #904 CP finding).
    static_assert(IsOpen<UnitRay>, "Topology: Ray must be an Open set.");
    static_assert(IsOpen<UnitInterval>,
                  "Topology: Interval must be an Open set.");
    static_assert(IsClosed<ClosedUnitRay>,
                  "Topology: closed ray must satisfy IsClosed.");
    static_assert(IsClosed<ClosedUnitInterval>,
                  "Topology: closed interval must satisfy IsClosed.");
    // #905: on the discrete int carrier a closed interval is ALSO open
    // (clopen); the pre-#905 `!IsOpen<ClosedUnitInterval>` claim held only
    // because the tag said less than the discrete structure.  A genuine
    // open-not-closed set needs a DENSE carrier (see the ℚ neighborhood test).
    static_assert(IsClopen<ClosedUnitInterval>,
                  "Topology: on the discrete int carrier a closed interval is "
                  "clopen (#905 structural inference).");

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

    REQUIRE(neighborhood(point) == dedekind::category::Boole::True);
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

    // Interval exposes supremum()/infimum() — satisfies HasExtrema.
    // This links topology::Interval to the order-theoretic completeness
    // concept IsDedekindComplete = IsTotallyOrdered && IsDense && HasExtrema.
    static_assert(dedekind::category::HasExtrema<ClosedUnitInterval>);
    static_assert(dedekind::category::HasExtrema<UnitInterval>);
    static_assert(dedekind::category::HasExtrema<LeftClosedInterval>);
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
    static_assert(closed_up(3) == dedekind::category::Boole::True);
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

    CHECK(open_interval(0) == dedekind::category::Boole::False);
    CHECK(open_interval(1) == dedekind::category::Boole::True);
    CHECK(open_interval(3) == dedekind::category::Boole::False);

    CHECK(closed_interval(0) == dedekind::category::Boole::True);
    CHECK(closed_interval(3) == dedekind::category::Boole::True);
    CHECK(closed_interval(4) == dedekind::category::Boole::False);

    CHECK(left_closed_interval(0) == dedekind::category::Boole::True);
    CHECK(left_closed_interval(3) == dedekind::category::Boole::False);

    constexpr ClosedUnitInterval constexpr_closed(0, 2);
    static_assert(constexpr_closed(0) == dedekind::category::Boole::True);
    static_assert(constexpr_closed(2) == dedekind::category::Boole::True);
    static_assert(constexpr_closed(3) == dedekind::category::Boole::False);
  }

  SECTION("Intervals compose as predicates in set-builder notation") {
    using namespace dedekind::category;
    using namespace dedekind::sets;

    UnitInterval open_mid(0, 3);

    // @c open_mid is itself the membership predicate; pass it directly (no
    // forwarding lambda).
    auto in_open_mid = Set{Comprehension{UniversalSet<int>{}, open_mid}};

    CHECK(in_open_mid(1));
    CHECK_FALSE(in_open_mid(0));
    CHECK_FALSE(in_open_mid(3));
  }
}

TEST_CASE("Topology: Ø/𝔸 in the clopen ∩ decidable boundary core (Stone)",
          "[topology][clopen][decidability]") {
  using namespace dedekind::sets;
  using namespace dedekind::category;
  using Emptyℤ = Ø<int, Boole>;
  using Universeℤ = UniversalSet<int, Boole>;

  SECTION("Ø and 𝔸 are clopen (∅ and X are open ∧ closed in every topology)") {
    static_assert(IsClopen<Emptyℤ>, "Ø is clopen");
    static_assert(IsClopen<Universeℤ>, "𝔸 is clopen");
    CHECK(IsClopen<Emptyℤ>);
    CHECK(IsClopen<Universeℤ>);
  }

  SECTION("Ø and 𝔸 are BOTH clopen AND decidable (the shared boundary core)") {
    static_assert(HasDecidableMembership<Emptyℤ> && IsClopen<Emptyℤ>,
                  "the empty boundary is both clopen and decidable");
    static_assert(HasDecidableMembership<Universeℤ> && IsClopen<Universeℤ>,
                  "the universe boundary is both clopen and decidable");
    CHECK((HasDecidableMembership<Emptyℤ> && IsClopen<Emptyℤ>));
    CHECK((HasDecidableMembership<Universeℤ> && IsClopen<Universeℤ>));
  }

  // The two concepts are INDEPENDENT conservative certificates; they coincide
  // only on the Boole core (the section above).  The next two sections pin BOTH
  // directions of that independence with a regression witness each.

  SECTION(
      "discrete carrier ⟹ every set clopen: an int ray is clopen by "
      "STRUCTURE, not by tag (#905)") {
    using OpenRay = Ray<int, Direction::Upward>;
    // Pre-#905 this section asserted OpenRay was open-but-NOT-clopen, because
    // the type carried only is_open_tag.  That was the #904 CP finding: the tag
    // said LESS than the structure.  On the discrete order on int every subset
    // is clopen ({n > p} = {n >= p+1}), so #905 infers it from
    // HasDiscreteCarrier. The ray IS clopen now.
    static_assert(HasDiscreteCarrier<OpenRay>,
                  "int is a discrete carrier for the ray");
    static_assert(IsOpen<OpenRay> && IsClosed<OpenRay> && IsClopen<OpenRay>,
                  "#905: discrete carrier ⟹ the int ray is clopen (open ∧ "
                  "closed) by inference, not by tag");
    // logic_species still defaults to Boole, so membership is also decidable.
    // Here the clopen and decidable certificates coincide (both hold).  The
    // GENUINE decidable-but-NOT-clopen witness (the other independence
    // direction) needs a DENSE carrier and lives in the ℚ neighborhood test.
    static_assert(HasDecidableMembership<OpenRay>,
                  "OpenRay membership is Boole-decidable");
    CHECK(IsClopen<OpenRay>);
    CHECK(HasDecidableMembership<OpenRay>);
  }

  SECTION(
      "clopen but NOT decidable: a Kleene boundary "
      "(IsClopen does not imply HasDecidableMembership)") {
    // The other direction of independence.  A boundary object is clopen for
    // ANY logic species (∅ and X are open ∧ closed in every topology, inferred
    // via IsBoundaryObject), but HasDecidableMembership gates on
    // logic_species == Boole.  So a Kleene-valued boundary is clopen while its
    // membership is NOT decidable: the clopen certificate is conservative, it
    // does not by itself certify Boole-decidability off the core.
    using EmptyK = Ø<int, Kleene>;
    using UniverseK = UniversalSet<int, Kleene>;
    static_assert(IsClopen<EmptyK> && !HasDecidableMembership<EmptyK>,
                  "Ø<int,Kleene> is clopen but not decidable-membership");
    static_assert(IsClopen<UniverseK> && !HasDecidableMembership<UniverseK>,
                  "𝔸<int,Kleene> is clopen but not decidable-membership");
    CHECK((IsClopen<EmptyK> && !HasDecidableMembership<EmptyK>));
    CHECK((IsClopen<UniverseK> && !HasDecidableMembership<UniverseK>));
  }
}
