/** @file dedekind/topology/shapes_test.cpp
 *
 * The order-topology reading of @c order's shapes.  Topology owns no shapes of
 * its own: a halfspace is a principal up-/down-set (an order-theoretic datum),
 * an interval the meet of two, and open / closed is what the strictness of
 * those bounds MEANS in the carrier's order topology --- inferred, never
 * tagged. On a dense carrier (@c double here) a strict ray is open and not
 * closed; on a discrete carrier (@c int) every subset is clopen.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.order;
import dedekind.topology;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;
using namespace dedekind::topology;

TEST_CASE(
    "Topology: open / closed are read off the strictness of order's shapes",
    "[topology][continuity]") {
  using OpenRay = Halfspace<double, Direction::Upward, Strictness::Strict>;
  using ClosedRay = Halfspace<double, Direction::Upward, Strictness::NonStrict>;
  using OpenInterval = Interval<double, Strictness::Strict, Strictness::Strict>;
  using ClosedInterval =
      Interval<double, Strictness::NonStrict, Strictness::NonStrict>;
  using LeftClosedInterval =
      Interval<double, Strictness::NonStrict, Strictness::Strict>;

  SECTION(
      "dense carrier: strict is open, non-strict is closed, mixed is neither") {
    STATIC_CHECK(IsOpen<OpenRay> && !IsClosed<OpenRay>);
    STATIC_CHECK(IsClosed<ClosedRay> && !IsOpen<ClosedRay>);
    STATIC_CHECK(IsOpen<OpenInterval> && !IsClosed<OpenInterval>);
    STATIC_CHECK(IsClosed<ClosedInterval> && !IsOpen<ClosedInterval>);
    STATIC_CHECK(!IsOpen<LeftClosedInterval> && !IsClosed<LeftClosedInterval>);
    STATIC_CHECK(!IsClopen<OpenRay>);
    // the complement swaps the reading
    STATIC_CHECK(IsClosed<Not<OpenRay>> && !IsOpen<Not<OpenRay>>);
  }

  SECTION("convexity: rays, points and their meets") {
    STATIC_CHECK(IsConvex<OpenRay>);
    STATIC_CHECK(IsConvex<OpenInterval>);
    STATIC_CHECK(IsConvex<ClosedInterval>);
    STATIC_CHECK(IsConvex<Singleton<double>>);
  }

  SECTION("a dense open interval is a neighbourhood of its points") {
    constexpr auto nbhd =
        make_interval<Strictness::Strict, Strictness::Strict>(0.0, 2.0);
    STATIC_CHECK(IsNeighborhood<decltype(nbhd), double>);
    STATIC_CHECK(nbhd(1.0) == Boole::True);
    STATIC_CHECK(nbhd(0.0) == Boole::False);
    STATIC_CHECK(nbhd(2.0) == Boole::False);
    CHECK(nbhd(1.0) == Boole::True);  // codecov
  }

  SECTION("boundary semantics of the three closures") {
    const auto open_iv =
        make_interval<Strictness::Strict, Strictness::Strict>(0.0, 3.0);
    const auto closed_iv =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(0.0, 3.0);
    const auto left_closed =
        make_interval<Strictness::NonStrict, Strictness::Strict>(0.0, 3.0);
    CHECK(open_iv(0.0) == Boole::False);
    CHECK(open_iv(1.0) == Boole::True);
    CHECK(open_iv(3.0) == Boole::False);
    CHECK(closed_iv(0.0) == Boole::True);
    CHECK(closed_iv(3.0) == Boole::True);
    CHECK(closed_iv(4.0) == Boole::False);
    CHECK(left_closed(0.0) == Boole::True);
    CHECK(left_closed(3.0) == Boole::False);
  }

  SECTION("discrete carrier: the same shapes are clopen by structure") {
    using IntRay = Halfspace<int, Direction::Upward, Strictness::Strict>;
    using IntInterval = Interval<int, Strictness::Strict, Strictness::Strict>;
    STATIC_CHECK(HasDiscreteCarrier<IntRay>);
    STATIC_CHECK(IsClopen<IntRay>);
    STATIC_CHECK(IsClopen<IntInterval>);
    STATIC_CHECK(IsClopen<Not<IntRay>>);
    CHECK(IsClopen<IntRay>);  // codecov
  }
}

TEST_CASE("Topology: Ø/𝔸 in the clopen ∩ decidable boundary core (Stone)",
          "[topology][clopen][decidability]") {
  using Emptyℤ = Ø<int, Boole>;
  using Universeℤ = Universe<int, Boole>;

  SECTION("Ø and 𝔸 are clopen (∅ and X are open ∧ closed in every topology)") {
    STATIC_CHECK(IsClopen<Emptyℤ>);
    STATIC_CHECK(IsClopen<Universeℤ>);
    CHECK(IsClopen<Emptyℤ>);
    CHECK(IsClopen<Universeℤ>);
  }

  SECTION(
      "on the boundary objects the two topologies agree: clopen AND "
      "decidable") {
    STATIC_CHECK(HasDecidableMembership<Emptyℤ> && IsClopen<Emptyℤ>);
    STATIC_CHECK(HasDecidableMembership<Universeℤ> && IsClopen<Universeℤ>);
    CHECK((HasDecidableMembership<Emptyℤ> && IsClopen<Emptyℤ>));
  }

  SECTION(
      "discrete carrier ⟹ every set clopen: an int ray is clopen by "
      "STRUCTURE, not by tag (#905), and decidable") {
    using OpenRay = Halfspace<int, Direction::Upward, Strictness::Strict>;
    STATIC_CHECK(HasDiscreteCarrier<OpenRay>);
    STATIC_CHECK(IsOpen<OpenRay> && IsClosed<OpenRay> && IsClopen<OpenRay>);
    STATIC_CHECK(HasDecidableMembership<OpenRay>);
    CHECK(IsClopen<OpenRay>);
    CHECK(HasDecidableMembership<OpenRay>);
  }

  SECTION("the ℕ proxy is a discrete carrier too (#937): its ray is clopen") {
    using NatRay =
        Halfspace<Cardinality, Direction::Upward, Strictness::Strict>;
    STATIC_CHECK(HasDiscreteCarrier<NatRay>);
    STATIC_CHECK(IsClopen<NatRay>);
    CHECK(IsClopen<NatRay>);
  }

  SECTION(
      "clopen but NOT recognised decidable: a Kleene boundary (the two "
      "topologies are different certificates)") {
    using EmptyK = Ø<int, Kleene>;
    using UniverseK = Universe<int, Kleene>;
    STATIC_CHECK(IsClopen<EmptyK> && !HasDecidableMembership<EmptyK>);
    STATIC_CHECK(IsClopen<UniverseK> && !HasDecidableMembership<UniverseK>);
    CHECK((IsClopen<EmptyK> && !HasDecidableMembership<EmptyK>));
  }
}
