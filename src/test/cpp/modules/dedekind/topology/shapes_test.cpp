/** @file dedekind/topology/shapes_test.cpp
 *
 * The order-topology reading of @c order's shapes.  Topology owns no shapes of
 * its own: a halfspace is a principal up-/down-set (an order-theoretic datum),
 * an interval the meet of two, and open / closed is what the strictness of
 * those bounds MEANS in the carrier's order topology --- inferred, never
 * tagged. On a discrete carrier (@c int, ℕ) every subset is clopen; the dense
 * reading (a strict ray is open and not closed, ¬ swaps) is witnessed on ℚ in
 * numbers/neighborhood_test, since @c double is not @c IsTotallyOrdered here
 * (NaN) and nothing upstream of topology is dense.
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

TEST_CASE("Topology: on a discrete carrier every shape is clopen by structure",
          "[topology][continuity]") {
  using IntRay = UpRay<int, Strictness::Strict>;
  using IntInterval = Interval<int, Strictness::Strict, Strictness::Strict>;
  using ClosedIntInterval =
      Interval<int, Strictness::NonStrict, Strictness::NonStrict>;

  SECTION("clopen, whatever the strictness") {
    STATIC_CHECK(HasDiscreteCarrier<IntRay>);
    STATIC_CHECK(IsClopen<IntRay>);
    STATIC_CHECK(IsClopen<IntInterval>);
    STATIC_CHECK(IsClopen<ClosedIntInterval>);
    STATIC_CHECK(IsClopen<Not<IntRay>>);
    CHECK(IsClopen<IntRay>);  // codecov
  }

  SECTION("convexity: rays, points and their meets") {
    STATIC_CHECK(IsConvex<IntRay>);
    STATIC_CHECK(IsConvex<IntInterval>);
    STATIC_CHECK(IsConvex<Singleton<int>>);
  }

  SECTION("an open interval is a neighbourhood of its points") {
    constexpr auto nbhd =
        make_interval<Strictness::Strict, Strictness::Strict>(0, 3);
    STATIC_CHECK(IsNeighborhood<decltype(nbhd), int>);
    STATIC_CHECK(nbhd(1) == Boole::True);
    STATIC_CHECK(nbhd(0) == Boole::False);
    STATIC_CHECK(nbhd(3) == Boole::False);
    CHECK(nbhd(2) == Boole::True);  // codecov
  }

  SECTION("boundary semantics of the three closures") {
    const auto open_iv =
        make_interval<Strictness::Strict, Strictness::Strict>(0, 3);
    const auto closed_iv =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(0, 3);
    const auto left_closed =
        make_interval<Strictness::NonStrict, Strictness::Strict>(0, 3);
    CHECK(open_iv(0) == Boole::False);
    CHECK(open_iv(1) == Boole::True);
    CHECK(open_iv(3) == Boole::False);
    CHECK(closed_iv(0) == Boole::True);
    CHECK(closed_iv(3) == Boole::True);
    CHECK(closed_iv(4) == Boole::False);
    CHECK(left_closed(0) == Boole::True);
    CHECK(left_closed(3) == Boole::False);
  }
}

TEST_CASE("Topology: Ø/𝔸 in the clopen ∩ decidable boundary core (Stone)",
          "[topology][clopen][decidability]") {
  using Emptyℤ = Ø<int, Boole>;
  using Universeℤ = 𝔸<int, Boole>;

  SECTION("Ø and 𝔸 are clopen (∅ and X are open ∧ closed in every topology)") {
    STATIC_CHECK(IsClopen<Emptyℤ>);
    STATIC_CHECK(IsClopen<Universeℤ>);
    CHECK(IsClopen<Emptyℤ>);
    CHECK(IsClopen<Universeℤ>);
  }

  SECTION("on the boundary objects both readings hold: clopen AND decidable") {
    STATIC_CHECK(HasDecidableMembership<Emptyℤ> && IsClopen<Emptyℤ>);
    STATIC_CHECK(HasDecidableMembership<Universeℤ> && IsClopen<Universeℤ>);
    CHECK((HasDecidableMembership<Emptyℤ> && IsClopen<Emptyℤ>));
  }

  SECTION(
      "discrete carrier ⟹ every set clopen: an int ray is clopen by "
      "STRUCTURE, not by tag (#905), and decidable") {
    using OpenRay = UpRay<int, Strictness::Strict>;
    STATIC_CHECK(HasDiscreteCarrier<OpenRay>);
    STATIC_CHECK(IsOpen<OpenRay> && IsClosed<OpenRay> && IsClopen<OpenRay>);
    STATIC_CHECK(HasDecidableMembership<OpenRay>);
    CHECK(IsClopen<OpenRay>);
    CHECK(HasDecidableMembership<OpenRay>);
  }

  SECTION("the ℕ proxy is a discrete carrier too (#937): its ray is clopen") {
    using NatRay = UpRay<Cardinality, Strictness::Strict>;
    STATIC_CHECK(HasDiscreteCarrier<NatRay>);
    STATIC_CHECK(IsClopen<NatRay>);
    CHECK(IsClopen<NatRay>);
  }

  SECTION(
      "order-clopen but NOT recognised decidable: a Kleene boundary --- on "
      "a discrete carrier decidable ⇒ order-clopen, never the converse") {
    using EmptyK = Ø<int, Kleene>;
    using UniverseK = 𝔸<int, Kleene>;
    STATIC_CHECK(IsClopen<EmptyK> && !HasDecidableMembership<EmptyK>);
    STATIC_CHECK(IsClopen<UniverseK> && !HasDecidableMembership<UniverseK>);
    CHECK((IsClopen<EmptyK> && !HasDecidableMembership<EmptyK>));
  }
}
