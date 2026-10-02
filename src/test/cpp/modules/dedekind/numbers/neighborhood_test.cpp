/** @file dedekind/numbers/neighborhood_test.cpp
 *
 * @brief The acceptance test: a "neighborhood between two ℚ" that is at once a
 * topological neighborhood AND a bona-fide Lwv/ETCS set AND Jlt-intensional.
 *
 * A real is approached through its rational neighborhoods — open intervals with
 * ℚ endpoints shrinking onto it (the completion of ℚ).  For that perspective to
 * live inside this library the neighborhood must obey the Lwv laws: it is a
 * subobject of its carrier (Member + ι + χ, the ETCS axioms) over a regular
 * carrier (the Jlt value-semantics half), with a decidable characteristic map
 * and no enumeration.  Since @c topology::Interval now inherits the @c SetExpr
 * ETCS surface while keeping its open tag, one open @c Interval is all of these
 * at once.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>  // std::remove_cvref_t

import dedekind.category; // IsSet (the ETCS axioms)
import dedekind.numbers;  // Rational, Cut
import dedekind.order;
import dedekind.sets; // HasDecidableMembership
import dedekind.topology; // IsOpen, IsClosed, IsNeighborhood (read off order's shapes)

using namespace dedekind::numbers;
using dedekind::order::Direction;
using dedekind::order::Halfspace;
using dedekind::order::make_interval;
using dedekind::order::Strictness;

namespace {
using Q = Rational<>;
// A rational neighborhood (7/5, 3/2) ⊂ ℚ — an open interval between two ℚ.
constexpr auto nbhd =
    make_interval<Strictness::Strict, Strictness::Strict>(Q{7, 5}, Q{3, 2});
using QNbhd = std::remove_cvref_t<decltype(nbhd)>;  // Meet<H↑, H↓> over ℚ
}  // namespace

TEST_CASE("a rational neighborhood is a topological neighborhood AND a Lwv set",
          "[numbers][neighborhood][topology][lwv]") {
  SECTION("Lwv/ETCS: a first-class set, over a regular (Jlt) carrier") {
    STATIC_CHECK(dedekind::sets::IsSetObject<QNbhd>);
    STATIC_CHECK(std::regular<Q>);  // the Jlt value-semantics half of Lwv
  }

  SECTION("topology: an open neighborhood of its points") {
    STATIC_CHECK(dedekind::topology::IsOpen<QNbhd>);
    STATIC_CHECK(dedekind::topology::IsNeighborhood<QNbhd, Q>);
  }

  SECTION(
      "dense carrier ⟹ the GENUINE open-⊋-clopen witness (#905): ℚ is not "
      "discrete, so open shapes are open-but-NOT-closed") {
    // This is the independence direction that int (discrete) CANNOT provide:
    // #905 makes every set on int clopen, so the "decidable but NOT clopen"
    // witness the #904 shapes test used to place on Ray<int> must live on a
    // DENSE carrier.  ℚ is dense (!HasDiscreteCarrier), so its open shapes are
    // open, not closed, and hence not clopen: the real open ⊋ clopen.
    using namespace dedekind::topology;
    using QOpenRay =
        Halfspace<Q, Direction::Upward, Strictness::Strict>;  // {x > p}
    STATIC_CHECK(!HasDiscreteCarrier<QOpenRay>);
    STATIC_CHECK(!HasDiscreteCarrier<QNbhd>);
    STATIC_CHECK(IsOpen<QOpenRay> && !IsClosed<QOpenRay> &&
                 !IsClopen<QOpenRay>);
    STATIC_CHECK(IsOpen<QNbhd> && !IsClosed<QNbhd> && !IsClopen<QNbhd>);
    // Yet membership is Boole-decidable: decidable does NOT imply clopen (the
    // #904 independence direction, relocated here to a dense carrier).
    STATIC_CHECK(dedekind::sets::HasDecidableMembership<QOpenRay>);
    CHECK(!IsClopen<QOpenRay>);
  }

  SECTION("intensional, decidable characteristic map χ (no enumeration)") {
    STATIC_CHECK(static_cast<bool>(nbhd(Q{143, 100})));    // 1.43 ∈ (1.4,1.5)
    STATIC_CHECK_FALSE(static_cast<bool>(nbhd(Q{2})));     // 2 ∉
    STATIC_CHECK_FALSE(static_cast<bool>(nbhd(Q{7, 5})));  // open: 1.4 ∉
    CHECK(static_cast<bool>(nbhd(Q{145, 100})));           // codecov
  }

  SECTION("completion: a rational neighborhood PINS a real — (7/5,3/2) ∋ √2") {
    // The same open interval, read over the real carrier, catches √2: the
    // continuum is the limit of shrinking rational neighborhoods.  And it is
    // STILL a first-class ETCS set (over the reals now).
    constexpr auto rn = make_interval<Strictness::Strict, Strictness::Strict>(
        Cut<>{Q{7, 5}}, Cut<>{Q{3, 2}});
    STATIC_CHECK(dedekind::sets::IsSetObject<decltype(rn)>);
    STATIC_CHECK(static_cast<bool>(rn(Cut<>::sqrt(Q{2}))));  // 7/5 < √2 < 3/2
    CHECK(static_cast<bool>(rn(Cut<>::sqrt(Q{2}))));         // codecov
  }

  SECTION("non-Boolean logic: a Kleene interval composes through L::AND") {
    // The interval's operator() must combine its rays via L::AND, not built-in
    // &&, so it works for a non-Boolean classifier (Ternary is a scoped enum).
    // Guards against a regression on interval.cppm's membership.
    using dedekind::category::Kleene;
    using dedekind::category::Ternary;
    constexpr auto ti =
        make_interval<Strictness::Strict, Strictness::Strict, Kleene>(1, 5);
    STATIC_CHECK(ti(3) == Ternary::True);   // 1 < 3 < 5
    STATIC_CHECK(ti(0) == Ternary::False);  // 0 ≤ 1 (open lower)
    STATIC_CHECK(ti(5) == Ternary::False);  // 5 ≥ 5 (open upper)
    CHECK(ti(3) == Ternary::True);          // codecov
  }
}
