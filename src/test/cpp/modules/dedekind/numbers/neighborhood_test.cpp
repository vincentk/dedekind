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
 * and no enumeration.  Order's two-sided cut @c Interval is a set object
 * (@c IsSetObject) whose openness reads off the strictness of its bounds, so
 * one open interval is all of these at once.
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
using dedekind::order::Interval;
using dedekind::order::Strictness;
using dedekind::topology::is_order_closed;
using dedekind::topology::is_order_open;

namespace {
using Q = Rational<>;
constexpr auto open_ = Strictness::Strict;
constexpr auto closed_ = Strictness::NonStrict;
// A rational neighborhood (7/5, 3/2) ⊂ ℚ — an open interval between two ℚ.
constexpr auto nbhd = Interval<Q>::bounded(Q{7, 5}, open_, Q{3, 2}, open_);
using QNbhd = std::remove_cvref_t<decltype(nbhd)>;
}  // namespace

TEST_CASE("a rational neighborhood is a topological neighborhood AND a Lwv set",
          "[numbers][neighborhood][topology][lwv]") {
  SECTION("Lwv/ETCS: a first-class set, over a regular (Jlt) carrier") {
    STATIC_CHECK(dedekind::sets::IsSetObject<QNbhd>);
    STATIC_CHECK(std::regular<Q>);  // the Jlt value-semantics half of Lwv
  }

  SECTION(
      "dense carrier ⟹ the genuine open-⊋-clopen witness (#905): ℚ is not "
      "discrete, so an open interval is open but NOT closed") {
    // The independence direction int (discrete) cannot provide: on a dense
    // carrier an open shape is open, not closed, hence not clopen --- yet its
    // membership is Boole-decidable.
    using dedekind::topology::HasDiscreteCarrier;
    constexpr auto ray = Interval<Q>::half(Q{1}, Direction::Upward, open_);
    STATIC_CHECK(!HasDiscreteCarrier<QNbhd>);
    STATIC_CHECK(is_order_open(ray) && !is_order_closed(ray));
    STATIC_CHECK(is_order_open(nbhd) && !is_order_closed(nbhd));
    STATIC_CHECK(dedekind::sets::HasDecidableMembership<QNbhd>);
    CHECK(!is_order_closed(ray));
  }

  SECTION(
      "dense carrier: open / closed are read off the sides; a mixed one is "
      "neither") {
    using dedekind::topology::IsConvex;
    constexpr auto closed_iv =
        Interval<Q>::bounded(Q{0}, closed_, Q{3}, closed_);
    constexpr auto left_closed =
        Interval<Q>::bounded(Q{0}, closed_, Q{3}, open_);
    STATIC_CHECK(is_order_closed(closed_iv) && !is_order_open(closed_iv));
    STATIC_CHECK(!is_order_open(left_closed) && !is_order_closed(left_closed));
    STATIC_CHECK(IsConvex<decltype(left_closed)>);
    STATIC_CHECK(static_cast<bool>(closed_iv(Q{3})));     // closed: 3 ∈ [0,3]
    STATIC_CHECK(!static_cast<bool>(left_closed(Q{3})));  // open above: 3 ∉
    CHECK(static_cast<bool>(left_closed(Q{0})));          // codecov
  }

  SECTION("intensional, decidable characteristic map χ (no enumeration)") {
    STATIC_CHECK(static_cast<bool>(nbhd(Q{143, 100})));    // 1.43 ∈ (1.4,1.5)
    STATIC_CHECK_FALSE(static_cast<bool>(nbhd(Q{2})));     // 2 ∉
    STATIC_CHECK_FALSE(static_cast<bool>(nbhd(Q{7, 5})));  // open: 1.4 ∉
    CHECK(static_cast<bool>(nbhd(Q{145, 100})));           // codecov
  }

  SECTION("completion: a rational neighborhood PINS a real — (7/5,3/2) ∋ √2") {
    constexpr auto rn =
        Interval<Cut<>>::bounded(Cut<>{Q{7, 5}}, open_, Cut<>{Q{3, 2}}, open_);
    STATIC_CHECK(dedekind::sets::IsSetObject<decltype(rn)>);
    STATIC_CHECK(static_cast<bool>(rn(Cut<>::sqrt(Q{2}))));  // 7/5 < √2 < 3/2
    CHECK(static_cast<bool>(rn(Cut<>::sqrt(Q{2}))));         // codecov
  }

  SECTION("non-Boolean logic: a Kleene interval answers in its species") {
    using dedekind::category::Kleene;
    using dedekind::category::Ternary;
    constexpr auto ti = Interval<int, Kleene>::bounded(1, open_, 5, open_);
    STATIC_CHECK(ti(3) == Ternary::True);   // 1 < 3 < 5
    STATIC_CHECK(ti(0) == Ternary::False);  // 0 ≤ 1 (open lower)
    STATIC_CHECK(ti(5) == Ternary::False);  // 5 ≥ 5 (open upper)
    CHECK(ti(3) == Ternary::True);          // codecov
  }
}
