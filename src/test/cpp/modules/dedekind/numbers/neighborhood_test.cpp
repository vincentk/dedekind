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

import dedekind.category; // IsSet (the ETCS axioms)
import dedekind.numbers;  // Rational, Cut
import dedekind.order;
import dedekind.topology; // Interval, Boundary, IsOpen, IsNeighborhood

using namespace dedekind::numbers;
using dedekind::topology::Boundary;
using dedekind::topology::Interval;

namespace {
using Q = Rational<>;
// A rational neighborhood (7/5, 3/2) ⊂ ℚ — an open interval between two ℚ.
using QNbhd = Interval<Q, Boundary::Open, Boundary::Open>;
constexpr QNbhd nbhd{Q{7, 5}, Q{3, 2}};
}  // namespace

TEST_CASE("a rational neighborhood is a topological neighborhood AND a Lwv set",
          "[numbers][neighborhood][topology][lwv]") {
  SECTION("Lwv/ETCS: a first-class set, over a regular (Jlt) carrier") {
    STATIC_CHECK(dedekind::category::IsSet<QNbhd>);
    STATIC_CHECK(std::regular<Q>);  // the Jlt value-semantics half of Lwv
  }

  SECTION("topology: an open neighborhood of its points") {
    STATIC_CHECK(dedekind::topology::IsOpen<QNbhd>);
    STATIC_CHECK(dedekind::topology::IsNeighborhood<QNbhd, Q>);
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
    using RNbhd = Interval<Cut<>, Boundary::Open, Boundary::Open>;
    constexpr RNbhd rn{Cut<>{Q{7, 5}}, Cut<>{Q{3, 2}}};
    STATIC_CHECK(dedekind::category::IsSet<RNbhd>);
    STATIC_CHECK(static_cast<bool>(rn(Cut<>::sqrt(Q{2}))));  // 7/5 < √2 < 3/2
    CHECK(static_cast<bool>(rn(Cut<>::sqrt(Q{2}))));         // codecov
  }

  SECTION(
      "non-Boolean logic: a TernaryLogic interval composes through L::AND") {
    // The interval's operator() must combine its rays via L::AND, not built-in
    // &&, so it works for a non-Boolean classifier (Ternary is a scoped enum).
    // Guards against a regression on interval.cppm's membership.
    using dedekind::category::Ternary;
    using dedekind::category::TernaryLogic;
    constexpr Interval<int, Boundary::Open, Boundary::Open, TernaryLogic> ti{1,
                                                                             5};
    STATIC_CHECK(ti(3) == Ternary::True);   // 1 < 3 < 5
    STATIC_CHECK(ti(0) == Ternary::False);  // 0 ≤ 1 (open lower)
    STATIC_CHECK(ti(5) == Ternary::False);  // 5 ≥ 5 (open upper)
    CHECK(ti(3) == Ternary::True);          // codecov
  }
}
