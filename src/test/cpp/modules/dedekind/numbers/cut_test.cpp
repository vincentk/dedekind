/** @file dedekind/numbers/cut_test.cpp
 *
 * @brief The decidable real interval @f$(-\sqrt2,\sqrt2)@f$ over the genuine
 * cut-real carrier @c Cut<>, hosted on the runtime-pivot @c topology::Interval.
 *
 * The load-bearing claim: a real interval whose bounds are @b irrational
 * (@f$\pm\sqrt2@f$) has @b decidable membership at every rational point, and
 * that decision collapses to @f$q^2<2@f$ --- so the interval is exactly the
 * @f$\mathbb{Q}@f$-shadow @c Sqrt2_Symbolic already exhibits.  This is what the
 * §5 wave exhibit needs: real intervals with named-irrational bounds, decided
 * at compile time.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category; // Ternary (the ℚ-shadow's logic species)
import dedekind.numbers;
import dedekind.order;
import dedekind.topology;

using namespace dedekind::numbers;
using dedekind::topology::Boundary;
using dedekind::topology::Interval;

namespace {
using Q = Rational<>;
constexpr Cut<> two_root = Cut<>::sqrt(Q{2});  // √2
// (−√2, √2) as a real interval — irrational, runtime-pivot bounds.
constexpr Interval<Cut<>, Boundary::Open, Boundary::Open> band{-two_root,
                                                               two_root};

// Membership of a rational point q, decided by q² <=> 2.
constexpr bool in_band(long n) { return static_cast<bool>(band(Cut<>{n})); }
}  // namespace

TEST_CASE("(-√2, √2): a decidable real interval with irrational bounds",
          "[numbers][cut][real][interval]") {
  SECTION("the carrier is a genuine totally-ordered real species") {
    STATIC_CHECK(dedekind::order::IsTotallyOrdered<Cut<>>);
    STATIC_CHECK(std::regular<Cut<>>);
  }

  SECTION("membership at rational points collapses to q² < 2 (decidable)") {
    STATIC_CHECK(in_band(0));         // 0 < 2
    STATIC_CHECK(in_band(1));         // 1 < 2
    STATIC_CHECK(in_band(-1));        // 1 < 2
    STATIC_CHECK_FALSE(in_band(2));   // 4 > 2
    STATIC_CHECK_FALSE(in_band(-2));  // 4 > 2
    // Non-integer rationals straddling √2 ≈ 1.4142…
    STATIC_CHECK(static_cast<bool>(band(Cut<>{Q{7, 5}})));        // 1.4² < 2
    STATIC_CHECK_FALSE(static_cast<bool>(band(Cut<>{Q{3, 2}})));  // 1.5² > 2
  }

  SECTION("coherence: the interval IS the ℚ-shadow Sqrt2_Symbolic exhibits") {
    // Sqrt2_Symbolic<Q>() classifies {q : q² < 2} = (−√2, √2)∩ℚ.  Same verdict
    // (Ternary::True ↦ member; note Ternary::False = −1, so compare the tag).
    using dedekind::category::Ternary;
    const auto shadow = Sqrt2_Symbolic<Q>();
    STATIC_CHECK((shadow(Q{1}) == Ternary::True) == in_band(1));
    STATIC_CHECK((shadow(Q{2}) == Ternary::True) == in_band(2));
  }

  SECTION("runtime exercise (codecov): the same decisions at runtime") {
    CHECK(in_band(0));
    CHECK(in_band(1));
    CHECK_FALSE(in_band(2));
    CHECK(two_root.contains(Q{1}));        // 1 < √2
    CHECK_FALSE(two_root.contains(Q{2}));  // ¬(2 < √2)
    CHECK((-two_root) < two_root);
  }
}
