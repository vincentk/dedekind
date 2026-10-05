/** @file dedekind/numbers/cut_test.cpp
 *
 * @brief The decidable real interval @f$(-\sqrt2,\sqrt2)@f$ over the genuine
 * cut-real carrier @c Cut<>, hosted on order's value-carrying two-sided cut
 * @c Interval (@c make_interval).
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
#include <type_traits>

import dedekind.category; // Ternary (the ℚ-shadow's logic species)
import dedekind.numbers;
import dedekind.order;
import dedekind.topology;

using namespace dedekind::numbers;
using dedekind::category::IsLSet;
using dedekind::order::make_interval;
using dedekind::order::pivot;
using dedekind::order::Strictness;
using dedekind::topology::IsClosed;
using dedekind::topology::IsOpen;

namespace {
using Q = Rational<>;
constexpr Cut<> two_root = Cut<>::sqrt(Q{2});  // √2
// (−√2, √2) as a real interval — irrational, runtime-pivot bounds.
constexpr auto band =
    make_interval<Strictness::Strict, Strictness::Strict>(-two_root, two_root);

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

  SECTION("radical vs radical: the both-radical magnitude branch") {
    // Distinct radicands (same sign) exercise compare()'s magnitude path.
    STATIC_CHECK(Cut<>::sqrt(Q{2}) < Cut<>::sqrt(Q{3}));    // √2 < √3
    STATIC_CHECK(-Cut<>::sqrt(Q{3}) < -Cut<>::sqrt(Q{2}));  // −√3 < −√2
    CHECK(Cut<>::sqrt(Q{2}) < Cut<>::sqrt(Q{3}));           // codecov
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

TEST_CASE(
    "the cut IS its lower set: a set over ℚ, and ℚ ↪ ℝ is the principal ray",
    "[numbers][cut][real][sets]") {
  SECTION("the lower set is a set over ℚ whose χ is contains") {
    constexpr auto below_root = lower_set(two_root);  // {q ∈ ℚ | q < √2}
    STATIC_CHECK(IsLSet<std::remove_cvref_t<decltype(below_root)>>);
    STATIC_CHECK(below_root(Q{1}));
    STATIC_CHECK(below_root(Q{7, 5}));
    STATIC_CHECK_FALSE(below_root(Q{3, 2}));
    STATIC_CHECK_FALSE(below_root(Q{2}));
    CHECK(below_root(Q{1}) == two_root.contains(Q{1}));
    CHECK(below_root(Q{2}) == two_root.contains(Q{2}));
  }
  SECTION("a rational's lower set is the order datum's ray {q < p}") {
    constexpr Q p{3, 2};
    constexpr auto ray = principal_ray(p);
    constexpr auto down = lower_set(Cut<>{p});
    STATIC_CHECK(pivot(ray) == p);
    STATIC_CHECK(ray(Q{1}) == down(Q{1}));
    STATIC_CHECK(ray(p) == down(p));
    STATIC_CHECK(ray(Q{2}) == down(Q{2}));
    STATIC_CHECK_FALSE(ray(p));  // strict: the bound is not below itself
    // The order topology reads the datum: {q < p} open, {q ≥ p} closed.
    using Ray = PrincipalRay<Q>;
    using CoRay = std::remove_cvref_t<decltype(~ray)>;
    STATIC_CHECK(IsOpen<Ray>);
    STATIC_CHECK(IsClosed<CoRay>);
    STATIC_CHECK(pivot(~ray) == p);
  }
}
