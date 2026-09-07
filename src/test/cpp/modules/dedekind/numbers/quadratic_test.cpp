/** @file dedekind/numbers/quadratic_test.cpp
 *
 * @brief @f$\mathbb{Q}(\sqrt2)@f$ as a genuine, decidable quadratic real field,
 * and the coherence that its @f$\sqrt2@f$ is the @b same real as @c Cut's.
 *
 * The field laws are already witnessed at compile time inside
 * @c quadratic.cppm (the honest contrast with a postulated field); here we add
 * runtime coverage and the cross-carrier coherence witness: the decidable
 * order-leaf (@c Cut) and the decidable field (@c QuadraticReal) must place
 * @f$\sqrt2@f$ at the same point among the rationals, or the two real carriers
 * would have drifted apart.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <concepts>

import dedekind.algebra;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::numbers;

namespace {
using Q = Rational<>;
using R2 = QuadraticReal<2>;
constexpr R2 r2 = R2::root();            // √2 as a field element
constexpr Cut<> c2 = Cut<>::sqrt(Q{2});  // √2 as an order-leaf

// The two √2's agree on where √2 sits relative to the integer n.
constexpr bool same_place(long n) {
  return (Cut<>{n} <=> c2) == (R2{n} <=> r2);
}
}  // namespace

TEST_CASE("ℚ(√2): a decidable quadratic real field", "[numbers][quadratic]") {
  SECTION("the carrier is a totally-ordered field (concepts)") {
    STATIC_CHECK(dedekind::order::IsTotallyOrdered<R2>);
    STATIC_CHECK(dedekind::algebra::HasFieldOperators<R2>);
    STATIC_CHECK(std::regular<R2>);
  }

  SECTION("field ops close and compute exactly (codecov)") {
    CHECK(r2 * r2 == R2{2});                       // √2·√2 = 2
    CHECK((R2{1} + r2) * (R2{1} - r2) == R2{-1});  // conjugate norm
    CHECK(r2 * r2.inverse() == R2{1});             // √2·(1/√2) = 1
    CHECK(R2{1} / r2 == R2::of(Q{}, Q{1, 2}));     // 1/√2 = ½√2
    CHECK((r2 + R2{3}) - R2{3} == r2);             // additive inverse
  }

  SECTION("total, decidable order") {
    CHECK(R2{1} < r2);
    CHECK(r2 < R2{2});
    CHECK(r2 < R2{Q{3, 2}});  // √2 < 1.5  (1.5² = 2.25 > 2)
    CHECK(R2{Q{7, 5}} < r2);  // 1.4 < √2  (1.4² = 1.96 < 2)
    CHECK(-r2 < r2);
  }

  SECTION("coherence: Cut's √2 and ℚ(√2)'s √2 are the SAME real") {
    STATIC_CHECK(same_place(0));
    STATIC_CHECK(same_place(1));
    STATIC_CHECK(same_place(2));
    STATIC_CHECK(same_place(-2));
    CHECK(same_place(1));  // codecov
  }

  SECTION("mixed-sign order: both branches of sign_of") {
    // a>0,b<0 (the sq branch) and a<0,b>0 (the reversed branch).
    STATIC_CHECK(R2::of(Q{-2}, Q{1}) < R2{});  // −2+√2 < 0   (a²>b²D, reversed)
    STATIC_CHECK(R2{} < R2::of(Q{-1}, Q{1}));  // 0 < −1+√2   (a²<b²D, reversed)
    STATIC_CHECK(R2::of(Q{2}, Q{-1}) > R2{});  // 2−√2 > 0    (a²>b²D, sq)
    CHECK(R2::of(Q{-2}, Q{1}) < R2{});         // codecov
  }
}
