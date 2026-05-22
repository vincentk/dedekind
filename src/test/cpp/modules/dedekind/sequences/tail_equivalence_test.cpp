/** @file dedekind/sequences/tail_equivalence_test.cpp
 *
 * Unit coverage for the tail-equivalence congruence on Path (the
 * sequences↔quotients cross-cutter): @c TailEquivalent and its
 * registration as an @c IsCongruence w.r.t. pointwise +.
 *
 * Tail-equivalence s ~ t ⟺ ∃N ∀n≥N. s(n)=t(n) — agreement past a finite
 * prefix.  The runtime relation reads the operational window; the
 * type-level congruence (asserted in tail_equivalence.cppm for Path<int>)
 * is the structural claim.
 *
 * Coverage:
 *  - the sampled relation: agreement past a prefix ⇒ tail-equivalent;
 *    a difference inside the window ⇒ not; reflexivity.
 *  - the congruence's computational content: s~s' ∧ t~t' ⇒ s+t ~ s'+t'.
 *  - the absorptive connection: an eventually-constant path is
 *    tail-equivalent to the constant path at its limit.
 *  - the congruence fires at a second integral instantiation (Path<long>)
 *    and is honestly rejected over float (Path<double>, NaN policy).
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>

import dedekind.sequences;
import dedekind.category;

using namespace dedekind::sequences;

namespace {

// s and t differ only on the first `prefix_len` indices, then agree.
Path<int> ramp_then(int tail_value, int head_value, std::size_t prefix_len) {
  return Path<int>{
      [=](std::size_t n) { return n < prefix_len ? head_value : tail_value; }};
}

}  // namespace

TEST_CASE("sequences:tail — the sampled relation detects eventual agreement",
          "[sequences][tail][relation]") {
  const TailEquivalent<int> tail_eq{};  // default window [0, 64)
  const auto a = ramp_then(7, 1, 3);    // 1,1,1,7,7,7,…
  const auto b = ramp_then(7, 2, 3);    // 2,2,2,7,7,7,… — agrees from index 3
  const auto c = Path<int>{[](std::size_t n) { return static_cast<int>(n); }};

  // a and b differ only on [0,3) and agree from index 3 onward.  A window
  // STARTING at 3 sees only the agreeing tail ⇒ tail-equivalent…
  const TailEquivalent<int> tail_from_3{.window_start = 3, .window_span = 64};
  REQUIRE(tail_from_3(a, b));
  // …while the default window starts at 0 and so catches the differing
  // prefix [0,3):
  REQUIRE_FALSE(tail_eq(a, b));
  // a is not tail-equivalent to the unbounded ramp c:
  REQUIRE_FALSE(tail_eq(a, c));
  // Reflexivity:
  REQUIRE(tail_eq(c, c));
}

TEST_CASE("sequences:tail — congruence content: s~s' ∧ t~t' ⇒ s+t ~ s'+t'",
          "[sequences][tail][congruence]") {
  // s, s' agree from index 2; t, t' agree from index 4.
  const auto s = ramp_then(10, 0, 2);
  const auto s_prime = ramp_then(10, 99, 2);
  const auto t = ramp_then(20, 0, 4);
  const auto t_prime = ramp_then(20, 99, 4);

  const auto sum = s + t;
  const auto sum_prime = s_prime + t_prime;

  // The sums agree from index max(2,4) = 4 onward — witness on a window
  // starting at 4.
  const TailEquivalent<int> tail_from_4{.window_start = 4, .window_span = 64};
  REQUIRE(tail_from_4(sum, sum_prime));
}

TEST_CASE(
    "sequences:tail — an eventually-constant path is tail-equivalent to "
    "its constant (absorptive connection)",
    "[sequences][tail][absorptive]") {
  // Eventually constant at 42 from index 5; the absorptive shape's germ.
  const auto eventually_42 = ramp_then(42, -1, 5);
  const auto constant_42 = Path<int>{[](std::size_t) { return 42; }};

  const TailEquivalent<int> tail_from_5{.window_start = 5, .window_span = 64};
  REQUIRE(tail_from_5(eventually_42, constant_42));
}

TEST_CASE(
    "sequences:tail — the congruence fires at a second (integral) "
    "instantiation, and is honestly rejected over float",
    "[sequences][tail][congruence][honest-rejection]") {
  // Non-vacuous re-instantiation at Path<long> (the main-file static_assert
  // pins Path<int>); long's equality is reflexive, so the congruence fires.
  STATIC_CHECK(
      dedekind::category::IsCongruence<TailEquivalent<long>, Path<long>,
                                       std::plus<Path<long>>>);
  // Over double the carrier's equality is not reflexive (NaN), so
  // tail-equivalence is not an equivalence relation and not a congruence —
  // same policy as std::equal_to<double>.
  STATIC_CHECK_FALSE(
      dedekind::category::IsCongruence<TailEquivalent<double>, Path<double>,
                                       std::plus<Path<double>>>);
}
