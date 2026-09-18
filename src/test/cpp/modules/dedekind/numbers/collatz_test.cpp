/** @file dedekind/numbers/collatz_test.cpp
 *
 * Runtime coverage for the bounded-Collatz reachability exhibit (§4).  The
 * load-bearing facts are compile-time @c static_assert s in @c :collatz; these
 * runtime @c CHECK s make the same witnesses visible to Codecov and pin the
 * operational behaviour (a @c static_assert is invisible to line coverage).
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>

import dedekind.category;
import dedekind.numbers;

using namespace dedekind::numbers;
using dedekind::category::Ternary;

TEST_CASE("numbers:collatz — the step map ℕ → ℕ", "[numbers][collatz]") {
  CHECK(collatz_step(1) == 4);    // odd: 3·1+1
  CHECK(collatz_step(4) == 2);    // even: 4/2
  CHECK(collatz_step(2) == 1);    // even: 2/2
  CHECK(collatz_step(27) == 82);  // odd: 3·27+1
}

TEST_CASE("numbers:collatz — reach time is the first index hitting 1",
          "[numbers][collatz]") {
  // 6 → 3 → 10 → 5 → 16 → 8 → 4 → 2 → 1  (reaches 1 at index 8).
  REQUIRE(collatz_reach_time(6, 100).has_value());
  CHECK(collatz_reach_time(6, 100).value() == 8u);
  CHECK(collatz_reach_time(1, 0).value() == 0u);  // 1 is already there
  // Budget one step short of the trajectory: no reach witnessed.
  CHECK_FALSE(collatz_reach_time(6, 7).has_value());
}

TEST_CASE("numbers:collatz — the Rosolini verdict: IN / U (never OUT)",
          "[numbers][collatz][dominance]") {
  // 27 reaches 1 in ~111 steps: sufficient budget → IN, short budget → U.
  CHECK(reaches_1_within(27, 120) == Ternary::True);
  CHECK(reaches_1_within(27, 50) == Ternary::Unknown);
  CHECK(reaches_1_within(6, 8) == Ternary::True);
  CHECK(reaches_1_within(6, 7) == Ternary::Unknown);
  // There is no False: an undecided answer negates to itself under the honest
  // dominance — "never reaches 1" has no finite certificate for standard
  // Collatz, so the classifier only ever answers True or Unknown.
}

TEST_CASE("numbers:collatz — the collapse: a windowed+budgeted decidable ∀",
          "[numbers][collatz][collapse]") {
  // The open ∀-conjecture, restricted to a finite window and budget, decides.
  CHECK(all_reach_1_within<100, 200>());
  CHECK(all_reach_1_within<1000, 300>());
}

TEST_CASE("numbers:collatz — orbit and reach-indicator are sequences",
          "[numbers][collatz][sequence]") {
  const auto orbit = collatz_orbit(6);
  CHECK(orbit.at(0) == 6u);
  CHECK(orbit.at(8) == 1u);
  // The reach indicator is Unknown before 1 appears and absorbs to True after.
  const auto reach = collatz_reach_path(6);
  CHECK(reach.at(0) == Ternary::Unknown);  // 6 ≠ 1
  CHECK(reach.at(20) == Ternary::True);    // reached (step 8) and stays True
}
