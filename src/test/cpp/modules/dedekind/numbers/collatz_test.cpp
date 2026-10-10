/** @file dedekind/numbers/collatz_test.cpp
 *
 * Runtime coverage for the bounded-Collatz reachability exhibit (§4).  The
 * load-bearing facts are compile-time @c static_assert s in @c :collatz; these
 * runtime @c CHECK s make the same witnesses visible to Codecov and pin the
 * operational behaviour (a @c static_assert is invisible to line coverage).
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <limits>
#include <utility>

import dedekind.category;
import dedekind.numbers;
import dedekind.sequences;
import dedekind.sets;

using namespace dedekind::numbers;
using dedekind::category::Ternary;
using dedekind::sets::finite_cardinality;

TEST_CASE("numbers:collatz — the recurrence, explicit and as a relation",
          "[numbers][collatz]") {
  // The explicit named rule ℕ → ℕ.
  CHECK(collatz_step(1) == 4);    // odd: 3·1+1
  CHECK(collatz_step(4) == 2);    // even: 4/2
  CHECK(collatz_step(2) == 1);    // even: 2/2
  CHECK(collatz_step(27) == 82);  // odd: 3·27+1
  // The point-free relation, validated on pairs (ℕ = 𝔸<Cardinality>).
  CHECK(collatz(std::pair{finite_cardinality(6), finite_cardinality(3)}));
  CHECK(collatz(std::pair{finite_cardinality(7), finite_cardinality(22)}));
  CHECK(!collatz(std::pair{finite_cardinality(6), finite_cardinality(4)}));
  // The shadow saturates where 3n+1 leaves the word; the top is a fixpoint.
  constexpr auto top = std::numeric_limits<std::size_t>::max();
  CHECK(collatz_step(top) == top);
  CHECK(collatz_step(top / 3 + 2) == top);  // odd, and 3n+1 overflows
}

TEST_CASE("numbers:collatz — two steps, and the attractor's pre-image",
          "[numbers][collatz][relational]") {
  const auto n = [](std::size_t i) { return finite_cardinality(i); };
  CHECK(collatz2(std::pair{n(4), n(1)}));   // 4 → 2 → 1
  CHECK(collatz2(std::pair{n(6), n(10)}));  // 6 → 3 → 10
  CHECK(!collatz2(std::pair{n(6), n(5)}));
  CHECK(converged_2(n(4)));   // captured by {1, 2, 4}
  CHECK(converged_2(n(1)));   // already inside the cycle
  CHECK(!converged_2(n(3)));  // 3 → 10 → 5
  CHECK(pending_2(n(3)));
  CHECK(!pending_2(n(4)));
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
  CHECK(reaches_1_within(2, 0) == Ternary::Unknown);  // budget 0: no step
  CHECK(reaches_1_within(std::numeric_limits<std::size_t>::max(), 8) ==
        Ternary::Unknown);  // saturated orbit: undecided, never wrapped
  // There is no False: an undecided answer negates to itself under the honest
  // dominance — "never reaches 1" has no finite certificate for standard
  // Collatz, so the classifier only ever answers True or Unknown.
}

TEST_CASE("numbers:collatz — the collapse: a windowed+budgeted decidable ∀",
          "[numbers][collatz][collapse]") {
  // The open ∀-conjecture, restricted to a finite window and budget, decides.
  CHECK(all_reach_1_within<100, 118>());
  CHECK_FALSE(all_reach_1_within<100, 117>());  // 97 needs 118
  CHECK(all_reach_1_within<1000, 178>());
  CHECK_FALSE(all_reach_1_within<1000, 177>());  // 871 needs 178
}

TEST_CASE("numbers:collatz — orbit and reach-indicator are sequences",
          "[numbers][collatz][sequence]") {
  const auto orbit = collatz_orbit(6);
  CHECK(orbit.at(0) == 6u);
  CHECK(orbit.at(8) == 1u);
  // The reach indicator is Unknown before 1 appears and absorbs to True after.
  const auto reach = collatz_reach_path(6);
  STATIC_CHECK(dedekind::sequences::IsAbsorptiveSequence<ReachPath>);  // the
  // registration is on the type; a const-qualified decltype is not it
  CHECK(reach.at(0) == Ternary::Unknown);  // 6 ≠ 1
  CHECK(reach.at(20) == Ternary::True);    // reached (step 8) and stays True
}
