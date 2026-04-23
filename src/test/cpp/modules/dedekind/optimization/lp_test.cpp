/** @file dedekind/optimization/lp_test.cpp
 *
 * Unit coverage for the `:lp` partition of `dedekind.optimization`.
 * Phase 1 (this file): smoke test the `Halfspace2D` membership predicate.
 * Phase 2: the `maximize(objective, polytope) → Singleton<vertex>`
 * reduction — to be populated once the reduction lands.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.numbers;
import dedekind.optimization;

using namespace dedekind::numbers;
using namespace dedekind::optimization;

namespace {
using Rat = Rational<long>;
}  // namespace

TEST_CASE("optimization:lp — Halfspace2D membership at the type level",
          "[optimization][lp][halfspace]") {
  // x + y ≤ 4
  using H = Halfspace2D<Rat, Rat{1L}, Rat{1L}, Rat{4L}>;

  // (2, 2) is on the boundary (2 + 2 = 4).
  STATIC_CHECK(H::template contains<Rat{2L}, Rat{2L}>());
  // (1, 1) is strictly interior.
  STATIC_CHECK(H::template contains<Rat{1L}, Rat{1L}>());
  // (3, 3) violates (3 + 3 = 6 > 4).
  STATIC_CHECK_FALSE(H::template contains<Rat{3L}, Rat{3L}>());
}
