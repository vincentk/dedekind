/** @file test/cpp/modules/dedekind/sets/quantifier_test.cpp
 *
 * Runtime exercises for :quantifier — the compile-time witnesses in the
 * partition are static_asserts (invisible to coverage), so the reductions
 * and combinators are also driven at run time here.
 */
#include <catch2/catch_test_macros.hpp>
#include <ranges>

import dedekind.sets;

using namespace dedekind::sets;

TEST_CASE("quantifier: forall / exists set operations over a range",
          "[sets][quantifier]") {
  CHECK(forall(std::views::iota(2, 8), [](int x) { return x > 1; }));
  CHECK_FALSE(forall(std::views::iota(0, 3), [](int x) { return x > 0; }));
  CHECK(exists(std::views::iota(0, 5), [](int x) { return x == 3; }));
  CHECK_FALSE(exists(std::views::iota(0, 3), [](int x) { return x > 9; }));
}
