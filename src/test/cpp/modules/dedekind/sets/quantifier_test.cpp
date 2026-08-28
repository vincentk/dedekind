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

TEST_CASE("quantifier: max / min are the extremal sets (membership)",
          "[sets][quantifier][extrema]") {
  // The extremum of [2,6) is a singleton set; membership is max(s)(x).
  const auto s = std::views::iota(2, 6);
  CHECK(max(s)(5));
  CHECK_FALSE(max(s)(4));
  CHECK_FALSE(max(s)(6));  // 6 ∉ [2,6): not in the domain at all
  CHECK(min(s)(2));
  CHECK_FALSE(min(s)(3));
}

TEST_CASE("quantifier: argmax / argmin return the optimiser fibre",
          "[sets][quantifier][argmax]") {
  const auto d = std::views::iota(0, 7);               // {0,…,6}
  const auto cap = [](int x) { return x * (6 - x); };  // concave, peak at 3

  // Unique optimiser: argmax is a function (singleton fibre {3}).
  CHECK(argmax(d, cap)(3));
  CHECK_FALSE(argmax(d, cap)(2));
  // A tie: x mod 2 is maximised by every odd argument (fibre {1,3,5}).
  const auto odd = [](int x) { return x % 2; };
  CHECK(argmax(d, odd)(1));
  CHECK(argmax(d, odd)(3));
  CHECK(argmax(d, odd)(5));
  CHECK_FALSE(argmax(d, odd)(2));
  // argmin of the concave map sits at both ends {0,6}.
  CHECK(argmin(d, cap)(0));
  CHECK(argmin(d, cap)(6));
  CHECK_FALSE(argmin(d, cap)(3));
}
