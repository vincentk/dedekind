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

TEST_CASE("quantifier: ForAll / Exists combinators bind the inner variable",
          "[sets][quantifier]") {
  // Exists(dom, p2) = λx. ∃y∈dom. p2(y,x): {x | ∃y∈{6}. x·y==42} selects 7.
  const auto has42 =
      Exists(std::views::single(6), [](int y, int x) { return x * y == 42; });
  CHECK(has42(7));
  CHECK_FALSE(has42(8));

  // ForAll(dom, p2) = λx. ∀y∈dom. p2(y,x): {x | ∀y∈{2,3}. x%y==0} = multiples
  // of 6.
  const auto by6 =
      ForAll(std::views::iota(2, 4), [](int y, int x) { return x % y == 0; });
  CHECK(by6(6));
  CHECK_FALSE(by6(9));
}
