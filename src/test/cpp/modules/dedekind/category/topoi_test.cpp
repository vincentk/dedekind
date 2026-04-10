/** @file test/cpp/modules/dedekind/category/logic_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Topos: Point-Free Logic Composition", "[category][topoi]") {
  // 1. We now classify raw lambdas into formal Predicate arrows
  auto is_even = classify<int>([](int x) { return x % 2 == 0; });
  auto is_positive = classify<int>([](int x) { return x > 0; });

  SECTION("Conjunction (Intersection)") {
    // operator&& now requires both to be IsArrow
    auto is_even_and_positive = is_even && is_positive;

    STATIC_CHECK(IsPredicate<decltype(is_even_and_positive)>);
    CHECK(is_even_and_positive(2) == true);
    CHECK(is_even_and_positive(-2) == false);
    CHECK(is_even_and_positive(3) == false);
  }

  SECTION("Negation (Complement)") {
    auto is_odd = !is_even;

    CHECK(is_odd(1) == true);
    CHECK(is_odd(2) == false);
  }

  SECTION("Ternary Logic Support") {
    // Verify that the sovereign logic resolves correctly for non-bool Ω
    auto t_true = classify<int>([](int) { return Ternary::True; });
    auto t_unknown = classify<int>([](int) { return Ternary::Unknown; });

    auto result = t_true && t_unknown;
    CHECK(result(0) == Ternary::Unknown);
  }
}