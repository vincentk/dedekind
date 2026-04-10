/** @file test/cpp/modules/dedekind/category/topoi_test.cpp */
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

  SECTION("Disjunction (Union)") {
    auto is_even_or_positive = is_even || is_positive;

    STATIC_CHECK(IsPredicate<decltype(is_even_or_positive)>);
    CHECK(is_even_or_positive(2) == true);   // even
    CHECK(is_even_or_positive(3) == true);   // positive
    CHECK(is_even_or_positive(-2) == true);  // even
    CHECK(is_even_or_positive(-3) == false); // neither
  }

  SECTION("Negation (Complement)") {
    auto is_odd = !is_even;

    STATIC_CHECK(IsPredicate<decltype(is_odd)>);
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

TEST_CASE("Topos: IsCharacteristic Concept", "[category][topoi]") {
  // IsCharacteristic is a categorical alias for IsPredicate
  auto is_even = classify<int>([](int x) { return x % 2 == 0; });

  STATIC_CHECK(IsCharacteristic<decltype(is_even)>);
}

TEST_CASE("Topos: Constant Truth Morphisms", "[category][topoi]") {
  SECTION("logical_true morphism: 1 → Ω") {
    auto t = logical_true();
    CHECK(t(One{}) == true);
  }

  SECTION("logical_false morphism: 1 → Ω") {
    auto f = logical_false();
    CHECK(f(One{}) == false);
  }

  SECTION("Ternary logical_true/false") {
    auto t = logical_true<TernaryLogic>();
    auto f = logical_false<TernaryLogic>();
    CHECK(t(One{}) == Ternary::True);
    CHECK(f(One{}) == Ternary::False);
  }
}
