/** @file test/cpp/modules/dedekind/category/topoi_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Topos: Point-Free Logic Composition", "[category][topoi]") {
  // 1. Materialise Subobjects (Bodies)
  auto s_even = classify<int>([](int x) { return x % 2 == 0; });
  auto s_positive = classify<int>([](int x) { return x > 0; });

  SECTION("Conjunction (Intersection)") {
    // 2. Extract the characteristic arrows (Rules) for composition
    auto χ_both = s_even.χ && s_positive.χ;

    // 3. Materialise the resulting intersection as a new Subobject
    auto s_both = classify<int>(χ_both);

    STATIC_CHECK(IsSubobject<decltype(s_both), int>);
    // We check the internal rule χ to verify membership
    CHECK(s_both.χ(2) == true);
    CHECK(s_both.χ(-2) == false);
    CHECK(s_both.χ(3) == false);
  }

  SECTION("Disjunction (Union)") {
    auto χ_either = s_even.χ || s_positive.χ;
    auto s_either = classify<int>(χ_either);

    STATIC_CHECK(IsSubobject<decltype(s_either), int>);
    CHECK(s_either.χ(2) == true);    // even
    CHECK(s_either.χ(3) == true);    // positive
    CHECK(s_either.χ(-2) == true);   // even
    CHECK(s_either.χ(-3) == false);  // neither
  }

  SECTION("Negation (Complement)") {
    auto χ_odd = !s_even.χ;
    auto s_odd = classify<int>(χ_odd);

    STATIC_CHECK(IsSubobject<decltype(s_odd), int>);
    CHECK(s_odd.χ(1) == true);
    CHECK(s_odd.χ(2) == false);
  }

  SECTION("Ternary Logic Support") {
    // Note: use arrow directly to stay in the morphic layer for composition
    auto t_true = arrow<int>([](int) { return Ternary::True; });
    auto t_unknown = arrow<int>([](int) { return Ternary::Unknown; });

    auto χ_res = t_true && t_unknown;
    CHECK(χ_res(0) == Ternary::Unknown);
  }

  SECTION("Textbook set-operator laws via characteristic morphisms") {
    auto p = classify<int>([](int x) { return x % 2 == 0; }).χ;
    auto q = classify<int>([](int x) { return x > 0; }).χ;
    auto r = classify<int>([](int x) { return x < 3; }).χ;

    for (int x : {-2, -1, 0, 1, 2, 3, 4}) {
      CHECK((p && q)(x) == (q && p)(x));
      CHECK((p || q)(x) == (q || p)(x));

      CHECK(((p && q) && r)(x) == (p && (q && r))(x));
      CHECK(((p || q) || r)(x) == (p || (q || r))(x));

      CHECK((p && (q || r))(x) == ((p && q) || (p && r))(x));
      CHECK((p || (q && r))(x) == ((p || q) && (p || r))(x));

      CHECK((p || (p && q))(x) == p(x));
      CHECK((p && (p || q))(x) == p(x));
    }
  }
}

TEST_CASE("Topos: IsCharacteristic Concept", "[category][topoi]") {
  // IsCharacteristic is a categorical alias for IsPredicate (The Arrow)
  auto χ = arrow<int>([](int x) { return x % 2 == 0; });

  STATIC_CHECK(IsCharacteristic<decltype(χ)>);

  SECTION("Arrow evaluation at domain points") {
    CHECK(χ(0) == true);
    CHECK(χ(1) == false);
    CHECK(χ(4) == true);
    CHECK(χ(-1) == false);
  }

  SECTION("Arrow composition with logical operators") {
    auto positive = arrow<int>([](int x) { return x > 0; });
    auto composed = χ && positive;

    CHECK(composed(2) == true);    // 2 is even and positive
    CHECK(composed(1) == false);   // 1 is positive but odd
    CHECK(composed(-2) == false);  // -2 is even but negative
  }
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

  SECTION("Constant morphisms composed with characteristic morphisms") {
    auto t = logical_true<ClassicalLogic>();
    auto p = arrow<int>([](int x) { return x % 2 == 0; });

    // true && p(x) == p(x)
    auto result = arrow<int>([p](int x) { return t(One{}) && p(x); });
    CHECK(result(2) == true);
    CHECK(result(3) == false);
  }
}

TEST_CASE("Topos: Arrow Operations Closure", "[category][topoi]") {
  auto even = arrow<int>([](int x) { return x % 2 == 0; });
  auto positive = arrow<int>([](int x) { return x > 0; });
  auto small = arrow<int>([](int x) { return x < 10; });

  SECTION("Conjunction associativity of arrows") {
    auto res1 = (even && positive) && small;
    auto res2 = even && (positive && small);

    for (int x : {0, 1, 2, 3, 5, 9, 10, -2}) {
      CHECK(res1(x) == res2(x));
    }
  }

  SECTION("Disjunction associativity of arrows") {
    auto res1 = (even || positive) || small;
    auto res2 = even || (positive || small);

    for (int x : {-5, -2, -1, 0, 1, 2, 15}) {
      CHECK(res1(x) == res2(x));
    }
  }

  SECTION("Distributivity of arrows") {
    auto res1 = even && (positive || small);
    auto res2 = (even && positive) || (even && small);

    for (int x : {-4, -2, 0, 1, 2, 4, 8, 12}) {
      CHECK(res1(x) == res2(x));
    }
  }
}
