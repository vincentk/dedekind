#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Category: Functor Concepts", "[category][functor]") {
  using IntCat = DiscreteCategory<int>;
  auto plus_one = arrow([](int x) { return x + 1; });

  STATIC_CHECK(IsFunctor<identity_functor<IntCat>>);
  STATIC_CHECK(IsEndofunctor<identity_functor<IntCat>>);
  STATIC_CHECK(IsFunctor<box_functor<int>>);
  STATIC_CHECK(IsHubArrow<identity_functor<IntCat>>);
  STATIC_CHECK(IsHubArrow<box_functor<int>>);
  STATIC_CHECK(IsSpokeArrow<decltype(plus_one)>);
}

TEST_CASE("Category: Functor operator>> (Functor then Arrow)",
          "[category][functor][operator-shift]") {
  SECTION("Identity functor preserves arrow behavior") {
    identity_functor<DiscreteCategory<int>> idf;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = idf >> plus_one;

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted(41) == 42);
    CHECK(lifted(-2) == -1);
  }

  SECTION("Box functor lifts codomain into Box<T>") {
    box_functor<int> boxf;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = boxf >> plus_one;

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted(Box<int>{41}) == Box<int>{42});
    CHECK(lifted(Box<int>{-2}) == Box<int>{-1});
  }

  SECTION("Box functor identity case maps to boxed identity") {
    box_functor<int> boxf;
    auto id_int = id<int>();

    auto lifted_id = boxf >> id_int;

    CHECK(lifted_id(Box<int>{7}) == Box<int>{7});
    CHECK(lifted_id(Box<int>{0}) == Box<int>{0});
  }
}

TEST_CASE("Category: Functor operator>> (Functor composition)",
          "[category][functor][operator-shift]") {
  identity_functor<DiscreteCategory<int>> idf;
  box_functor<int> boxf;
  auto plus_one = arrow([](int x) { return x + 1; });
  auto composed = idf >> boxf;

  STATIC_CHECK(IsFunctor<decltype(composed)>);

  SECTION("Composed functor carries source and target category handles") {
    static_assert(std::same_as<typename decltype(composed)::Σ_cat,
                               DiscreteCategory<int>>);
    static_assert(std::same_as<typename decltype(composed)::Τ_cat,
                               DiscreteCategory<Box<int>>>);
    SUCCEED();
  }

  SECTION("Composed functor action matches direct application") {
    auto via_composed = composed >> plus_one;
    auto via_direct = boxf >> plus_one;

    CHECK(via_composed(Box<int>{9}) == via_direct(Box<int>{9}));
    CHECK(via_composed(Box<int>{-1}) == via_direct(Box<int>{-1}));
  }
}

TEST_CASE("Category: Immerse and Fish Examples",
          "[category][functor][immerse]") {
  SECTION("Box immerse with lambda application") {
    auto b = Box{42};
    auto h = box_hub<int>{};
    auto fishy_box = immerse(h, b);
    auto result_box = fishy_box >> [](int x) { return x + 1; };

    CHECK(result_box == Box<int>{43});
  }

  SECTION("Box immerse preserves identity") {
    auto b = Box{42};
    auto h = box_hub<int>{};
    auto fishy_box = immerse(h, b);
    auto result_id = fishy_box >> [](int x) { return x; };

    CHECK(result_id == Box<int>{42});
  }

  SECTION("Maybe immerse with short-circuit on nullopt") {
    auto m = std::optional<int>{std::nullopt};
    auto mh = maybe_hub<int>{};
    auto fishy_maybe = immerse(mh, m);

    // The lambda should never be called due to Hub's short-circuit logic
    auto result_maybe = fishy_maybe >> [](int x) { return x * 10; };

    CHECK(!result_maybe.has_value());
  }

  SECTION("Maybe immerse applies function when value present") {
    auto m = std::optional<int>{5};
    auto mh = maybe_hub<int>{};
    auto fishy_maybe = immerse(mh, m);

    auto result_maybe = fishy_maybe >> [](int x) { return x * 10; };

    REQUIRE(result_maybe.has_value());
    CHECK(result_maybe.value() == 50);
  }
}
