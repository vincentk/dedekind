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
  STATIC_CHECK(IsSpokeArrow<decltype(plus_one)>);
}

TEST_CASE("Category: Functor hub action", "[category][functor][hub-action]") {
  SECTION("Identity functor preserves arrow behavior") {
    identity_functor<DiscreteCategory<int>> idf;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = idf.φ(plus_one);

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted(41) == 42);
    CHECK(lifted(-2) == -1);
  }

  SECTION("Box functor lifts codomain into Box<T>") {
    box_functor<int> boxf;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = boxf.φ(plus_one);

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted(Box<int>{41}) == Box<int>{42});
    CHECK(lifted(Box<int>{-2}) == Box<int>{-1});
  }
}

TEST_CASE("Category: Functor composition", "[category][functor][composition]") {
  using IntCat = DiscreteCategory<int>;
  using IdF = identity_functor<IntCat>;

  auto plus_one = arrow([](int x) { return x + 1; });
  composite_functor<IdF, IdF> composed{};

  SECTION("Composed functor keeps category handles") {
    static_assert(std::same_as<typename decltype(composed)::Σ_cat, IntCat>);
    static_assert(std::same_as<typename decltype(composed)::Τ_cat, IntCat>);
    SUCCEED();
  }

  SECTION("Composed action matches direct application") {
    auto via_composed = composed.φ(plus_one);
    auto via_direct = IdF{}.φ(plus_one);

    CHECK(via_composed(9) == via_direct(9));
    CHECK(via_composed(-1) == via_direct(-1));
  }
}
