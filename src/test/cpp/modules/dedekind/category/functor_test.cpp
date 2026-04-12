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
  STATIC_CHECK(IsFunctor<maybe_functor<int>>);
  STATIC_CHECK(IsFunctor<trace_functor<int>>);
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

  SECTION("Maybe functor short-circuits empty inputs") {
    maybe_functor<int> maybef;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = maybef.φ(plus_one);

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted(std::optional<int>{41}) == std::optional<int>{42});
    CHECK(lifted(std::optional<int>{}) == std::nullopt);
  }

  SECTION("Trace functor lifts into StringCategory") {
    trace_functor<int> tracef;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = tracef.φ(plus_one);

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted.label == "lifted");
    CHECK(lifted.domain_id == 0);
    CHECK(lifted.codomain_id == 0);
  }
}

TEST_CASE("Category: Functor composition", "[category][functor][composition]") {
  using IntCat = DiscreteCategory<int>;
  using IdF = identity_functor<IntCat>;

  auto plus_one = arrow([](int x) { return x + 1; });
  composite_functor<IdF, IdF> composed{};

  SECTION("Composed functor keeps category handles") {
    static_assert(IsFunctor<decltype(composed)>);
    static_assert(std::same_as<typename decltype(composed)::Σ_cat, IntCat>);
    static_assert(std::same_as<typename decltype(composed)::Τ_cat, IntCat>);
    SUCCEED();
  }

  SECTION("Composed object mapping matches direct application") {
    auto mapped = composed(IntCat{});

    STATIC_CHECK(std::same_as<decltype(mapped), IntCat>);
    SUCCEED();
  }

  SECTION("Composed action matches direct application") {
    auto via_composed = composed.φ(plus_one);
    auto via_direct = IdF{}.φ(plus_one);

    CHECK(via_composed(9) == via_direct(9));
    CHECK(via_composed(-1) == via_direct(-1));
  }

  SECTION("Fish composition instantiates to a functor") {
    auto downstream = IdF{} >> IdF{};
    auto upstream = IdF{} << IdF{};

    STATIC_CHECK(IsFunctor<decltype(downstream)>);
    STATIC_CHECK(IsFunctor<decltype(upstream)>);
    CHECK(downstream.φ(plus_one)(9) == 10);
    CHECK(upstream.φ(plus_one)(9) == 10);
  }
}

TEST_CASE("Category: verify_functor_composition",
          "[category][functor][composition-proof]") {
  using IntCat = DiscreteCategory<int>;
  using IdF = identity_functor<IntCat>;

  typename IdF::Σ_cat::Arrow f = id<int>();
  typename IdF::Σ_cat::Arrow g = id<int>();

  verify_functor_composition<IdF>(f, g);
  SUCCEED();
}
