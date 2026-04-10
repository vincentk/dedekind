#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

namespace dedekind::category {

struct WitnessArrow {
  using Domain = int;
  using Codomain = int;

  int vertex;
  std::function<int(int)> transform;

  int operator()(int x) const { return transform(x); }

  friend auto operator>>(const WitnessArrow& f, const WitnessArrow& g) {
    return WitnessArrow{g.vertex, [f, g](int x) { return g(f(x)); }};
  }
};

struct WitnessCategory {
  using Species = int;
  using Arrow = WitnessArrow;
  using Id = WitnessArrow;

  static Arrow id_c(int x) {
    return Arrow{x, [](int y) { return y; }};
  }
};

template <int Offset>
struct TranslateFunctor {
  using Σ_cat = WitnessCategory;
  using Τ_cat = WitnessCategory;
  using Domain = WitnessCategory::Arrow;
  using Codomain = WitnessCategory::Arrow;

  auto fmap(const WitnessArrow& f) const {
    return WitnessArrow{f.vertex + Offset,
                        [f](int x) { return f(x - Offset) + Offset; }};
  }

  Codomain operator()(const Domain& f) const { return fmap(f); }
};

struct Shift01 {
  auto operator()(int c) const {
    return WitnessArrow{c + 1, [](int x) { return x + 1; }};
  }
};

struct Shift12 {
  auto operator()(int c) const {
    return WitnessArrow{c + 2, [](int x) { return x + 1; }};
  }
};

static_assert(IsCategory<WitnessCategory>);
static_assert(IsFunctor<TranslateFunctor<1>>);
static_assert(IsFunctor<TranslateFunctor<2>>);

}  // namespace dedekind::category

TEST_CASE("Category: Natural Transformation Concepts", "[category][natural]") {
  using IdF = identity_functor<WitnessCategory>;
  using F1 = TranslateFunctor<1>;
  using F2 = TranslateFunctor<2>;

  STATIC_CHECK(IsNaturalTransformation<identity_transformation<IdF>, IdF, IdF>);
  STATIC_CHECK(IsNaturalTransformation<Shift01, IdF, F1>);
  STATIC_CHECK(IsNaturalTransformation<Shift12, F1, F2>);
}

TEST_CASE("Category: Natural Transformation Runtime Witnesses",
          "[category][natural]") {
  using IdF = identity_functor<WitnessCategory>;
  using F1 = TranslateFunctor<1>;
  using F2 = TranslateFunctor<2>;

  SECTION("Identity transformation returns the target identity component") {
    identity_transformation<F1> alpha{F1{}};
    auto component = alpha(4);

    CHECK(component.vertex == 5);
    CHECK(component(42) == 42);
  }

  SECTION("Vertical composition stacks component arrows") {
    vertical_composition<Shift01, Shift12, IdF, F1, F2> composite{Shift01{},
                                                                  Shift12{}};
    auto component = composite(3);

    CHECK(component.vertex == 5);
    CHECK(component(10) == 12);
  }

  SECTION("Horizontal composition matches successive functorial shifts") {
    horizontal_composition<Shift01, Shift01, IdF, F1, IdF, F1> composite{
        Shift01{}, Shift01{}, IdF{}};
    auto component = composite(3);

    CHECK(component.vertex == 5);
    CHECK(component(10) == 12);
  }
}