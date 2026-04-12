#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

namespace {

using IntCat = DiscreteCategory<int>;
using IdF = identity_functor<IntCat>;

struct IdentityComponent {
  auto operator()(int) const { return id<int>(); }
};

static_assert(IsNaturalTransformation<IdentityComponent, IdF, IdF>);

}  // namespace

TEST_CASE("Category: Natural Transformation Concepts", "[category][natural]") {
  STATIC_CHECK(IsNaturalTransformation<identity_transformation<IdF>, IdF, IdF>);
  STATIC_CHECK(IsNaturalTransformation<IdentityComponent, IdF, IdF>);
}

TEST_CASE("Category: Natural Transformation Runtime Witnesses",
          "[category][natural]") {
  SECTION("Identity transformation returns the target identity component") {
    identity_transformation<IdF> alpha{IdF{}};
    auto component = alpha(4);

    CHECK(component(42) == 42);
    CHECK(component(4) == 4);
  }
}

TEST_CASE("Category: Natural textbook operator defaults",
          "[category][natural][defaults]") {
  SECTION("Maybe hub supports η and μ defaults") {
    auto injected = η(maybe_hub, 7);
    CHECK(injected == std::optional<int>{7});

    std::optional<std::optional<int>> nested{std::optional<int>{11}};
    CHECK(μ(maybe_hub, nested) == std::optional<int>{11});
  }

  SECTION("Identity hub supports η, μ, ε, δ defaults") {
    auto injected = η(identity_hub, 5);
    STATIC_CHECK(std::same_as<decltype(injected), Identity<int>>);
    CHECK(injected(123) == 123);

    auto nested = id<Identity<int>>();
    auto joined = μ(identity_hub, nested);
    STATIC_CHECK(std::same_as<decltype(joined), Identity<int>>);
    CHECK(joined(77) == 77);

    auto extracted = ε(identity_hub, id<int>());
    STATIC_CHECK(std::same_as<decltype(extracted), Identity<int>>);
    CHECK(extracted(13) == 13);

    auto duplicated = δ(identity_hub, id<int>());
    STATIC_CHECK(std::same_as<decltype(duplicated), Identity<Identity<int>>>);
    CHECK(duplicated(id<int>())(9) == 9);
  }

  SECTION("Box hub supports η, μ, ε, δ defaults") {
    auto injected = η(box_hub, 4);
    CHECK(injected == Box<int>{4});

    Box<Box<int>> nested{{17}};
    CHECK(μ(box_hub, nested) == Box<int>{17});

    CHECK(ε(box_hub, Box<int>{19}) == 19);
    CHECK(δ(box_hub, Box<int>{19}) == Box<Box<int>>{{19}});
  }
}