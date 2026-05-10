#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <tuple>

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
    identity_transformation<IdF> α{IdF{}};
    auto component = α(4);

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

  SECTION("Maybe hub supports monadic η, μ defaults") {
    // Maybe is mathematically a monad, not a comonad — counit on
    // nullopt has no honest answer.  The bona-fide Frobenius carrier
    // is std::tuple<T> (next SECTION); Maybe stays monad-only here.
    auto injected = η(maybe_hub, 4);
    CHECK(injected == Maybe<int>{4});

    Maybe<Maybe<int>> nested{Maybe<int>{17}};
    CHECK(μ(maybe_hub, nested) == Maybe<int>{17});
  }

  SECTION("Tuple hub supports η, μ, ε, δ defaults (Frobenius)") {
    // std::tuple<T> is the project's bona-fide Frobenius (= monad +
    // comonad) carrier per #632.  The 1-tuple always has a single
    // element, so std::get<0> is total — no Some-fragment caveat.
    auto injected = η(tuple_hub, 4);
    CHECK(injected == std::tuple<int>{4});

    std::tuple<std::tuple<int>> nested{std::tuple<int>{17}};
    CHECK(μ(tuple_hub, nested) == std::tuple<int>{17});

    CHECK(ε(tuple_hub, std::tuple<int>{19}) == 19);
    CHECK(δ(tuple_hub, std::tuple<int>{19}) ==
          std::tuple<std::tuple<int>>{std::tuple<int>{19}});
  }
}