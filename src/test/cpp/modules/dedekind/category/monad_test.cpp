#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <tuple>

import dedekind.category;

using namespace dedekind::category;

namespace {

using IntCat = DiscreteCategory<int>;
using IdF = identity_functor<IntCat>;

struct IdentityUnit {
  auto operator()(int) const { return id<int>(); }
};

struct IdentityJoin {
  auto operator()(int) const { return id<int>(); }
};

struct IdentityCounit {
  auto operator()(int) const { return id<int>(); }
};

struct IdentityDuplicate {
  auto operator()(int) const { return id<int>(); }
};

static_assert(
    IsNaturalTransformation<IdentityUnit, identity_functor<IntCat>, IdF>);
static_assert(
    IsNaturalTransformation<IdentityJoin, composite_functor<IdF, IdF>, IdF>);
static_assert(
    IsNaturalTransformation<IdentityCounit, IdF, identity_functor<IntCat>>);
static_assert(IsNaturalTransformation<IdentityDuplicate, IdF,
                                      composite_functor<IdF, IdF>>);

}  // namespace

TEST_CASE("Category: Monad and Comonad Concepts", "[category][monad]") {
  STATIC_CHECK(IsMonad<IdF, IdentityUnit, IdentityJoin>);
  STATIC_CHECK(IsComonad<IdF, IdentityCounit, IdentityDuplicate>);
}

TEST_CASE("Category: Monad Pipeline Operators",
          "[category][monad][operator-shift]") {
  SECTION("η_tag lifts through the identity monad") {
    CHECK((42 >> η_tag<IdentityUnit>{}) == 42);
    CHECK((-7 >> η_tag<IdentityUnit>{}) == -7);
  }

  SECTION("μ_tag joins through the identity monad") {
    CHECK((42 >> μ_tag<IdF, IdentityJoin>{}) == 42);
    CHECK((-7 >> μ_tag<IdF, IdentityJoin>{}) == -7);
  }
}

TEST_CASE("Category: Monad textbook aliases", "[category][monad][aliases]") {
  SECTION("pure and join route to η/μ for Maybe") {
    CHECK(pure(maybe_hub, 3) == std::optional<int>{3});

    std::optional<std::optional<int>> nested{std::optional<int>{8}};
    CHECK(join(maybe_hub, nested) == std::optional<int>{8});
  }

  SECTION("pure and join route to η/μ for Identity") {
    auto p = pure(identity_hub, 3);
    STATIC_CHECK(std::same_as<decltype(p), Identity<int>>);
    CHECK(p(3) == 3);

    auto j = join(identity_hub, id<Identity<int>>());
    STATIC_CHECK(std::same_as<decltype(j), Identity<int>>);
    CHECK(j(8) == 8);
  }

  SECTION("pure, join route through Maybe defaults (monad only)") {
    // Maybe is a monad, not a comonad — extract / duplicate live on
    // the Frobenius carrier std::tuple<T>, not on Maybe.
    CHECK(pure(maybe_hub, 2) == Maybe<int>{2});
    CHECK(join(maybe_hub, Maybe<Maybe<int>>{Maybe<int>{6}}) == Maybe<int>{6});
  }

  SECTION("pure, join, extract, duplicate route through tuple defaults") {
    // std::tuple<T> is the project's bona-fide Frobenius carrier
    // per #632 — both monad and comonad laws hold without concession.
    CHECK(pure(tuple_hub, 2) == std::tuple<int>{2});
    CHECK(join(tuple_hub, std::tuple<std::tuple<int>>{std::tuple<int>{6}}) ==
          std::tuple<int>{6});
    CHECK(extract(tuple_hub, std::tuple<int>{9}) == 9);
    CHECK(duplicate(tuple_hub, std::tuple<int>{9}) ==
          std::tuple<std::tuple<int>>{std::tuple<int>{9}});
  }
}

TEST_CASE("Category: Maybe-style bind and join behavior",
          "[category][monad][maybe]") {
  SECTION("Bind maps present optional values") {
    std::optional<int> value{5};
    auto result = value >>= [](int x) { return std::optional<int>{x + 7}; };

    REQUIRE(result.has_value());
    CHECK(*result == 12);
  }

  SECTION("Bind short-circuits on nullopt") {
    std::optional<int> value{std::nullopt};
    auto result = value >>= [](int x) { return std::optional<int>{x + 7}; };

    CHECK_FALSE(result.has_value());
  }

  SECTION("Bind chaining preserves null propagation") {
    std::optional<int> value{3};
    auto result = (value >>= [](int x) {
      return x > 0 ? std::optional<int>{x * 2} : std::nullopt;
    }) >>= [](int x) {
      return x > 10 ? std::optional<int>{x - 1} : std::nullopt;
    };

    CHECK_FALSE(result.has_value());
  }

  SECTION("Bind with identity flattens nested optional") {
    std::optional<std::optional<int>> nested{std::optional<int>{9}};
    auto result = nested >>= [](std::optional<int> inner) { return inner; };

    REQUIRE(result.has_value());
    CHECK(*result == 9);
  }

  SECTION("Bind with identity keeps nullopt when outer optional is empty") {
    std::optional<std::optional<int>> nested{std::nullopt};
    auto result = nested >>= [](std::optional<int> inner) { return inner; };

    CHECK_FALSE(result.has_value());
  }
}
