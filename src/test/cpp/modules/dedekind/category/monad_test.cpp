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
  // The identity monad: its laws decided, every component an identity.
  STATIC_CHECK(IsMonad<IdF, IdentityUnit, IdentityJoin>);
  STATIC_CHECK(IsComonad<IdF, IdentityCounit, IdentityDuplicate>);
}

TEST_CASE("Category: Monad textbook aliases", "[category][monad][aliases]") {
  SECTION("η and μ for Maybe") {
    CHECK(η(maybe_monad, 3) == std::optional<int>{3});

    std::optional<std::optional<int>> nested{std::optional<int>{8}};
    CHECK(μ(maybe_monad, nested) == std::optional<int>{8});
  }

  SECTION("η and μ for Identity") {
    auto p = η(identity_monad, 3);
    STATIC_CHECK(std::same_as<decltype(p), Identity<int>>);
    CHECK(p(3) == 3);

    auto j = μ(identity_monad, id<Identity<int>>());
    STATIC_CHECK(std::same_as<decltype(j), Identity<int>>);
    CHECK(j(8) == 8);
  }

  SECTION("η, μ, ε, δ through the tuple defaults") {
    // std::tuple<T> is the project's bona-fide Frobenius carrier
    // per #632 — both monad and comonad laws hold without concession.
    CHECK(η(tuple_monad, 2) == std::tuple<int>{2});
    CHECK(μ(tuple_monad, std::tuple<std::tuple<int>>{std::tuple<int>{6}}) ==
          std::tuple<int>{6});
    CHECK(ε(tuple_monad, std::tuple<int>{9}) == 9);
    CHECK(δ(tuple_monad, std::tuple<int>{9}) ==
          std::tuple<std::tuple<int>>{std::tuple<int>{9}});
  }
}

TEST_CASE("Category: Maybe-style bind and join behavior",
          "[category][monad][maybe]") {
  SECTION("Bind maps present optional values") {
    std::optional<int> value{5};
    auto result = value >>=
        arrow<int>([](int x) { return std::optional<int>{x + 7}; });

    REQUIRE(result.has_value());
    CHECK(*result == 12);
  }

  SECTION("Bind short-circuits on nullopt") {
    std::optional<int> value{std::nullopt};
    auto result = value >>=
        arrow<int>([](int x) { return std::optional<int>{x + 7}; });

    CHECK_FALSE(result.has_value());
  }

  SECTION("Bind chaining preserves null propagation") {
    std::optional<int> value{3};
    auto result = (value >>= arrow<int>([](int x) {
                     return x > 0 ? std::optional<int>{x * 2} : std::nullopt;
                   })) >>= arrow<int>([](int x) {
      return x > 10 ? std::optional<int>{x - 1} : std::nullopt;
    });

    CHECK_FALSE(result.has_value());
  }

  SECTION("Bind with identity flattens nested optional") {
    std::optional<std::optional<int>> nested{std::optional<int>{9}};
    auto result = nested >>= id<std::optional<int>>();

    REQUIRE(result.has_value());
    CHECK(*result == 9);
  }

  SECTION("Bind with identity keeps nullopt when outer optional is empty") {
    std::optional<std::optional<int>> nested{std::nullopt};
    auto result = nested >>= id<std::optional<int>>();

    CHECK_FALSE(result.has_value());
  }
}
