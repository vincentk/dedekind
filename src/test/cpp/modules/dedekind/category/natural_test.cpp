#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <functional>
#include <tuple>
#include <type_traits>

import dedekind.category;

using namespace dedekind::category;

namespace {

using IntCat = DiscreteCategory<int>;
using IdF = identity_functor<IntCat>;

struct IdentityComponent {
  auto operator()(int) const { return id<int>(); }
};

static_assert(IsNaturalTransformation<IdentityComponent, IdF, IdF>);
// A composite of two stateless functors is one functor, though not an empty
// type: the square on it is read on its one value.
using IdIdF = composite_functor<IdF, IdF>;
static_assert(has_one_value_v<IdIdF> && !std::is_empty_v<IdIdF> &&
              IsNaturalTransformation<IdentityComponent, IdIdF, IdIdF>);
// The identity transformation on a monoid's delooping, and on an action out
// of it: their identities are identity arrows (IdentityIn), preserved by the
// functors, so the squares are the unit law.
using Sums = Delooping<std::size_t, std::plus<std::size_t>>;
using Adding = ActionFunctor<std::plus<std::size_t>, std::size_t, std::size_t>;
static_assert(
    IsNaturalTransformation<identity_transformation<identity_functor<Sums>>,
                            identity_functor<Sums>, identity_functor<Sums>> &&
    IsNaturalTransformation<identity_transformation<Adding>, Adding, Adding>);

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
  SECTION("Maybe monad supports η and μ defaults") {
    auto injected = η(maybe_monad, 7);
    CHECK(injected == std::optional<int>{7});

    std::optional<std::optional<int>> nested{std::optional<int>{11}};
    CHECK(μ(maybe_monad, nested) == std::optional<int>{11});
  }

  SECTION("Identity monad supports η, μ, ε, δ defaults") {
    auto injected = η(identity_monad, 5);
    STATIC_CHECK(std::same_as<decltype(injected), Identity<int>>);
    CHECK(injected(123) == 123);

    auto nested = id<Identity<int>>();
    auto joined = μ(identity_monad, nested);
    STATIC_CHECK(std::same_as<decltype(joined), Identity<int>>);
    CHECK(joined(77) == 77);

    auto extracted = ε(identity_monad, id<int>());
    STATIC_CHECK(std::same_as<decltype(extracted), Identity<int>>);
    CHECK(extracted(13) == 13);

    auto duplicated = δ(identity_monad, id<int>());
    STATIC_CHECK(std::same_as<decltype(duplicated), Identity<Identity<int>>>);
    CHECK(duplicated(id<int>())(9) == 9);
  }

  SECTION("Maybe monad supports monadic η, μ defaults") {
    // Maybe is mathematically a monad, not a comonad — counit on
    // nullopt has no honest answer.  The bona-fide Frobenius carrier
    // is std::tuple<T> (next SECTION); Maybe stays monad-only here.
    auto injected = η(maybe_monad, 4);
    CHECK(injected == Maybe<int>{4});

    Maybe<Maybe<int>> nested{Maybe<int>{17}};
    CHECK(μ(maybe_monad, nested) == Maybe<int>{17});
  }

  SECTION("Tuple monad supports η, μ, ε, δ defaults (Frobenius)") {
    // std::tuple<T> is the project's bona-fide Frobenius (= monad +
    // comonad) carrier per #632.  The 1-tuple always has a single
    // element, so std::get<0> is total — no Some-fragment caveat.
    auto injected = η(tuple_monad, 4);
    CHECK(injected == std::tuple<int>{4});

    std::tuple<std::tuple<int>> nested{std::tuple<int>{17}};
    CHECK(μ(tuple_monad, nested) == std::tuple<int>{17});

    CHECK(ε(tuple_monad, std::tuple<int>{19}) == 19);
    CHECK(δ(tuple_monad, std::tuple<int>{19}) ==
          std::tuple<std::tuple<int>>{std::tuple<int>{19}});
  }
}