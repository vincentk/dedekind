#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <functional>
#include <limits>
#include <optional>
#include <tuple>
#include <utility>

import dedekind.category;

using namespace dedekind::category;
using std::size_t;

// The checked arithmetic is a Kleisli arrow of the maybe monad: T x T ->
// Maybe<T>, nothing on overflow.  Bind threads it, and overflow short-circuits
// the rest of the chain (#1061).
namespace maybe_witness {
constexpr int big = std::numeric_limits<int>::max();
static_assert((Maybe<std::pair<int, int>>{{40, 2}} >>=
               SafeAddTransform<int>{}) == Maybe<int>{42});
static_assert(!(Maybe<std::pair<int, int>>{{big, 1}} >>=
                SafeAddTransform<int>{}));
static_assert(!(Maybe<std::pair<int, int>>{} >>= SafeAddTransform<int>{}));
}  // namespace maybe_witness

// The test's arrows, from the library's combinators: a section of + or · then
// the unit is a Kleisli arrow, A → T(B); the counit then a section is a
// co-Kleisli arrow, W(A) → B.
namespace kleisli_arrows {
constexpr auto adding = curry<size_t>(std::plus<size_t>{});
constexpr auto scaling = curry<size_t>(std::multiplies<size_t>{});
constexpr unit_witness<maybe_functor<size_t>, size_t> η_maybe{};
constexpr unit_witness<tuple_functor<size_t>, size_t> η_tuple{};
constexpr counit_witness<tuple_functor<size_t>, size_t> ε_tuple{};
}  // namespace kleisli_arrows
using namespace kleisli_arrows;

// The Kleisli-triple laws, on values: η(x) >>= f is f(x), m >>= η is m, and
// binding twice is binding once by the Kleisli composite.
namespace kleisli_laws {
constexpr auto f = adding(5uz) >> η_maybe;
constexpr auto g = scaling(2uz) >> η_maybe;
constexpr Maybe<size_t> m{10};
static_assert(κ(η_maybe(10uz), f) == f(10uz) && κ(m, η_maybe) == m &&
              κ(κ(m, f), g) == κ(m, adding(5uz) >> scaling(2uz) >> η_maybe) &&
              κ(Maybe<size_t>{}, f) == Maybe<size_t>{});
}  // namespace kleisli_laws

TEST_CASE("Category: Kleisli triples", "[category][kleisli]") {
  // Maybe is a monad (a Kleisli triple only); std::tuple<T> is a monad and a
  // comonad, with no Some-fragment concession.
  STATIC_CHECK(IsKleisliTriple<maybe_functor<size_t>, size_t, size_t>);
  STATIC_CHECK(IsKleisliTriple<tuple_functor<size_t>, size_t, size_t>);
  STATIC_CHECK(IsCoKleisliTriple<tuple_functor<size_t>, size_t, size_t>);
  STATIC_CHECK(IsFrobenius<tuple_functor<size_t>, size_t, size_t>);
  STATIC_CHECK(IsArrow<decltype(adding(10uz) >> η_maybe)> &&
               IsArrow<decltype(ε_tuple >> adding(3uz))>);
}

TEST_CASE("Category: Tuple Frobenius structure",
          "[category][kleisli][frobenius]") {
  std::tuple<size_t> value{9};

  SECTION("Unit followed by counit returns the original value") {
    CHECK(ε_tuple(η_tuple(std::get<0>(value))) == std::get<0>(value));
  }

  SECTION("Kleisli right unit law: κ(ma, η) = ma") {
    CHECK(κ(value, η_tuple) == value);
  }

  SECTION("Co-Kleisli right counit law: σ(wa, ε) = wa") {
    CHECK(σ(value, ε_tuple) == value);
  }

  SECTION("Bind then extract agrees with direct function result") {
    const auto f = scaling(3uz) >> η_tuple;
    CHECK(ε_tuple(κ(value, f)) == ε_tuple(f(std::get<0>(value))));
  }

  SECTION("Extend then extract agrees with direct co-Kleisli result") {
    const auto g = ε_tuple >> adding(4uz);
    CHECK(ε_tuple(σ(value, g)) == g(value));
  }
}

TEST_CASE("Category: Tuple Bind and Extend", "[category][kleisli]") {
  std::tuple<size_t> value{21};

  SECTION("Bind (>>=) chains in-context computations") {
    CHECK((value >>= scaling(2uz) >> η_tuple) == std::tuple<size_t>{42});
  }

  SECTION("Extend (<<=) samples context to build new context") {
    CHECK((value <<= ε_tuple >> adding(1uz)) == std::tuple<size_t>{22});
  }
}

TEST_CASE("Category: Maybe Bind", "[category][kleisli]") {
  Maybe<size_t> value{21};

  SECTION("Bind (>>=) chains in-context computations") {
    CHECK((value >>= scaling(2uz) >> η_maybe) == Maybe<size_t>{42});
  }

  SECTION("Bind on nullopt propagates the empty case") {
    CHECK((Maybe<size_t>{} >>= adding(10uz) >> η_maybe) == std::nullopt);
  }

  SECTION("Chaining multiple binds") {
    CHECK(((value >>= scaling(2uz) >> η_maybe) >>= adding(1uz) >> η_maybe) ==
          Maybe<size_t>{43});
  }
}

TEST_CASE("Category: Kleisli κ (kappa) extension",
          "[category][kleisli][kappa]") {
  Maybe<size_t> value{10};

  SECTION("κ applies Kleisli arrow to monadic value") {
    CHECK(κ(value, scaling(2uz) >> η_maybe) == Maybe<size_t>{20});
  }

  SECTION("κ chains multiple Kleisli extensions") {
    const auto step1 = κ(value, adding(5uz) >> η_maybe);
    CHECK(κ(step1, scaling(2uz) >> η_maybe) == Maybe<size_t>{30});
  }
}

TEST_CASE("Category: Co-Kleisli σ (sigma) extend",
          "[category][kleisli][sigma]") {
  std::tuple<size_t> value{7};

  SECTION("σ applies co-Kleisli arrow to comonadic value") {
    CHECK(σ(value, ε_tuple >> adding(3uz)) == std::tuple<size_t>{10});
  }

  SECTION("Chaining multiple coextends") {
    CHECK(((value <<= ε_tuple >> scaling(2uz)) <<= ε_tuple >> adding(3uz)) ==
          std::tuple<size_t>{17});
  }
}

TEST_CASE("Category: Named Kleisli aliases", "[category][kleisli][aliases]") {
  SECTION("bind and extend alias κ and σ") {
    CHECK(bind(Maybe<size_t>{6}, adding(4uz) >> η_maybe) == Maybe<size_t>{10});
    CHECK(extend(std::tuple<size_t>{6}, ε_tuple >> scaling(3uz)) ==
          std::tuple<size_t>{18});
  }

  SECTION("bind with maybe_monad uses the default join route") {
    const auto plus_two = adding(2uz) >> η_maybe;
    CHECK(bind(maybe_monad, Maybe<size_t>{5}, plus_two) == Maybe<size_t>{7});
    CHECK(bind(maybe_monad, Maybe<size_t>{}, plus_two) == std::nullopt);
  }

  SECTION("bind and extend with tuple_monad use the default routes") {
    const std::tuple<size_t> value{6};
    CHECK(bind(tuple_monad, value, adding(4uz) >> η_tuple) ==
          std::tuple<size_t>{10});
    CHECK(extend(tuple_monad, value, ε_tuple >> scaling(3uz)) ==
          std::tuple<size_t>{18});
  }
}

TEST_CASE("Category: Kleisli operator-shape concepts",
          "[category][kleisli][concept-shape]") {
  using MaybeBind = decltype(adding(1uz) >> η_maybe);
  using TupleCoBind = decltype(ε_tuple >> scaling(2uz));
  STATIC_CHECK(HasKleisliBindOperators<Maybe<size_t>, MaybeBind>);
  STATIC_CHECK(HasComonadicExtendOperators<std::tuple<size_t>, TupleCoBind>);
  STATIC_CHECK(!HasKleisliBindOperators<size_t, MaybeBind>);
}
