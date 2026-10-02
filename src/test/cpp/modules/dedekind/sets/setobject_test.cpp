/** @file dedekind/sets/setobject_test.cpp
 *
 * The noun, exercised at runtime: a set object is (universe, χ).  The
 * @c static_assert witnesses in @c :setobject / @c :boundaries pin the shape;
 * this test drives the two legs and the lattice nodes as values so the fold is
 * covered where Codecov can see it.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>
#include <utility>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

namespace {
struct IsEven {
  constexpr bool operator()(int x) const { return x % 2 == 0; }
};
struct IsPositive {
  constexpr bool operator()(int x) const { return x > 0; }
};
}  // namespace

TEST_CASE("sets:setobject — the two legs of a set object at runtime",
          "[sets][setobject][legs]") {
  const auto even = Set<int, Boole, IsEven>{IsEven{}};
  const auto positive = Set<int, Boole, IsPositive>{IsPositive{}};

  SECTION("a Set's universe is 𝔸 over its carrier; its classifier is P") {
    STATIC_CHECK(IsSetObject<decltype(even)>);
    const auto u = universe(even);
    STATIC_CHECK(Is𝔸<decltype(u)>);
    CHECK(u(7));  // the universe accepts everything
    CHECK(classifier(even)(4));
    CHECK_FALSE(classifier(even)(3));
  }

  SECTION("a comprehension's universe is its base's; χ restricts") {
    const auto small_even = Comprehension{even, IsPositive{}};
    STATIC_CHECK(IsSetObject<decltype(small_even)>);
    STATIC_CHECK(std::same_as<universe_t<decltype(small_even)>,
                              universe_t<decltype(even)>>);
    CHECK(static_cast<bool>(small_even(4)));
    CHECK_FALSE(static_cast<bool>(small_even(-4)));
    CHECK_FALSE(static_cast<bool>(classifier(small_even)(3)));
  }

  SECTION("Ø and 𝔸: the universe is its own universe") {
    CHECK(universe(Ø<int>{})(0));
    CHECK(universe(𝔸<int>{})(0));
    STATIC_CHECK(std::same_as<universe_t<Ø<int>>, 𝔸<int>>);
    CHECK_FALSE(static_cast<bool>(classifier(Ø<int>{})(0)));
  }

  SECTION("a lattice node over set objects is a set object: χ is pointwise") {
    const auto both = MakeMeet{}(even, positive);  // Meet<Set, Set>
    STATIC_CHECK(IsSetObject<decltype(both)>);
    CHECK(static_cast<bool>(both(4)));
    CHECK_FALSE(static_cast<bool>(both(3)));
    CHECK_FALSE(static_cast<bool>(both(-4)));
    CHECK(universe(both)(-4));  // the operands' universe, the whole
    const auto either = MakeJoin{}(even, positive);
    CHECK(static_cast<bool>(either(3)));
    CHECK_FALSE(static_cast<bool>(either(-3)));
    const auto odd = Not<decltype(even)>{even};
    STATIC_CHECK(IsSetObject<decltype(odd)>);
    CHECK(static_cast<bool>(odd(3)));
    CHECK(universe(odd)(3));
  }

  SECTION("the reducer folds a term to a set object") {
    const auto r = subobject_reduce<Boole>(MakeMeet{}(Ø<int>{}, even));
    STATIC_CHECK(IsSetObject<decltype(r)>);
    CHECK_FALSE(static_cast<bool>(r(4)));  // Ø ∧ even = Ø
    const auto top = subobject_reduce<Boole>(MakeJoin{}(𝔸<int>{}, even));
    CHECK(static_cast<bool>(top(3)));  // 𝔸 ∨ even = 𝔸
  }

  SECTION("a pair universe projects to the factor universes") {
    const auto pairs = 𝔸<std::pair<int, bool>>{};
    STATIC_CHECK(IsProduct<decltype(pairs), 𝔸<int>, 𝔸<bool>>);
    CHECK(π_1(pairs)(42));
    CHECK(π_2(pairs)(false));
  }
}
