/** @file test/cpp/modules/dedekind/category/partial_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Partial: maturity mirrors total hierarchy", "[category][partial]") {
  using UAdd =
      BoundedAddTransform<unsigned int, FullMachineBoundaryPolicy<unsigned int>>;
  using UMul =
      BoundedMulTransform<unsigned int, FullMachineBoundaryPolicy<unsigned int>>;

  SECTION("Unsigned bounded add matures to partial Abelian group") {
    STATIC_CHECK(IsMagmoid<unsigned int, UAdd>);
    STATIC_CHECK(IsPartialSemigroup<unsigned int, UAdd>);
    STATIC_CHECK(IsPartialMonoid<unsigned int, UAdd>);
    STATIC_CHECK(IsPartialLoop<unsigned int, UAdd>);
    STATIC_CHECK(IsPartialGroup<unsigned int, UAdd>);
    STATIC_CHECK(IsPartialAbelianGroup<unsigned int, UAdd>);
  }

  SECTION("Restricted boundary is honest about uncertainty") {
    BoundedAddTransform<int, IntervalBoundaryPolicy<int>> add{{-10, 10}};
    const auto result = add({8, 5});

    CHECK(result.status == Ternary::Unknown);
  }

  SECTION("SafeAdd remains a partial monoid") {
    STATIC_CHECK(IsPartialMonoid<int, SafeAddTransform<int>>);
  }

  SECTION("Honest division is not a partial semigroup") {
    STATIC_CHECK_FALSE(IsPartialSemigroup<int, HonestDivTransform<int>>);
  }

  SECTION("Unsigned bounded multiply matures to commutative partial monoid") {
    STATIC_CHECK(IsPartialSemigroup<unsigned int, UMul>);
    STATIC_CHECK(IsPartialCommutativeSemigroup<unsigned int, UMul>);
    STATIC_CHECK(IsPartialMonoid<unsigned int, UMul>);
  }

  SECTION("Bounded division exposes undefined points") {
    BoundedDivTransform<int, FullMachineBoundaryPolicy<int>> div{};
    const auto by_zero = div({42, 0});
    CHECK(by_zero.status == Ternary::False);
  }
}
