/** @file test/cpp/modules/dedekind/category/partial_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Partial: maturity mirrors total hierarchy", "[category][partial]") {
  using UAdd = BoundedAddTransform<unsigned int,
                                   FullMachineBoundaryPolicy<unsigned int>>;
  using UMul = BoundedMulTransform<unsigned int,
                                   FullMachineBoundaryPolicy<unsigned int>>;

  SECTION("Unsigned bounded add matures to partial Abelian group") {
    STATIC_CHECK(IsPartialMagma<unsigned int, UAdd>);
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

  SECTION("Unsigned bounded add success cases") {
    UAdd add{};
    const auto result1 = add({10, 20});
    CHECK(result1.value == 30);
    CHECK(result1.status == Ternary::True);

    const auto result_small = add({5, 5});
    CHECK(result_small.value == 10);
    CHECK(result_small.status == Ternary::True);
  }

  SECTION("Unsigned bounded multiply success cases") {
    UMul mul{};
    const auto mul_large = mul({10, 20});
    CHECK(mul_large.value == 200);
    CHECK(mul_large.status == Ternary::True);

    const auto mul_unit = mul({1, 1});
    CHECK(mul_unit.value == 1);
    CHECK(mul_unit.status == Ternary::True);
  }

  SECTION("SafeAdd behavior with values") {
    SafeAddTransform<int> add{};
    const auto result = add({100, 100});
    CHECK(result.has_value());
    if (result) {
      CHECK(*result == 200);
    }
  }

  SECTION("Interval boundary policy with in-range values") {
    BoundedAddTransform<int, IntervalBoundaryPolicy<int>> add{{-10, 10}};
    const auto result = add({3, 2});
    CHECK(result.value == 5);
    CHECK(result.status == Ternary::True);
  }

  SECTION("Interval boundary policy with out-of-range result") {
    BoundedAddTransform<int, IntervalBoundaryPolicy<int>> add{{-10, 10}};
    const auto overflow = add({8, 5});
    CHECK(overflow.status == Ternary::Unknown);

    const auto underflow = add({-8, -5});
    CHECK(underflow.status == Ternary::Unknown);
  }

  SECTION("Commutativity of bounded operations") {
    BoundedAddTransform<int, FullMachineBoundaryPolicy<int>> add{};
    const auto ab = add({7, 3});
    const auto ba = add({3, 7});
    CHECK(ab.value == ba.value);
    CHECK(ab.status == ba.status);
  }
}
