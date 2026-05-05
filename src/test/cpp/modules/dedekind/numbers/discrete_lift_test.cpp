/** @file test/cpp/modules/dedekind/numbers/discrete_lift_test.cpp
 *
 * Long-range test for @c discrete_lift_t<S> from @c category:etcs (#572).
 * Exercises the lift on @c IsSet-witnessing carriers built from the
 * project's concrete numeric tower (@c int, @c unsigned, @c Rational,
 * @c Real) — illustrates that the textbook @c Set @c ↪ @c Cat
 * embedding fires at every rung of the carrier ladder, not only on
 * primitive int.  The short-range test that pins the lift on
 * primitive ambients lives in @c category/discrete_lift_test.cpp.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::numbers;

TEST_CASE("discrete_lift_t fires across the numeric tower",
          "[numbers][category][etcs][discrete][lift][tower]") {
  // Build IsSet-witnessing carriers at concrete tower positions.
  // Each predicate selects a non-trivial subobject so the lift's
  // subobject-preservation behaviour is observable.
  const auto naturals_under_10 =
      ambient_set<unsigned>([](const unsigned& n) { return n < 10u; });
  const auto integers_in_range =
      ambient_set<int>([](const int& n) { return n >= -5 && n <= 5; });
  const auto positive_rationals = ambient_set<Rational<default_integer>>(
      [](const Rational<default_integer>& q) { return q.num() > 0; });

  using NatSubset = decltype(naturals_under_10);
  using IntSubset = decltype(integers_in_range);
  using RationalSubset = decltype(positive_rationals);

  // Each carrier is a bona fide IsSet over its tower-rung Ambient.
  STATIC_CHECK(IsSet<NatSubset>);
  STATIC_CHECK(IsSet<IntSubset>);
  STATIC_CHECK(IsSet<RationalSubset>);

  // The lift fires at every tower rung; the textbook Set ↪ Cat
  // embedding is mechanically observable per concrete carrier.
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<NatSubset>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<IntSubset>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<RationalSubset>>);

  // IsCategory falls out of IsDiscreteCategory; pinned for clarity.
  STATIC_CHECK(IsCategory<discrete_lift_t<NatSubset>>);
  STATIC_CHECK(IsCategory<discrete_lift_t<IntSubset>>);
  STATIC_CHECK(IsCategory<discrete_lift_t<RationalSubset>>);
}

TEST_CASE("discrete_lift_t distinguishes refined subobjects of ℚ",
          "[numbers][category][etcs][discrete][lift][rational]") {
  // Two distinct subobjects of ℚ — the strictly-positive rationals
  // and the rationals strictly between 0 and 1.  Both share the
  // same Ambient (Rational<default_integer>), but the predicates
  // pick out different subobjects; the lift should produce
  // distinct discrete categories (the textbook
  // Disc(ℚ_{>0}) /= Disc(ℚ_{(0,1)}) reading).
  const auto positive = ambient_set<Rational<default_integer>>(
      [](const Rational<default_integer>& q) { return q.num() > 0; });
  const auto open_unit_interval = ambient_set<Rational<default_integer>>(
      [](const Rational<default_integer>& q) {
        return q.num() > 0 && q.num() < q.den();
      });

  using S1 = decltype(positive);
  using S2 = decltype(open_unit_interval);

  // Same ambient, distinct types.
  STATIC_CHECK(std::same_as<typename S1::Ambient, Rational<default_integer>>);
  STATIC_CHECK(std::same_as<typename S2::Ambient, Rational<default_integer>>);
  STATIC_CHECK_FALSE(std::same_as<S1, S2>);

  // The lift produces distinct discrete categories — subobject
  // information is preserved through Set ↪ Cat.
  STATIC_CHECK_FALSE(std::same_as<discrete_lift_t<S1>, discrete_lift_t<S2>>);

  // Both still satisfy IsDiscreteCategory; the distinction is in
  // identity, not concept-membership.
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<S1>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<S2>>);
}
