/** @file test/cpp/modules/dedekind/category/discrete_lift_test.cpp
 *
 * Short-range test for the @c discrete_lift_t<S> template added in
 * @c category:etcs (#572).  Exercises the lift on primitive
 * @c IsSet-witnessing carriers built directly from @c ambient_set<T>;
 * the long-range test that runs the same lift across the project's
 * concrete numeric tower lives in @c numbers/discrete_lift_test.cpp.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("discrete_lift_t produces a DiscreteCategory for any IsSet carrier",
          "[category][etcs][discrete][lift]") {
  // Build several IsSet-witnessing carriers from primitive ambients.
  const auto int_non_negative =
      ambient_set<int>([](const int& x) { return x >= 0; });
  const auto bool_true = ambient_set<bool>([](const bool& x) { return x; });
  const auto uint_even =
      ambient_set<unsigned>([](const unsigned& x) { return (x % 2u) == 0u; });

  using IntNonNegSet = decltype(int_non_negative);
  using BoolTrueSet = decltype(bool_true);
  using UIntEvenSet = decltype(uint_even);

  // Each carrier is an IsSet (sanity sister-check).
  STATIC_CHECK(IsSet<IntNonNegSet>);
  STATIC_CHECK(IsSet<BoolTrueSet>);
  STATIC_CHECK(IsSet<UIntEvenSet>);

  // The lift fires on each carrier and produces a DiscreteCategory.
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<IntNonNegSet>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<BoolTrueSet>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<UIntEvenSet>>);

  // IsDiscreteCategory subsumes IsCategory; pin it for clarity.
  STATIC_CHECK(IsCategory<discrete_lift_t<IntNonNegSet>>);
  STATIC_CHECK(IsCategory<discrete_lift_t<BoolTrueSet>>);
  STATIC_CHECK(IsCategory<discrete_lift_t<UIntEvenSet>>);

  // The lift uses S itself, not S::Ambient — the static-type witness.
  STATIC_CHECK(
      std::same_as<discrete_lift_t<IntNonNegSet>, DiscreteCategory<IntNonNegSet>>);
}

TEST_CASE("discrete_lift_t preserves subobject identity (Disc(S1) /= Disc(S2))",
          "[category][etcs][discrete][lift][subobject]") {
  // Two distinct subobjects of the same ambient int.  Predicate
  // selection differs (non-negative vs. strictly positive); the
  // resulting set types are distinct, and so should be their lifts —
  // textbook reading: Disc({0,1,2,...}) is not the same category as
  // Disc({1,2,3,...}).
  const auto int_non_negative =
      ambient_set<int>([](const int& x) { return x >= 0; });
  const auto int_positive =
      ambient_set<int>([](const int& x) { return x > 0; });

  using S1 = decltype(int_non_negative);
  using S2 = decltype(int_positive);

  // Both have the same Ambient (int) but are structurally distinct
  // IsSet types.
  STATIC_CHECK(std::same_as<typename S1::Ambient, int>);
  STATIC_CHECK(std::same_as<typename S2::Ambient, int>);
  STATIC_CHECK_FALSE(std::same_as<S1, S2>);

  // The lift therefore produces distinct discrete categories — the
  // subobject information survives the lift.
  STATIC_CHECK_FALSE(
      std::same_as<discrete_lift_t<S1>, discrete_lift_t<S2>>);

  // Both still satisfy IsDiscreteCategory; the distinction is in
  // identity, not concept-membership.
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<S1>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<S2>>);
}
