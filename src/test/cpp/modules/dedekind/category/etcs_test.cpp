/** @file test/cpp/modules/dedekind/category/etcs_test.cpp */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("ETCS: set lattice operations", "[category][etcs][sets]") {
  const auto s_even = classify<int>([](const int& x) { return x % 2 == 0; });
  const auto s_positive = classify<int>([](const int& x) { return x > 0; });

  SECTION("Classical intersection/union/complement") {
    const auto both = set_intersection(s_even, s_positive);
    const auto either = set_union(s_even, s_positive);
    const auto not_even = set_complement(s_even);

    STATIC_CHECK(IsSetObject<decltype(both), int>);
    STATIC_CHECK(IsSetObject<decltype(either), int>);
    STATIC_CHECK(IsSetObject<decltype(not_even), int>);

    CHECK(both.χ(2) == true);
    CHECK(both.χ(-2) == false);
    CHECK(either.χ(-2) == true);
    CHECK(either.χ(-3) == false);
    CHECK(not_even.χ(3) == true);
    CHECK(not_even.χ(2) == false);
  }

  SECTION("Lattice aliases meet/join map to intersection/union") {
    const auto m = meet(s_even, s_positive);
    const auto j = join(s_even, s_positive);

    CHECK(m.χ(4) == true);
    CHECK(m.χ(-4) == false);
    CHECK(j.χ(-4) == true);
    CHECK(j.χ(-3) == false);
  }
}

TEST_CASE("ETCS: ternary support lattice", "[category][etcs][support]") {
  const auto bounded = classify<int>([](const int& x) {
    if (x < -10 || x > 10) return Ternary::Unknown;
    return Ternary::True;
  });

  const auto non_negative = classify<int>(
      [](const int& x) { return x >= 0 ? Ternary::True : Ternary::False; });

  SECTION("Support intersection propagates unknown honestly") {
    const auto support = set_intersection(bounded, non_negative);

    STATIC_CHECK(HasTernarySupport<decltype(support)>);

    CHECK(support.χ(5) == Ternary::True);
    CHECK(support.χ(-5) == Ternary::False);
    CHECK(support.χ(50) == Ternary::Unknown);
  }
}
