/** @file test/cpp/modules/dedekind/category/posetal_test.cpp */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Posetal: textbook default relation", "[category][posetal][order]") {
  SECTION("Default witness is <= over classical Omega") {
    STATIC_CHECK(IsPosetal<int>);
  }

  SECTION("Path composition follows transitivity") {
    CHECK(check_path<int, std::less_equal<int>>(1, 2, 3));
    CHECK_FALSE(check_path<int, std::less_equal<int>>(3, 2, 1));
  }

  SECTION("Semilattice and lattice laws for min/max are validated in tests") {
    const int a = 7;
    const int b = 3;
    const int c = 9;

    const auto meet = std::ranges::min;
    const auto join = std::ranges::max;

    CHECK(meet(a, b) == meet(b, a));
    CHECK(join(a, b) == join(b, a));

    CHECK(meet(meet(a, b), c) == meet(a, meet(b, c)));
    CHECK(join(join(a, b), c) == join(a, join(b, c)));

    CHECK(meet(a, a) == a);
    CHECK(join(a, a) == a);

    CHECK(join(a, meet(a, b)) == a);
    CHECK(meet(a, join(a, b)) == a);

    CHECK(join(a, meet(b, c)) == meet(join(a, b), join(a, c)));
    CHECK(meet(a, join(b, c)) == join(meet(a, b), meet(a, c)));
  }

  SECTION(
      "Certified semilattice and lattice concepts verify commutativity and "
      "associativity") {
    STATIC_CHECK(
        IsCertifiedOrderMeetSemilattice<int, decltype(std::ranges::min)>);
    STATIC_CHECK(
        IsCertifiedOrderJoinSemilattice<int, decltype(std::ranges::max)>);
    STATIC_CHECK(
        IsCertifiedOrderLatticeOperations<int, decltype(std::ranges::max),
                                          decltype(std::ranges::min)>);

    const int x = 5;
    const int y = 12;
    const int z = 3;

    const auto meet = std::ranges::min;
    const auto join = std::ranges::max;

    CHECK(IsCertifiedOrderMeetSemilattice<int, decltype(meet)>);
    CHECK(IsCertifiedOrderJoinSemilattice<int, decltype(join)>);
    CHECK(IsOrderLatticeOperations<int, decltype(join), decltype(meet)>);

    CHECK(meet(x, y) == y || meet(x, y) == x);
    CHECK(join(x, y) == x || join(x, y) == y);

    CHECK(meet(x, meet(y, z)) == meet(meet(x, y), z));
    CHECK(join(x, join(y, z)) == join(join(x, y), z));

    CHECK(join(x, meet(x, y)) == x);
    CHECK(meet(x, join(x, y)) == x);

    CHECK(join(x, meet(y, z)) == meet(join(x, y), join(x, z)));
    CHECK(meet(x, join(y, z)) == join(meet(x, y), meet(x, z)));
  }
}
