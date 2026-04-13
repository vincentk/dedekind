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
}
