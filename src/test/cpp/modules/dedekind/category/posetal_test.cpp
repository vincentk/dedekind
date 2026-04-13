/** @file test/cpp/modules/dedekind/category/posetal_test.cpp */
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
}
