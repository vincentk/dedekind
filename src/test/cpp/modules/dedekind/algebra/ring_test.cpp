#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
using namespace dedekind::algebra;

TEST_CASE("Algebra: The Ring of Integers", "[algebra][ring]") {
  SECTION("The Identity of Z") {
    // Z is a Rig (0 and 1 exist, + and * are harmonious)
    STATIC_CHECK(IsRig<int>);

    // Z is a Rng (Negatives exist)
    STATIC_CHECK(IsRng<int>);

    // Therefore, Z is a Ring
    STATIC_CHECK(IsRing<int>);
  }

  SECTION("The Point-Free Engine") {
    int a = 6;
    int b = 7;
    // Verify our constrained operator* is actually being used
    CHECK(dedekind::algebra::operator*(a, b) == 42);
  }
}
