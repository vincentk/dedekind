#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
using namespace dedekind::algebra;
using namespace dedekind::category;

TEST_CASE("Group: The Rules of Symmetry", "[algebra][group]") {
  SECTION("Additive Groups (Inversion)") {
    // Documentation-only checkpoint:
    // Machine `int` is not modeled as a total additive group in this layer,
    // because overflow breaks closure/invertibility in the mathematical sense.
    // STATIC_CHECK(IsAdditiveGroup<int>);

    int q = 42;
    int zero = 0;  // Identity
    CHECK(q + zero == q);
    CHECK(zero + q == q);
    CHECK(q + (-q) == zero);
  }
}
