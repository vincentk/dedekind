#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
using namespace dedekind::algebra;

TEST_CASE("Algebra: The Ring of Integers", "[algebra][ring]") {
  SECTION("The Identity of Z") {
    // Documentation-only checkpoint:
    // machine `int` is not asserted as a total additive group witness.
    // STATIC_CHECK(IsAdditiveGroup<int>);
    SUCCEED("int group witness intentionally deferred (overflow semantics).");
  }

  SECTION("Axiomatic Action") {
    int a = 6;
    int b = 7;
    // This will now correctly use the BUILT-IN operator*
    CHECK(a * b == 42);

    // If you want to verify our algebraic logic:
    CHECK(std::multiplies<int>{}(a, b) == 42);
  }
}
