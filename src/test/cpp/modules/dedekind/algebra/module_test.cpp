#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;

using namespace dedekind::algebra;

TEST_CASE("Modules: Integer Polynomial Action", "[algebra][modules]") {
  SECTION("Polynomial Carrier over Z") {
    // p(x) = 3x^2 + 2x + 5
    // Coeffs in vector: {5, 2, 3} (constant first)
    Polynomial<int> p({5, 2, 3});
    CHECK(p.degree() == 2);
    CHECK_FALSE(p.is_zero());
  }
}
