#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;

using namespace dedekind::algebra;

TEST_CASE("Modules: Integer Polynomial Action", "[algebra][modules]") {
  SECTION("Horner's Method over Z") {
    // p(x) = 3x^2 + 2x + 5
    // Coeffs in vector: {5, 2, 3} (constant first)
    Polynomial<int> p({5, 2, 3});

    // Evaluate p(2) = 3(4) + 2(2) + 5 = 12 + 4 + 5 = 21
    int x = 2;
    int result = evaluate(p, x);

    CHECK(result == 21);
  }
}
