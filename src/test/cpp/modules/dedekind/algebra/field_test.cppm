#include <catch2/catch_test_macros.hpp>
import dedekind.algebra;

using namespace dedekind::algebra;

TEST_CASE("Algebra: Euclidean Domain R[x]", "[algebra][euclidean]") {
  using ℤ = int;

  // a(x) = x² - 1, b(x) = x - 1
  Polynomial<ℤ> a({-1, 0, 1});
  Polynomial<ℤ> b({-1, 1});

  SECTION("Exact Division") {
    auto [q, r] = div_rem(a, b);

    // (x² - 1) / (x - 1) = x + 1
    REQUIRE(q.degree() == 1);
    REQUIRE(q(0) == 1);
    REQUIRE(r.is_zero());
  }

  SECTION("Division with Remainder") {
    // a(x) = x² (degree 2), b(x) = x - 1 (degree 1)
    Polynomial<ℤ> a2({0, 0, 1});
    auto [q, r] = div_rem(a2, b);

    // x² = (x + 1)(x - 1) + 1
    REQUIRE(r(0) == 1);
  }
}
