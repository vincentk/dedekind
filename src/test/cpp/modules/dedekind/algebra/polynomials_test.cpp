#include <catch2/catch_test_macros.hpp>
import dedekind.algebra;

using namespace dedekind::algebra;

TEST_CASE("Algebra: Polynomial Morphisms", "[algebra][polynomial]") {
  using ℤ = int;

  // Coefficients are stored constant-first.
  Polynomial<ℤ> p({-2, 0, 1});

  SECTION("Basic Structural Shape") {
    REQUIRE(p.degree() == 2);
    REQUIRE(!p.is_zero());
  }

  SECTION("Additive and Multiplicative Identities") {
    auto z = Polynomial<ℤ>::zero();
    auto o = Polynomial<ℤ>::one();
    REQUIRE(z.is_zero());
    REQUIRE(o.degree() == 0);
  }
}
