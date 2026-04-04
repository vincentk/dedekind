#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
using namespace dedekind::algebra;
using namespace dedekind::category;

TEST_CASE("Algebra: Monoid Axioms (Atomic)", "[algebra][monoid]") {
  SECTION("Additive Identity (0)") {
    // Z under addition is a Monoid
    STATIC_CHECK(IsAdditiveMonoid<int>);
    STATIC_CHECK(IsAdditiveMonoid<long>);

    // Runtime check for identity value
    int x = 42;
    int zero = 0;  // Inferred identity
    CHECK(x + zero == x);
    CHECK(zero + x == x);
  }

  SECTION("Multiplicative Identity (1)") {
    // Z under multiplication is a Monoid
    STATIC_CHECK(IsMultiplicativeMonoid<int>);

    int x = 42;
    int unit = 1;
    CHECK(x * unit == x);
  }

  SECTION("Boolean Monoids") {
    // Booleans are monoids under logic ops
    STATIC_CHECK(dedekind::category::IsMonoid<bool, std::logical_and<>>);
    STATIC_CHECK(dedekind::category::IsMonoid<bool, std::logical_or<>>);
  }
}
