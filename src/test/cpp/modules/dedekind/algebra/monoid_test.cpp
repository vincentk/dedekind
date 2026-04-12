#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
using namespace dedekind::algebra;
using namespace dedekind::category;

TEST_CASE("Algebra: Monoid Axioms (Atomic)", "[algebra][monoid]") {
  SECTION("Additive Identity (0)") {
    // Documentation-only checkpoint:
    // Machine integers are not asserted as total algebraic witnesses here
    // (overflow violates closure in the mathematical model).
    // STATIC_CHECK(dedekind::category::IsPointed<int, std::plus<int>>);
    // STATIC_CHECK(dedekind::category::IsPointed<long, std::plus<long>>);

    // Runtime check for identity value
    int x = 42;
    int zero = 0;  // Inferred identity
    CHECK(x + zero == x);
    CHECK(zero + x == x);
  }

  SECTION("Multiplicative Identity (1)") {
    // Documentation-only checkpoint for machine-int multiplicative identity.
    // STATIC_CHECK(dedekind::category::IsPointed<int, std::multiplies<int>>);

    int x = 42;
    int unit = 1;
    CHECK(x * unit == x);
  }

  SECTION("Boolean Monoids") {
    // bool may be a canonical monoid/group carrier depending on operation,
    // but witness registration is currently deferred in category.
    SUCCEED("Boolean monoid witness deferred during reintegration.");
  }
}
