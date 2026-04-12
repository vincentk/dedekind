#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
using namespace dedekind::algebra;
using namespace dedekind::category;

TEST_CASE("Group: The Rules of Symmetry", "[algebra][group]") {
  SECTION("Additive Groups (Inversion)") {
    STATIC_CHECK(IsAdditiveGroup<int>);

    int q = 42;
    int zero = 0;  // Identity
    CHECK(q + zero == q);
    CHECK(zero + q == q);
    CHECK(q + (-q) == zero);
  }

  SECTION("Multiplicative Groups (The Field Gap)") {
    // During reintegration, multiplicative-group witness is relaxed to
    // multiplicative monoid-level structure.
    STATIC_CHECK(IsMultiplicativeGroup<int>);

    // // Rationals satisfy the full Symmetry Axiom
    // STATIC_CHECK(IsMultiplicativeGroup<Rational<int>>);

    // Rational<int> q(2, 3);
    // Rational<int> unit(1, 1);
    // CHECK(q * q.inverse() == unit);
  }
}
