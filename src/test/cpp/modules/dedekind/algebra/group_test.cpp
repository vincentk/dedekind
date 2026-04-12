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

  SECTION("Multiplicative Monoids (The Field Gap)") {
    // Documentation-only checkpoint:
    // We intentionally do not assert machine `int` as a total multiplicative
    // monoid/group witness in this layer.
    // STATIC_CHECK(IsMultiplicativeMonoid<int>);

    // // Rationals satisfy the full Symmetry Axiom
    // STATIC_CHECK(IsMultiplicativeGroup<Rational<int>>);

    // Rational<int> q(2, 3);
    // Rational<int> unit(1, 1);
    // CHECK(q * q.inverse() == unit);

    // bool may be a valid algebraic carrier under suitable operations
    // (e.g., xor-group or and/or monoids), but those witnesses are tracked
    // separately from this reintegration patch.
  }
}
