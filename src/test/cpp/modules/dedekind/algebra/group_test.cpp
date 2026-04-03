#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
using namespace dedekind::algebra;

TEST_CASE("Group: The Rules of Symmetry", "[algebra][group]") {
  SECTION("Additive Groups (Inversion)") {
    STATIC_CHECK(IsAdditiveGroup<int>);
    STATIC_CHECK(IsAdditiveGroup<Rational<int>>);

    Rational<int> q(3, 4);
    Rational<int> zero(0, 1);
    CHECK(q + (-q) == zero);
  }

  SECTION("Multiplicative Groups (The Field Gap)") {
    // THE AXIOMATIC GUARD:
    // Integers satisfy Monoid (1) but not Group (1/x)
    STATIC_CHECK_FALSE(IsMultiplicativeGroup<int>);

    // Rationals satisfy the full Symmetry Axiom
    STATIC_CHECK(IsMultiplicativeGroup<Rational<int>>);

    Rational<int> q(2, 3);
    Rational<int> unit(1, 1);
    CHECK(q * q.inverse() == unit);
  }
}
