#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.algebra;
import dedekind.order;
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::sets;
using namespace dedekind::order;

TEST_CASE("Symbolic Algebra: Axiomatic Proofs", "[algebra][static]") {
  SECTION("Monoid Proof: Strings") {
    // Theorem: std::string is a Monoid under concatenation (+)
    // It has identity ("") and is associative, but no inverse.
    STATIC_REQUIRE(IsMonoid<std::string, std::plus<std::string>>);
    STATIC_REQUIRE_FALSE(IsGroup<std::string, std::plus<std::string>>);
    STATIC_REQUIRE_FALSE(is_commutative_v<std::string, std::plus<std::string>>);
  }

  SECTION("Ring Proof: Integers") {
    // Theorem: int is a Ring (Additive Abelian Group + Multiplicative Monoid)
    STATIC_REQUIRE(IsAbelianGroup<int, std::plus<int>>);
    STATIC_REQUIRE(IsMonoid<int, std::multiplies<int>>);
    STATIC_REQUIRE(IsRing<int>);

    // Theorem: int is NOT a Field (no multiplicative inverse for 2)
    STATIC_REQUIRE_FALSE(IsField<int>);
  }

  SECTION("Field Proof: Floating Point") {
    // Theorem: double satisfies the Field axioms (approximate math aside)
    STATIC_REQUIRE(IsField<double>);
  }
}

TEST_CASE("Symbolic Algebra: Minkowski Sum", "[algebra][static]") {
  auto U = UniversalSet<int, ℕ64>(ℕ64());

  SECTION("Identity Theorem: A + {0} = A") {
    auto A = closed_interval<int, decltype(U), 0, 10>(U);

    // We simulate a singleton {0} as an interval [0, 0]
    auto Zero = closed_interval<int, decltype(U), 0, 0>(U);

    auto result = A + Zero;

    // THE COOLNESS: If we implement the shortcut,
    // the type is still a ClosedIntervalNode, not a MinkowskiSumNode!
    using ResultType = decltype(result);
    STATIC_REQUIRE(is_interval_v<ResultType>);

    // Value Check
    REQUIRE(result.contains(5));
    REQUIRE_FALSE(result.contains(11));
  }

  SECTION("Interval Translation: [1, 2] + [10, 20] = [11, 22]") {
    auto I1 = closed_interval<int, decltype(U), 1, 2>(U);
    auto I2 = closed_interval<int, decltype(U), 10, 20>(U);

    auto sum = I1 + I2;

    // This proves the bounds were summed symbolically
    REQUIRE(sum.contains(11));  // 1 + 10
    REQUIRE(sum.contains(22));  // 2 + 20
    REQUIRE_FALSE(sum.contains(10));
    REQUIRE_FALSE(sum.contains(23));
  }
}
