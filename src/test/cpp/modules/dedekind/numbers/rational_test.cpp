#include <catch2/catch_test_macros.hpp>
#include <compare>
import dedekind.numbers;

using namespace dedekind::numbers;

TEST_CASE("Numbers: Rational Simplification", "[numbers][rational]") {
  SECTION("Euclidean GCD Lives In The Integer Spine") {
    REQUIRE(euclidean_gcd(default_integer{12}, default_integer{18}) ==
            default_integer{6});
    REQUIRE(euclidean_gcd(default_integer{-12}, default_integer{18}) ==
            default_integer{6});
  }

  SECTION("Integer Fractions") {
    Rational<default_integer> q(12, 18);

    // 12/18 simplified via GCD(12, 18)=6 -> 2/3
    REQUIRE(q.num() == 2);
    REQUIRE(q.den() == 3);
  }
}

TEST_CASE("Numbers: Rational Ordering", "[numbers][rational][ordering]") {
  using Q = Rational<default_integer>;

  SECTION("Less-than comparison") {
    CHECK((Q{1, 3} <=> Q{1, 2}) == std::strong_ordering::less);
  }

  SECTION("Greater-than comparison") {
    CHECK((Q{3, 4} <=> Q{1, 2}) == std::strong_ordering::greater);
  }

  SECTION("Equal comparison") {
    CHECK((Q{2, 4} <=> Q{1, 2}) == std::strong_ordering::equal);
  }
}

TEST_CASE("Numbers: The Rational Field", "[numbers][field]") {
  using RationalValue = Rational<default_integer>;

  SECTION("Multiplicative Identity: (a/b) * (b/a) = 1") {
    RationalValue a(3, 4);
    auto a_inv = a.inverse();  // 4/3

    auto identity = a * a_inv;

    // Should simplify to 1/1
    REQUIRE(identity.num() == 1);
    REQUIRE(identity.den() == 1);
  }

  SECTION("Division Morphism") {
    RationalValue a(1, 2);
    RationalValue b(1, 4);

    // (1/2) / (1/4) = 2
    auto res = a / b;
    REQUIRE(res.num() == 2);
    REQUIRE(res.den() == 1);
  }

  SECTION("Zero-Denominator Protection") {
    // Reciprocal of zero must throw
    REQUIRE_THROWS_AS(RationalValue(0, 1).inverse(), std::domain_error);
  }
}
