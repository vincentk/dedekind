#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <concepts>
#include <limits>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

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

// =============================================================================
// embed_double_ℚ — the exact dyadic embedding std::floating_point → ℚ (#399)
// =============================================================================

TEST_CASE("embed_double_ℚ: integer-valued doubles round-trip exactly",
          "[numbers][rational][embedding][float]") {
  using Rat = Rational<default_integer>;

  // Small positive integer-valued double.
  const Rat r_three = embed_double_ℚ<>(3.0);
  CHECK(r_three.num() == 3);
  CHECK(r_three.den() == 1);

  // Negative integer-valued double — the showcase-7 bound case.
  const Rat r_minus_twenty_one = embed_double_ℚ<>(-21.0);
  CHECK(r_minus_twenty_one.num() == -21);
  CHECK(r_minus_twenty_one.den() == 1);

  // Zero (both ±0).
  const Rat r_zero = embed_double_ℚ<>(0.0);
  CHECK(r_zero.num() == 0);
  CHECK(r_zero.den() == 1);
  const Rat r_neg_zero = embed_double_ℚ<>(-0.0);
  CHECK(r_neg_zero.num() == 0);
  CHECK(r_neg_zero.den() == 1);
}

TEST_CASE("embed_double_ℚ: dyadic rationals decompose to lowest terms",
          "[numbers][rational][embedding][float][dyadic]") {
  using Rat = Rational<default_integer>;

  // 0.5 = 1/2.
  const Rat r_half = embed_double_ℚ<>(0.5);
  CHECK(r_half.num() == 1);
  CHECK(r_half.den() == 2);

  // 0.25 = 1/4.
  const Rat r_quarter = embed_double_ℚ<>(0.25);
  CHECK(r_quarter.num() == 1);
  CHECK(r_quarter.den() == 4);

  // -0.75 = -3/4.
  const Rat r_minus_three_quarters = embed_double_ℚ<>(-0.75);
  CHECK(r_minus_three_quarters.num() == -3);
  CHECK(r_minus_three_quarters.den() == 4);

  // 1.5 = 3/2.
  const Rat r_three_halves = embed_double_ℚ<>(1.5);
  CHECK(r_three_halves.num() == 3);
  CHECK(r_three_halves.den() == 2);
}

TEST_CASE("embed_double_ℚ: NaN and ±∞ are out-of-band (throw at runtime)",
          "[numbers][rational][embedding][float][rejection]") {
  // NaN: rejected.
  REQUIRE_THROWS_AS(embed_double_ℚ<>(std::numeric_limits<double>::quiet_NaN()),
                    std::domain_error);

  // +∞: rejected.
  REQUIRE_THROWS_AS(embed_double_ℚ<>(std::numeric_limits<double>::infinity()),
                    std::domain_error);

  // -∞: rejected.
  REQUIRE_THROWS_AS(embed_double_ℚ<>(-std::numeric_limits<double>::infinity()),
                    std::domain_error);
}

TEST_CASE("embed_double_ℚ: registered as a monic arrow (IsInjective fires)",
          "[numbers][rational][embedding][float][category]") {
  using ArrowT = std::decay_t<decltype(embed_double_ℚ<>)>;
  STATIC_CHECK(dedekind::category::is_monic_arrow_v<ArrowT>);
  STATIC_CHECK(dedekind::category::IsInjective<ArrowT>);
}
