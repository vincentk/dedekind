#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <concepts>
#include <limits>
#include <optional>

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::morphologies;
using namespace dedekind::numbers;
using dedekind::sets::Integer;
using dedekind::sets::Numeral;

TEST_CASE("Numbers: Rational Simplification", "[numbers][rational]") {
  SECTION("Euclid lives in ℕ: the gcd of two integers is a natural") {
    REQUIRE(gcd(Integer{12}, Integer{18}) == Numeral{6u});
    REQUIRE(gcd(Integer{-12}, Integer{18}) == Numeral{6u});
  }

  SECTION("Integer Fractions") {
    Rational q(12, 18);

    // 12/18 simplified via GCD(12, 18)=6 -> 2/3
    REQUIRE(q.num() == Integer{2});
    REQUIRE(q.den() == Numeral{3u});
  }
}

TEST_CASE("Numbers: Rational Ordering", "[numbers][rational][ordering]") {
  using Q = Rational;

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
  using RationalValue = Rational;

  SECTION("Multiplicative Identity: (a/b) * (b/a) = 1") {
    RationalValue a(3, 4);
    const auto a_inv = nonzero(a).value().inverse();  // 4/3, in ℚ*

    auto identity = a * a_inv;

    // Should simplify to 1/1
    REQUIRE(identity.num() == Integer{1});
    REQUIRE(identity.den() == Numeral{1u});
  }

  SECTION("Division Morphism") {
    RationalValue a(1, 2);
    RationalValue b(1, 4);

    // (1/2) / (1/4) = 2: the divisor is certified first
    auto res = a / nonzero(b).value();
    REQUIRE(res.num() == Integer{2});
    REQUIRE(res.den() == Numeral{1u});
  }

  SECTION("Zero has no inverse: the check into ℚ* refuses it") {
    CHECK_FALSE(nonzero(RationalValue{}).has_value());
    CHECK(nonzero(RationalValue{0, 1}) == std::nullopt);
  }
}

// =============================================================================
// embed_𝕃_ℚ — the exact dyadic embedding of the finite doubles into ℚ
// =============================================================================

TEST_CASE("embed_𝕃_ℚ: integer-valued doubles round-trip exactly",
          "[numbers][rational][embedding][float]") {
  using Rat = Rational;

  // Small positive integer-valued double.
  const Rat r_three = embed_𝕃_ℚ(𝕃<double>{3.0});
  CHECK(r_three.num() == Integer{3});
  CHECK(r_three.den() == Numeral{1u});

  // Negative integer-valued double — the showcase-7 bound case.
  const Rat r_minus_twenty_one = embed_𝕃_ℚ(𝕃<double>{-21.0});
  CHECK(r_minus_twenty_one.num() == Integer{-21});
  CHECK(r_minus_twenty_one.den() == Numeral{1u});

  // Zero (both ±0).
  const Rat r_zero = embed_𝕃_ℚ(𝕃<double>{0.0});
  CHECK(r_zero.num() == Integer{0});
  CHECK(r_zero.den() == Numeral{1u});
  const Rat r_neg_zero = embed_𝕃_ℚ(𝕃<double>{-0.0});
  CHECK(r_neg_zero.num() == Integer{0});
  CHECK(r_neg_zero.den() == Numeral{1u});
}

TEST_CASE("embed_𝕃_ℚ: dyadic rationals decompose to lowest terms",
          "[numbers][rational][embedding][float][dyadic]") {
  using Rat = Rational;

  // 0.5 = 1/2.
  const Rat r_half = embed_𝕃_ℚ(𝕃<double>{0.5});
  CHECK(r_half.num() == Integer{1});
  CHECK(r_half.den() == Numeral{2u});

  // 0.25 = 1/4.
  const Rat r_quarter = embed_𝕃_ℚ(𝕃<double>{0.25});
  CHECK(r_quarter.num() == Integer{1});
  CHECK(r_quarter.den() == Numeral{4u});

  // -0.75 = -3/4.
  const Rat r_minus_three_quarters = embed_𝕃_ℚ(𝕃<double>{-0.75});
  CHECK(r_minus_three_quarters.num() == Integer{-3});
  CHECK(r_minus_three_quarters.den() == Numeral{4u});

  // 1.5 = 3/2.
  const Rat r_three_halves = embed_𝕃_ℚ(𝕃<double>{1.5});
  CHECK(r_three_halves.num() == Integer{3});
  CHECK(r_three_halves.den() == Numeral{2u});
}

TEST_CASE("embed_𝕃_ℚ: injectivity (the monic property)",
          "[numbers][rational][embedding][float][injectivity]") {
  // The arrow is registered monic; this exercises injectivity on values.
  STATIC_CHECK(
      dedekind::category::IsMonicArrow<std::decay_t<decltype(embed_𝕃_ℚ)>>);
  using Rat = Rational;
  const Rat r_three_a = embed_𝕃_ℚ(𝕃<double>{3.0});
  const Rat r_three_b = embed_𝕃_ℚ(𝕃<double>{3.0});  // same input → same output
  CHECK(r_three_a == r_three_b);

  // Distinct in-range inputs map to distinct rationals.
  CHECK(embed_𝕃_ℚ(𝕃<double>{3.0}) != embed_𝕃_ℚ(𝕃<double>{2.0}));
  CHECK(embed_𝕃_ℚ(𝕃<double>{0.5}) != embed_𝕃_ℚ(𝕃<double>{0.25}));
  CHECK(embed_𝕃_ℚ(𝕃<double>{-0.75}) != embed_𝕃_ℚ(𝕃<double>{0.75}));
}
