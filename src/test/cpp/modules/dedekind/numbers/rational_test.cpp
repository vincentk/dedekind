#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <concepts>
#include <cstdint>
#include <limits>
#include <optional>

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::morphologies;
using namespace dedekind::numbers;
using dedekind::sets::canonical;
using dedekind::sets::IntExpr;
using dedekind::sets::RatExpr;

TEST_CASE("Numbers: ℚ's canonical form is the reduced fraction",
          "[numbers][rational][canonical]") {
  using Q = RatExpr<>;
  // 12/18 is 2/3: the canonical form is reduced, its size that of Div(2, 3).
  const Q q{12, 18};
  CHECK(q == Q{2, 3});
  CHECK(canonical(q).size() == 3);
  // The sign moves to the numerator; a unit denominator leaves ℤ's form.
  CHECK(canonical(Q{2, -4}) == Q{-1, 2});
  CHECK(canonical(Q{6, 3}).size() == 1);
  CHECK(canonical(Q{0, 7}).size() == 1);
}

TEST_CASE("Numbers: ℚ's order", "[numbers][rational][ordering]") {
  using Q = RatExpr<>;
  CHECK((Q{1, 3} <=> Q{1, 2}) == std::strong_ordering::less);
  CHECK((Q{3, 4} <=> Q{1, 2}) == std::strong_ordering::greater);
  CHECK((Q{2, 4} <=> Q{1, 2}) == std::strong_ordering::equal);
  CHECK(Q{-1, 2} < Q{});
}

TEST_CASE("Numbers: the field ℚ, dividing by ℚ*", "[numbers][field]") {
  using Q = RatExpr<>;
  const Q a{3, 4};
  const auto a_inv = nonzero(a).value().inverse();  // 4/3, in ℚ*
  CHECK(a * a_inv == Q{1});
  CHECK(Q{1, 2} / nonzero(Q{1, 4}).value() == Q{2});
  // Zero has no inverse: the check into ℚ* refuses it.
  CHECK_FALSE(nonzero(Q{}).has_value());
  CHECK(nonzero(Q{0, 1}) == std::nullopt);
}

TEST_CASE("Numbers: ℚ past the word, both signs",
          "[numbers][rational][canonical]") {
  using Q = RatExpr<>;
  const Q w = Q::atom(std::numeric_limits<std::uint64_t>::max());
  const Q two_64 = w + Q{1};
  // (2^64)/(2^64·3) is 1/3, reduced through a gcd above the word.
  CHECK(two_64 / nonzero(two_64 * Q{3}).value() == Q{1, 3});
  CHECK(-two_64 / nonzero(two_64 * Q{-6}).value() == Q{1, 6});
  CHECK(Q{1} / nonzero(two_64).value() > Q{});
  // 2^53 + 1 and the double 2^53: equal under C++'s conversion, not in ℚ.
  CHECK(Q{(1LL << 53) + 1} != static_cast<double>(1LL << 53));
}

// =============================================================================
// from / embed_𝕃_ℚ — the exact dyadic embedding of the finite doubles into ℚ
// =============================================================================

TEST_CASE("embed_𝕃_ℚ: doubles are the dyadic rationals their bits spell",
          "[numbers][rational][embedding][float]") {
  using Q = RatExpr<>;
  CHECK(embed_𝕃_ℚ(𝕃<double>{3.0}) == Q{3});
  CHECK(embed_𝕃_ℚ(𝕃<double>{-21.0}) == Q{-21});
  CHECK(embed_𝕃_ℚ(𝕃<double>{0.0}) == Q{});
  CHECK(embed_𝕃_ℚ(𝕃<double>{-0.0}) == Q{});
  CHECK(embed_𝕃_ℚ(𝕃<double>{0.5}) == Q{1, 2});
  CHECK(embed_𝕃_ℚ(𝕃<double>{-0.75}) == Q{-3, 4});
  CHECK(embed_𝕃_ℚ(𝕃<double>{1.5}) == Q{3, 2});
  // The extremes stay within the declared bound of a term.
  CHECK(from(std::numeric_limits<double>::denorm_min()).has_value());
  CHECK(from(std::numeric_limits<double>::max()).has_value());
  CHECK_FALSE(from(std::numeric_limits<double>::quiet_NaN()).has_value());
  CHECK_FALSE(from(-std::numeric_limits<double>::infinity()).has_value());
}

TEST_CASE("embed_𝕃_ℚ: injectivity (the monic property)",
          "[numbers][rational][embedding][float][injectivity]") {
  STATIC_CHECK(
      dedekind::category::IsMonicArrow<std::decay_t<decltype(embed_𝕃_ℚ)>>);
  CHECK(embed_𝕃_ℚ(𝕃<double>{3.0}) == embed_𝕃_ℚ(𝕃<double>{3.0}));
  CHECK(embed_𝕃_ℚ(𝕃<double>{3.0}) != embed_𝕃_ℚ(𝕃<double>{2.0}));
  CHECK(embed_𝕃_ℚ(𝕃<double>{0.5}) != embed_𝕃_ℚ(𝕃<double>{0.25}));
  CHECK(embed_𝕃_ℚ(𝕃<double>{-0.75}) != embed_𝕃_ℚ(𝕃<double>{0.75}));
}
