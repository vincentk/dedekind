#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <vector>

import dedekind.category;
import dedekind.numbers;
import dedekind.order; // the point-free halfspace grammar

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;
using namespace dedekind::order;

TEST_CASE("Numbers: ExtensionalCardinal<> one-word semantics",
          "[numbers][cardinality][cardinal]") {
  using C1 = ExtensionalCardinal<>;
  const std::size_t max = std::numeric_limits<std::size_t>::max();

  SECTION("size_t embeds implicitly; results stay ExtensionalCardinal") {
    const C1 x = std::size_t{3};
    const C1 y = std::size_t{4};
    const C1 sum = x + y;
    STATIC_CHECK(std::is_same_v<decltype(sum), const C1>);
    CHECK(sum.value == 7);
  }

  SECTION("checked_add detects overflow and wraps") {
    const auto res = C1::checked_add(C1{max}, C1{std::size_t{1}});
    CHECK(res.overflowed);
    CHECK(res.result.value == 0);
  }

  SECTION("A narrower word wraps at its own width") {
    using C32 = ExtensionalCardinal<std::uint32_t>;
    const auto res = C32::checked_mul(C32{std::uint32_t{1} << 16},
                                      C32{std::uint32_t{1} << 16});
    CHECK(res.overflowed);
    CHECK(res.result.value == 0);
  }
}

TEST_CASE("Numbers: ExtensionalCardinal additive inverse",
          "[numbers][cardinality][inverse]") {
  using C1 = ExtensionalCardinal<>;

  SECTION("inverse() free function returns additive inverse") {
    const C1 three{3};
    const C1 neg_three = inverse(three, std::plus<C1>{});
    // In two's-complement wrapping arithmetic: 3 + (-3) == 0
    CHECK((three + neg_three) == C1{0});
  }

  SECTION("Additive inverse of zero is zero") {
    const C1 zero{0};
    CHECK(inverse(zero, std::plus<C1>{}) == C1{0});
  }
}

TEST_CASE("Numbers: Numeral, ℕ above the word",
          "[numbers][cardinality][numeral]") {
  using dedekind::sets::Numeral;
  constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
  const Numeral t{top};
  SECTION("a cube of 2^64 - 1 has three limbs, and the word is left behind") {
    const Numeral cube = t * t * t;
    CHECK(cube.limb_count() == 3);
    CHECK_FALSE(cube.word().has_value());
    CHECK(t.word() == std::size_t{top});
    // (2^64 - 1)^3 = 2^192 - 3·2^128 + 3·2^64 - 1
    CHECK(cube == Numeral(std::vector<std::uint64_t>{top, 2, top - 2}));
  }
  SECTION("the same value, however written") {
    CHECK((t + Numeral{1u}) * (t + Numeral{1u}) ==
          (t + Numeral{1u}) + (t + Numeral{1u}) * t);
    CHECK(Numeral{2u} + Numeral{2u} == Numeral{2u} * Numeral{2u});
  }
  SECTION("the order crosses the word's edge") {
    CHECK(t < t + Numeral{1u});
    CHECK(t * t > t + t);
    CHECK(dedekind::sets::predecessor(t + Numeral{1u}) == t);
  }
}

TEST_CASE("Numbers: Numeral divides, and Euclid runs under Lamé's bound",
          "[numbers][cardinality][numeral][division]") {
  const Numeral top{std::numeric_limits<std::uint64_t>::max()};
  const Numeral two_64 = top + Numeral{1u};
  SECTION("one limb each: the word's division, no allocation") {
    CHECK(Numeral{7u} / Numeral{2u} == Numeral{3u});
    CHECK(Numeral{7u} % Numeral{2u} == Numeral{1u});
    CHECK((Numeral{7u} / Numeral{2u}).limb_count() == 1);
  }
  SECTION("past the word: shift and subtract over the dividend's bits") {
    const Numeral a = two_64 * Numeral{7u} + Numeral{3u};
    CHECK(a / Numeral{7u} == two_64);
    CHECK(a % Numeral{7u} == Numeral{3u});
    CHECK(a / two_64 == Numeral{7u});
    CHECK(a % two_64 == Numeral{3u});
    CHECK((a / two_64) * two_64 + a % two_64 == a);
    CHECK(two_64 * two_64 / (two_64 * two_64) == Numeral{1u});
  }
  SECTION("Euclid: a common factor above the word, and coprime neighbours") {
    CHECK(gcd(two_64 * Numeral{6u}, two_64 * Numeral{4u}) ==
          two_64 * Numeral{2u});
    CHECK(gcd(two_64 + Numeral{1u}, two_64) == Numeral{1u});
    CHECK(gcd(Numeral{}, two_64) == two_64);
  }
}
