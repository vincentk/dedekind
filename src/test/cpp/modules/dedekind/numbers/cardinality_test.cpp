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

TEST_CASE("Numbers: ℕ's terms, above the word",
          "[numbers][cardinality][natural]") {
  using N = dedekind::sets::NatExpr<>;
  constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
  const N t = N::atom(top);
  const N one = N::atom(1);
  SECTION("(2^64)^3 is the binomial expansion of (t + 1)^3") {
    const N s = t + one;
    CHECK(s * s * s == t * t * t + N::atom(3) * t * t + N::atom(3) * t + one);
    CHECK(dedekind::sets::canonical(s * s * s).size() > 1);
  }
  SECTION("the same value, however written") {
    CHECK((t + one) * (t + one) == (t + one) + (t + one) * t);
    CHECK(N::atom(2) + N::atom(2) == N::atom(2) * N::atom(2));
  }
  SECTION("the order crosses the word's edge") {
    CHECK(t < t + one);
    CHECK(t * t > t + t);
    CHECK(dedekind::sets::predecessor(t + one) == t);
  }
}
