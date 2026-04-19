#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <cstddef>
#include <functional>
#include <limits>
#include <variant>

import dedekind.category;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::numbers;

TEST_CASE("Numbers: Cardinality draft policy", "[numbers][cardinality]") {
  SECTION("Finite and aleph0 comparison policy") {
    const Cardinality finite_three = finite_cardinality(3);

    CHECK(compare(finite_three, aleph0) == std::strong_ordering::less);
    CHECK(compare(aleph0, finite_three) == std::strong_ordering::greater);
    CHECK(compare(aleph0, aleph0) == std::strong_ordering::equal);
  }

  SECTION("Addition and multiplication absorb to aleph0") {
    const Cardinality finite_five = finite_cardinality(5);
    const Cardinality finite_six = finite_cardinality(6);

    CHECK(add(finite_five, finite_six) == finite_cardinality(11));
    CHECK(add(finite_five, aleph0) == Cardinality{aleph0});

    CHECK(mul(finite_five, finite_six) == finite_cardinality(30));
    CHECK(mul(finite_five, aleph0) == Cardinality{aleph0});
  }

  SECTION("Realization to size_t remains explicit") {
    const std::size_t sentinel = std::numeric_limits<std::size_t>::max() - 17;

    CHECK(realize_to_size_t(finite_cardinality(42), sentinel) == 42);
    CHECK(realize_to_size_t(aleph0, sentinel) == sentinel);
  }
}

TEST_CASE("Numbers: ExtensionalCardinal<N> multi-limb semantics",
          "[numbers][cardinality][cardinal]") {
  using C1 = ExtensionalCardinal<1>;
  using C2 = ExtensionalCardinal<2>;

  SECTION("size_t is a Liskov subtype: implicit embedding") {
    // size_t constructs ExtensionalCardinal<N> without explicit cast
    const C1 a = std::size_t{42};
    CHECK(a.limbs[0] == 42);

    const C2 b = std::size_t{7};
    CHECK(b.limbs[0] == 7);
    CHECK(b.limbs[1] == 0);
  }

  SECTION("Operator results are ExtensionalCardinal<N>, not size_t") {
    const C1 x = std::size_t{3};
    const C1 y = std::size_t{4};
    const C1 sum = x + y;
    STATIC_CHECK(std::is_same_v<decltype(sum), const C1>);
    CHECK(sum.limbs[0] == 7);
  }

  SECTION("Carry propagates across limbs in ExtensionalCardinal<2>") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    // limb[0] = SIZE_MAX, limb[1] = 0
    C2 hi{};
    hi.limbs[0] = max;
    // Adding 1 should carry into limb[1]
    const C2 one = std::size_t{1};
    const C2 result = hi + one;
    CHECK(result.limbs[0] == 0);
    CHECK(result.limbs[1] == 1);
  }

  SECTION("checked_add detects 1-limb overflow") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{1};
    const auto res = C1::checked_add(a, b);
    CHECK(res.overflowed == true);
    CHECK(res.value.limbs[0] == 0);  // wraps to zero
  }

  SECTION("add_or_aleph0 converts 1-limb overflow to Aleph0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{1};
    const auto result = add_or_aleph0(a, b);
    REQUIRE(std::holds_alternative<Aleph0>(result));
  }

  SECTION("mul_or_aleph0 converts large-product overflow to Aleph0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{2};
    const auto result = mul_or_aleph0(a, b);
    REQUIRE(std::holds_alternative<Aleph0>(result));
  }

  SECTION(
      "ExtensionalCardinal<2> addition overflow into variant-level Aleph0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    C2 big{};
    big.limbs[0] = max;
    big.limbs[1] = max;
    const C2 one = std::size_t{1};
    const auto result = add_or_aleph0(big, one);
    REQUIRE(std::holds_alternative<Aleph0>(result));
  }

  SECTION("realize_to_size_t on ExtensionalCardinal<2> with high limb") {
    const std::size_t sentinel = 0xDEAD;
    C2 big{};
    big.limbs[0] = 99;
    big.limbs[1] = 1;  // value > SIZE_MAX
    CHECK(big.realize_to_size_t(sentinel) == sentinel);

    C2 small{};
    small.limbs[0] = 42;
    small.limbs[1] = 0;
    CHECK(small.realize_to_size_t(sentinel) == 42);
  }

  SECTION("Three-way comparison is lexicographic on limbs") {
    C2 a{};
    a.limbs[0] = 0;
    a.limbs[1] = 1;  // value = 2^64

    C2 b{};
    b.limbs[0] = std::numeric_limits<std::size_t>::max();
    b.limbs[1] = 0;  // value = SIZE_MAX < 2^64

    CHECK((a <=> b) == std::strong_ordering::greater);
    CHECK((b <=> a) == std::strong_ordering::less);
  }
}

TEST_CASE("Numbers: Cardinality witnesses and composition",
          "[numbers][cardinality][effects]") {
  SECTION("Writer-like bind respects left identity") {
    const auto f = [](const Cardinality& c) {
      return CardinalityEffect{add(c, finite_cardinality(2)),
                               finite_cardinality(2)};
    };

    const Cardinality seed = finite_cardinality(7);
    const CardinalityEffect lhs = bind_effect(unit_effect(seed), f);
    const CardinalityEffect rhs = f(seed);

    CHECK(lhs.value == rhs.value);
    CHECK(lhs.trace == rhs.trace);
  }
}