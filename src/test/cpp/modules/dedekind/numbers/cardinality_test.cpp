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
using namespace dedekind::sets;

TEST_CASE("Numbers: Cardinality draft policy", "[numbers][cardinality]") {
  SECTION("Finite and ℵ_0 comparison policy") {
    const Cardinality finite_three = finite_cardinality(3);
    const Cardinality inf = ℵ_0{};

    CHECK(compare(finite_three, inf) == std::strong_ordering::less);
    CHECK(compare(inf, finite_three) == std::strong_ordering::greater);
    CHECK(compare(inf, inf) == std::strong_ordering::equal);
  }

  SECTION("Addition and multiplication absorb to ℵ_0") {
    const Cardinality finite_five = finite_cardinality(5);
    const Cardinality finite_six = finite_cardinality(6);
    const Cardinality inf = ℵ_0{};

    CHECK(add(finite_five, finite_six) == finite_cardinality(11));
    CHECK(add(finite_five, inf) == Cardinality{ℵ_0{}});

    CHECK(mul(finite_five, finite_six) == finite_cardinality(30));
    CHECK(mul(finite_five, inf) == Cardinality{ℵ_0{}});
  }

  SECTION("Realization to size_t remains explicit") {
    const std::size_t sentinel = std::numeric_limits<std::size_t>::max() - 17;

    CHECK(realize_to_size_t(finite_cardinality(42), sentinel) == 42);
    CHECK(realize_to_size_t(Cardinality{ℵ_0{}}, sentinel) == sentinel);
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

  SECTION("add_or_ℵ_0 converts 1-limb overflow to ℵ_0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{1};
    const auto result = add_or_ℵ_0(a, b);
    REQUIRE(std::holds_alternative<ℵ_0>(result));
  }

  SECTION("mul_or_ℵ_0 converts large-product overflow to ℵ_0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{2};
    const auto result = mul_or_ℵ_0(a, b);
    REQUIRE(std::holds_alternative<ℵ_0>(result));
  }

  SECTION("ExtensionalCardinal<2> addition overflow into variant-level ℵ_0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    C2 big{};
    big.limbs[0] = max;
    big.limbs[1] = max;
    const C2 one = std::size_t{1};
    const auto result = add_or_ℵ_0(big, one);
    REQUIRE(std::holds_alternative<ℵ_0>(result));
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

TEST_CASE(
    "Numbers: Cardinality heterogeneous comparison vs std::integral (#415)",
    "[numbers][cardinality][order][heterogeneous]") {
  const Cardinality five = finite_cardinality(5);
  const Cardinality inf = ℵ_0{};

  SECTION("Finite vs unsigned and signed (non-negative) — three-way") {
    CHECK((five <=> 5u) == std::strong_ordering::equal);
    CHECK((five <=> 4u) == std::strong_ordering::greater);
    CHECK((five <=> 6u) == std::strong_ordering::less);
    CHECK((five <=> 5) == std::strong_ordering::equal);  // signed int, rhs >= 0
    CHECK(five == 5u);
    CHECK(five == 5);
    CHECK(five > 4u);
    CHECK(five < 6);
  }
  SECTION("Negative signed values land strictly below the ℕ proxy") {
    // No element of Cardinality is negative; finite > any negative int.
    CHECK((five <=> -1) == std::strong_ordering::greater);
    CHECK(five > -1);
    CHECK_FALSE(five == -1);
    // Even the empty cardinal (zero) is greater than a negative int.
    const Cardinality zero = finite_cardinality(0);
    CHECK(zero > -1);
    CHECK_FALSE(zero == -7);
  }
  SECTION("ℵ_0 dominates every finite int") {
    CHECK((inf <=> 0u) == std::strong_ordering::greater);
    CHECK(inf > std::numeric_limits<unsigned>::max());
    CHECK(inf > -1);
    CHECK_FALSE(inf == 0u);
  }
  SECTION("bool is std::integral (regression for make_unsigned_t<bool>)") {
    // std::make_unsigned_t<bool> is undefined; the comparison path must
    // delegate through ExtensionalCardinal<>'s integral ctor instead.
    const Cardinality one = finite_cardinality(1);
    const Cardinality zero = finite_cardinality(0);
    CHECK(one == true);
    CHECK(zero == false);
    CHECK(one > false);
    CHECK(zero < true);
  }
  SECTION("Rhs-first direction synthesised by C++20 rewrite rules") {
    CHECK(5u == five);
    CHECK(4 < five);
    CHECK(0u < inf);
  }
}

TEST_CASE("Numbers: Cardinality homogeneous operator surface (#424)",
          "[numbers][cardinality][operators]") {
  const Cardinality three = finite_cardinality(3);
  const Cardinality four = finite_cardinality(4);
  const Cardinality inf = ℵ_0{};

  SECTION("operator+ wraps add() with ℵ_0 absorption") {
    CHECK((three + four) == finite_cardinality(7));
    CHECK((three + inf) == inf);
    CHECK((inf + inf) == inf);
  }
  SECTION("operator* wraps mul() with ℵ_0 absorption") {
    CHECK((three * four) == finite_cardinality(12));
    CHECK((three * inf) == inf);
    CHECK((inf * inf) == inf);
  }
  SECTION("operator<=> wraps compare(); finite < ℵ_0") {
    CHECK((three <=> four) == std::strong_ordering::less);
    CHECK((four <=> three) == std::strong_ordering::greater);
    CHECK((three <=> three) == std::strong_ordering::equal);
    CHECK((three <=> inf) == std::strong_ordering::less);
    CHECK((inf <=> three) == std::strong_ordering::greater);
    CHECK((inf <=> inf) == std::strong_ordering::equal);
  }
  SECTION("Four partial-order operators synthesised from <=>") {
    CHECK(three < four);
    CHECK(four > three);
    CHECK(three <= three);
    CHECK(three >= three);
    CHECK_FALSE(three > four);
    CHECK(three < inf);
  }
  SECTION("Explicit operator== covers cross-module visibility") {
    CHECK(three == finite_cardinality(3));
    CHECK_FALSE(three == four);
    CHECK(inf == Cardinality{ℵ_0{}});
    CHECK_FALSE(three == inf);
  }
  SECTION("Euclidean division and modulo (the ℕ-closed pair)") {
    const Cardinality seven = finite_cardinality(7);
    const Cardinality two = finite_cardinality(2);
    const Cardinality zero = finite_cardinality(0);
    // Textbook: 7 = 2*3 + 1, both quotient and remainder in ℕ.
    CHECK((seven / finite_cardinality(3)) == two);
    CHECK((seven % finite_cardinality(3)) == finite_cardinality(1));
    // Division-by-zero policy (totalised; ExtensionalCardinal convention).
    CHECK((seven / zero) == zero);
    CHECK((seven % zero) == seven);
    // ℵ_0 interactions: finite / ℵ_0 = 0; ℵ_0 / finite-non-zero = ℵ_0.
    CHECK((seven / inf) == zero);
    CHECK((inf / two) == inf);
  }
}

TEST_CASE("Numbers: variant carriers ↔ std::floating_point comparison (#428)",
          "[numbers][cardinality][order][floating]") {
  using std::numeric_limits;
  const auto pos_inf_d = numeric_limits<double>::infinity();
  const auto neg_inf_d = -numeric_limits<double>::infinity();
  const auto nan_d = numeric_limits<double>::quiet_NaN();

  SECTION("Cardinality vs double — finite, ℵ_0, ±inf, NaN, negatives") {
    const Cardinality five = finite_cardinality(5);
    const Cardinality inf = ℵ_0{};
    // Finite vs finite double.
    CHECK(five == 5.0);
    CHECK(five > 4.5);
    CHECK(five < 5.5);
    // Negative double — every Cardinality > -anything.
    CHECK(five > -1.0);
    CHECK(five > neg_inf_d);
    CHECK_FALSE(five == -5.0);
    // ℵ_0 dominates every finite double; equivalent to +inf.
    CHECK(inf > 1e308);
    CHECK(inf == pos_inf_d);
    CHECK(inf > neg_inf_d);
    // NaN propagates as unordered (all comparisons false; != is true).
    CHECK_FALSE(five < nan_d);
    CHECK_FALSE(five > nan_d);
    CHECK_FALSE(five == nan_d);
    CHECK(five != nan_d);
  }
  SECTION("SignedCardinality vs double — full sign + sentinel matrix") {
    const auto pos_three = finite_signed_cardinality(3);
    const auto neg_three = finite_signed_cardinality(-3);
    const auto pos_inf_sc = SignedCardinality{PositiveInfinity{}};
    const auto neg_inf_sc = SignedCardinality{NegativeInfinity{}};
    const auto naz = SignedCardinality{NaZ{}};
    // Finite vs finite double.
    CHECK(pos_three == 3.0);
    CHECK(neg_three == -3.0);
    CHECK(pos_three > 2.5);
    CHECK(neg_three < -2.5);
    // ±ℵ_0 maps to ±inf for the comparison.
    CHECK(pos_inf_sc == pos_inf_d);
    CHECK(neg_inf_sc == neg_inf_d);
    CHECK(pos_inf_sc > 1e308);
    CHECK(neg_inf_sc < -1e308);
    // NaZ propagates as unordered; NaN rhs same.
    CHECK_FALSE(naz < 0.0);
    CHECK_FALSE(naz > 0.0);
    CHECK_FALSE(naz == 0.0);
    CHECK_FALSE(pos_three < nan_d);
    CHECK_FALSE(pos_three == nan_d);
  }
  SECTION("Cross-variant ℕ ↔ ℤ comparison (the canonical ℕ ⊂ ℤ embedding)") {
    const Cardinality five_n = finite_cardinality(5);
    const Cardinality inf_n = ℵ_0{};
    const auto five_z = finite_signed_cardinality(5);
    const auto neg_three_z = finite_signed_cardinality(-3);
    const auto pos_inf_z = SignedCardinality{PositiveInfinity{}};
    const auto neg_inf_z = SignedCardinality{NegativeInfinity{}};
    const auto naz = SignedCardinality{NaZ{}};
    // Finite ℕ vs ℤ — the lift settles equality and ordering.
    CHECK(five_n == five_z);
    CHECK(five_n > neg_three_z);  // ℕ ≥ 0 > -3
    CHECK(five_n < finite_signed_cardinality(7));
    // ℵ_0 ↦ +ℵ_0 under the lift; equivalent to PositiveInfinity.
    CHECK(inf_n == pos_inf_z);
    CHECK_FALSE(inf_n > pos_inf_z);  // equivalent, not strictly greater
    CHECK(inf_n > five_z);
    // ℕ never below -ℵ_0.
    CHECK(five_n > neg_inf_z);
    // NaZ on the ℤ side propagates as unordered.
    CHECK_FALSE(five_n < naz);
    CHECK_FALSE(five_n > naz);
    CHECK_FALSE(five_n == naz);
  }
}
