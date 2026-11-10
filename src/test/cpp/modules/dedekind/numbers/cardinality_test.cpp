#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <variant>
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

// Compile-time contract pinning for #415: variant carrier ↔ std::integral
// relational ops MUST be constexpr-evaluable.  These static_asserts lock
// the contract that downstream halfspace machinery relies on — for
// example, @c element<𝔸<ℤ>> @c > @c -21 after the @c #402 retarget.
namespace {
constexpr auto sa_z5 = finite_signed_cardinality(5);
constexpr auto sa_z_neg3 = finite_signed_cardinality(-3);
constexpr auto sa_z_pos_inf = SignedCardinality{PositiveInfinity{}};
constexpr auto sa_z_neg_inf = SignedCardinality{NegativeInfinity{}};

// --- SignedCardinality × std::integral (forward and reversed) ---
//   * signed-int axis
static_assert(sa_z5 > -3);
static_assert(sa_z5 < 10);
static_assert(sa_z5 >= 5);
static_assert(sa_z5 <= 5);
static_assert(sa_z5 == 5);
static_assert(sa_z_neg3 < 0);
static_assert(sa_z_neg3 > -10);
static_assert(sa_z_neg3 == -3);
//   * unsigned-int axis (positive finite + negative-finite-vs-unsigned)
static_assert(sa_z5 > 4u);
static_assert(sa_z5 == 5u);
static_assert(sa_z_neg3 < 0u);  // negative SignedCardinality < any unsigned
static_assert(sa_z_neg3 < 5u);
//   * bool axis
static_assert(sa_z5 > false);
static_assert(sa_z5 > true);
static_assert(finite_signed_cardinality(1) == true);
static_assert(finite_signed_cardinality(0) == false);
//   * ±ℵ_0 dominates any finite int unconditionally
static_assert(sa_z_pos_inf > 1'000'000);
static_assert(sa_z_pos_inf > std::numeric_limits<unsigned>::max());
static_assert(sa_z_neg_inf < -1'000'000);
//   * reversed direction (T on LHS)
static_assert(-3 < sa_z5);
static_assert(0 > sa_z_neg3);
static_assert(5 == sa_z5);
static_assert(4u < sa_z5);
static_assert(5u == sa_z5);
static_assert(false < sa_z5);
}  // namespace

TEST_CASE(
    "Numbers: SignedCardinality heterogeneous comparison vs std::integral "
    "(#415)",
    "[numbers][cardinality][order][heterogeneous][signed]") {
  const SignedCardinality pos_five = finite_signed_cardinality(5);
  const SignedCardinality neg_three = finite_signed_cardinality(-3);
  const SignedCardinality pos_inf{PositiveInfinity{}};
  const SignedCardinality neg_inf{NegativeInfinity{}};
  const SignedCardinality naz{NaZ{}};

  SECTION("Positive finite vs signed and unsigned int") {
    CHECK(pos_five > -3);
    CHECK(pos_five > 0);
    CHECK(pos_five > 4);
    CHECK(pos_five < 6);
    CHECK(pos_five >= 5);
    CHECK(pos_five <= 5);
    CHECK(pos_five == 5);
    CHECK(pos_five > 4u);
    CHECK(pos_five == 5u);
  }
  SECTION("Negative finite vs signed and unsigned int") {
    CHECK(neg_three < 0);
    CHECK(neg_three < -2);
    CHECK(neg_three > -4);
    CHECK(neg_three == -3);
    CHECK_FALSE(neg_three == 3);
    // Negative SignedCardinality is strictly less than any unsigned.
    CHECK(neg_three < 0u);
    CHECK(neg_three < 5u);
  }
  SECTION("±ℵ_0 dominates / is dominated unconditionally") {
    CHECK(pos_inf > std::numeric_limits<int>::max());
    CHECK(pos_inf > 0);
    CHECK(pos_inf > -1);
    CHECK(pos_inf > 0u);
    CHECK_FALSE(pos_inf == 0);
    CHECK(neg_inf < std::numeric_limits<int>::min());
    CHECK(neg_inf < 0);
    CHECK(neg_inf < -1);
    CHECK_FALSE(neg_inf == 0);
  }
  SECTION("NaZ propagates as unordered: all relational ops false, == false") {
    CHECK_FALSE(naz < 0);
    CHECK_FALSE(naz <= 0);
    CHECK_FALSE(naz > 0);
    CHECK_FALSE(naz >= 0);
    CHECK_FALSE(naz == 0);
    CHECK(naz != 0);  // unordered semantics: NaZ is not equal to anything
  }
  SECTION("bool is std::integral on the SignedCardinality side too") {
    const SignedCardinality one = finite_signed_cardinality(1);
    const SignedCardinality zero = finite_signed_cardinality(0);
    CHECK(one == true);
    CHECK(zero == false);
    CHECK(one > false);
    CHECK(zero < true);
  }
  SECTION("Rhs-first direction synthesised by C++20 rewrite rules") {
    CHECK(-3 < pos_five);
    CHECK(0u < pos_five);
    CHECK(5 == pos_five);
    CHECK(0 > neg_three);
    CHECK(-3 == neg_three);
  }
}

TEST_CASE("Numbers: variant carriers ↔ std::floating_point comparison (#428)",
          "[numbers][cardinality][order][floating]") {
  using std::numeric_limits;
  const auto pos_inf_d = numeric_limits<double>::infinity();
  const auto neg_inf_d = -numeric_limits<double>::infinity();
  const auto nan_d = numeric_limits<double>::quiet_NaN();

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
    // Rhs-first direction (synthesised by C++20 rewrite rules).
    CHECK(3.0 == pos_three);
    CHECK(-3.0 == neg_three);
    CHECK(2.5 < pos_three);
    CHECK(pos_inf_d == pos_inf_sc);
    CHECK(neg_inf_d == neg_inf_sc);
    // Precision regime: same 2^53 rounding-fake guard as on the ℕ side,
    // applied to both signs.
    const auto two_to_53_z = finite_signed_cardinality(1LL << 53);
    const auto neg_two_to_53_z = finite_signed_cardinality(-(1LL << 53));
    const double two_to_53_d = static_cast<double>(1LL << 53);
    CHECK(two_to_53_z == two_to_53_d);
    CHECK(neg_two_to_53_z == -two_to_53_d);
    // Non-integer rhs at integer lhs.
    CHECK(pos_three != 3.25);
    CHECK(pos_three < 3.25);
    CHECK(neg_three > -3.25);
    // Mixed sign: any positive ≠ any negative.
    CHECK_FALSE(pos_three == -3.0);
    CHECK(pos_three > -3.0);
  }
}

TEST_CASE(
    "sets:cardinality: signed cardinalities as a term's constants are "
    "ordered totally, NaZ placed last (#884)",
    "[sets][cardinality][terms]") {
  using SC = SignedCardinality;
  using T = Term<SC>;
  const T three = T::constant(SC{SignedExtensionalCardinal<>{3}});
  const T five = T::constant(SC{SignedExtensionalCardinal<>{5}});
  const T inf = T::constant(SC{PositiveInfinity{}});
  const T naz = T::constant(SC{NaZ{}});
  CHECK(structural_order(three, five) < 0);
  CHECK(structural_order(five, inf) < 0);
  CHECK(structural_order(inf, naz) < 0);  // <=> leaves NaZ unordered
  CHECK(structural_order(naz, naz) == 0);
}

TEST_CASE("Numbers: Numeral, ℕ above the word",
          "[numbers][cardinality][numeral]") {
  using dedekind::sets::Numeral;
  constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
  const Numeral t{top};
  SECTION("a cube of 2^64 - 1 has three limbs, and the word is left behind") {
    const Numeral cube = t * t * t;
    CHECK(cube.limbs().size() == 3);
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
