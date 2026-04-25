/** @file dedekind/sets/signed_cardinality_test.cpp
 *
 * Runtime tests for the extended-integer carrier `SignedCardinality`
 * (variant of `SignedExtensionalCardinal<>`, `±ℵ_0`, `NaZ`).
 *
 * The static_asserts on the categorical concepts (IsAbelianGroup,
 * IsRing, IsCommutativeRing, IsSaturating) live alongside the carrier
 * in `sets/cardinality.cppm`.  This file covers the runtime arithmetic
 * semantics the concept layer cannot exercise: escalation on overflow,
 * NaZ propagation through every op, and the partial-ordering behaviour
 * the `compare_signed` switch under #377's review introduced.
 */

#include <catch2/catch_test_macros.hpp>
#include <climits>
#include <compare>
#include <variant>

import dedekind.sets;

using dedekind::sets::compare_signed;
using dedekind::sets::finite_signed_cardinality;
using dedekind::sets::NaZ;
using dedekind::sets::NegativeInfinity;
using dedekind::sets::PositiveInfinity;
using dedekind::sets::SignedCardinality;
using dedekind::sets::SignedExtensionalCardinal;

namespace {

using SEC = SignedExtensionalCardinal<>;

// Builds a finite SignedCardinality close to the magnitude wrap point
// so that adding it to itself overflows the underlying ExtensionalCardinal
// magnitude.  We do not need the literal max value --- any value whose
// doubled magnitude carries past the limb capacity triggers the
// escalation path.
constexpr SignedCardinality huge_positive() {
  SEC z;
  z.magnitude.limbs[0] = static_cast<SEC::magnitude_type::limb_type>(-1);
  return SignedCardinality{z};
}

constexpr SignedCardinality huge_negative() {
  SEC z;
  z.negative = true;
  z.magnitude.limbs[0] = static_cast<SEC::magnitude_type::limb_type>(-1);
  return SignedCardinality{z};
}

constexpr bool is_pos_inf(const SignedCardinality& v) {
  return std::holds_alternative<PositiveInfinity>(v);
}

constexpr bool is_neg_inf(const SignedCardinality& v) {
  return std::holds_alternative<NegativeInfinity>(v);
}

constexpr bool is_naz(const SignedCardinality& v) {
  return std::holds_alternative<NaZ>(v);
}

}  // namespace

TEST_CASE("SignedCardinality — finite + finite stays finite (no escalation)",
          "[sets][cardinality][signed][saturation]") {
  constexpr auto a = finite_signed_cardinality(7);
  constexpr auto b = finite_signed_cardinality(-3);

  constexpr auto sum = a + b;
  CHECK(std::holds_alternative<SEC>(sum));
  CHECK(std::get<SEC>(sum) == SEC{4});
}

TEST_CASE("SignedCardinality — same-sign overflow escalates to ±ℵ_0",
          "[sets][cardinality][signed][saturation]") {
  SECTION("Two large positives saturate to +ℵ_0") {
    const auto h = huge_positive();
    const auto sum = h + h;
    CHECK(is_pos_inf(sum));
  }

  SECTION("Two large negatives saturate to -ℵ_0") {
    const auto h = huge_negative();
    const auto sum = h + h;
    CHECK(is_neg_inf(sum));
  }
}

TEST_CASE("SignedCardinality — addition with infinities",
          "[sets][cardinality][signed][saturation]") {
  const auto pos_inf = SignedCardinality{PositiveInfinity{}};
  const auto neg_inf = SignedCardinality{NegativeInfinity{}};
  const auto five = finite_signed_cardinality(5);

  SECTION("(+ℵ_0) + finite = +ℵ_0") { CHECK(is_pos_inf(pos_inf + five)); }
  SECTION("(-ℵ_0) + finite = -ℵ_0") { CHECK(is_neg_inf(neg_inf + five)); }
  SECTION("(+ℵ_0) + (+ℵ_0) = +ℵ_0") { CHECK(is_pos_inf(pos_inf + pos_inf)); }
  SECTION("(-ℵ_0) + (-ℵ_0) = -ℵ_0") { CHECK(is_neg_inf(neg_inf + neg_inf)); }
  SECTION("(+ℵ_0) + (-ℵ_0) = NaZ (indeterminate)") {
    CHECK(is_naz(pos_inf + neg_inf));
  }
  SECTION("(-ℵ_0) + (+ℵ_0) = NaZ (commutative indeterminate)") {
    CHECK(is_naz(neg_inf + pos_inf));
  }
}

TEST_CASE("SignedCardinality — unary negation flips signs and infinities",
          "[sets][cardinality][signed][saturation]") {
  const auto pos_inf = SignedCardinality{PositiveInfinity{}};
  const auto neg_inf = SignedCardinality{NegativeInfinity{}};
  const auto naz = SignedCardinality{NaZ{}};

  CHECK(is_neg_inf(-pos_inf));
  CHECK(is_pos_inf(-neg_inf));
  CHECK(is_naz(-naz));  // NaZ propagates

  const auto five = finite_signed_cardinality(5);
  const auto neg_five = -five;
  REQUIRE(std::holds_alternative<SEC>(neg_five));
  CHECK(std::get<SEC>(neg_five) == SEC{-5});
}

TEST_CASE("SignedCardinality — subtraction reduces to add(-rhs)",
          "[sets][cardinality][signed][saturation]") {
  const auto pos_inf = SignedCardinality{PositiveInfinity{}};
  const auto five = finite_signed_cardinality(5);
  const auto seven = finite_signed_cardinality(7);

  SECTION("finite - finite") {
    const auto diff = five - seven;
    REQUIRE(std::holds_alternative<SEC>(diff));
    CHECK(std::get<SEC>(diff) == SEC{-2});
  }
  SECTION("(+ℵ_0) - finite = +ℵ_0") { CHECK(is_pos_inf(pos_inf - five)); }
  SECTION("(+ℵ_0) - (+ℵ_0) = NaZ") { CHECK(is_naz(pos_inf - pos_inf)); }
}

TEST_CASE("SignedCardinality — multiplication and saturation",
          "[sets][cardinality][signed][saturation]") {
  const auto pos_inf = SignedCardinality{PositiveInfinity{}};
  const auto neg_inf = SignedCardinality{NegativeInfinity{}};
  const auto zero = finite_signed_cardinality(0);
  const auto two = finite_signed_cardinality(2);
  const auto neg_two = finite_signed_cardinality(-2);

  SECTION("finite * finite stays finite") {
    const auto p = two * neg_two;
    REQUIRE(std::holds_alternative<SEC>(p));
    CHECK(std::get<SEC>(p) == SEC{-4});
  }
  SECTION("(+ℵ_0) * positive = +ℵ_0") { CHECK(is_pos_inf(pos_inf * two)); }
  SECTION("(+ℵ_0) * negative = -ℵ_0") { CHECK(is_neg_inf(pos_inf * neg_two)); }
  SECTION("(-ℵ_0) * negative = +ℵ_0") { CHECK(is_pos_inf(neg_inf * neg_two)); }
  SECTION("(+ℵ_0) * (+ℵ_0) = +ℵ_0") { CHECK(is_pos_inf(pos_inf * pos_inf)); }
  SECTION("(+ℵ_0) * (-ℵ_0) = -ℵ_0") { CHECK(is_neg_inf(pos_inf * neg_inf)); }
  SECTION("0 * (+ℵ_0) = NaZ (indeterminate)") {
    CHECK(is_naz(zero * pos_inf));
    CHECK(is_naz(pos_inf * zero));
  }

  SECTION("Magnitude overflow saturates per result sign") {
    const auto h_pos = huge_positive();
    const auto p = h_pos * two;
    CHECK(is_pos_inf(p));

    const auto h_neg = huge_negative();
    const auto n = h_neg * two;
    CHECK(is_neg_inf(n));
  }
}

TEST_CASE("SignedCardinality — division semantics",
          "[sets][cardinality][signed][saturation]") {
  const auto pos_inf = SignedCardinality{PositiveInfinity{}};
  const auto neg_inf = SignedCardinality{NegativeInfinity{}};
  const auto zero = finite_signed_cardinality(0);
  const auto two = finite_signed_cardinality(2);
  const auto neg_two = finite_signed_cardinality(-2);
  const auto seven = finite_signed_cardinality(7);

  SECTION("finite / finite truncates toward zero") {
    const auto q = seven / two;
    REQUIRE(std::holds_alternative<SEC>(q));
    CHECK(std::get<SEC>(q) == SEC{3});
  }
  SECTION("Division by zero returns NaZ") { CHECK(is_naz(seven / zero)); }
  SECTION("(±ℵ_0) / (±ℵ_0) returns NaZ (indeterminate)") {
    CHECK(is_naz(pos_inf / pos_inf));
    CHECK(is_naz(pos_inf / neg_inf));
    CHECK(is_naz(neg_inf / pos_inf));
    CHECK(is_naz(neg_inf / neg_inf));
  }
  SECTION("(±ℵ_0) / finite saturates per sign") {
    CHECK(is_pos_inf(pos_inf / two));
    CHECK(is_neg_inf(pos_inf / neg_two));
    CHECK(is_neg_inf(neg_inf / two));
    CHECK(is_pos_inf(neg_inf / neg_two));
  }
  SECTION("finite / (±ℵ_0) returns 0 (the limit)") {
    const auto q = seven / pos_inf;
    REQUIRE(std::holds_alternative<SEC>(q));
    CHECK(std::get<SEC>(q) == SEC{0});
  }
}

TEST_CASE("SignedCardinality — modulo",
          "[sets][cardinality][signed][saturation]") {
  const auto seven = finite_signed_cardinality(7);
  const auto two = finite_signed_cardinality(2);
  const auto pos_inf = SignedCardinality{PositiveInfinity{}};
  const auto zero = finite_signed_cardinality(0);

  SECTION("finite % finite mirrors C++ semantics") {
    const auto r = seven % two;
    REQUIRE(std::holds_alternative<SEC>(r));
    CHECK(std::get<SEC>(r) == SEC{1});
  }
  SECTION("Non-finite operand gives NaZ") {
    CHECK(is_naz(seven % pos_inf));
    CHECK(is_naz(pos_inf % seven));
  }
  SECTION("Modulo by zero gives NaZ") { CHECK(is_naz(seven % zero)); }
}

TEST_CASE("SignedCardinality — NaZ propagates through every op",
          "[sets][cardinality][signed][saturation][naz]") {
  const auto naz = SignedCardinality{NaZ{}};
  const auto five = finite_signed_cardinality(5);

  CHECK(is_naz(naz + five));
  CHECK(is_naz(five + naz));
  CHECK(is_naz(naz - five));
  CHECK(is_naz(naz * five));
  CHECK(is_naz(five * naz));
  CHECK(is_naz(naz / five));
  CHECK(is_naz(five / naz));
  CHECK(is_naz(naz % five));
  CHECK(is_naz(five % naz));
}

TEST_CASE("SignedCardinality — partial-ordering with unordered NaZ",
          "[sets][cardinality][signed][saturation][order]") {
  const auto pos_inf = SignedCardinality{PositiveInfinity{}};
  const auto neg_inf = SignedCardinality{NegativeInfinity{}};
  const auto naz = SignedCardinality{NaZ{}};
  const auto five = finite_signed_cardinality(5);
  const auto seven = finite_signed_cardinality(7);

  SECTION("-ℵ_0 < finite < +ℵ_0") {
    CHECK(compare_signed(neg_inf, five) == std::partial_ordering::less);
    CHECK(compare_signed(five, pos_inf) == std::partial_ordering::less);
    CHECK(compare_signed(neg_inf, pos_inf) == std::partial_ordering::less);
  }
  SECTION("Infinity equivalence with itself") {
    CHECK(compare_signed(pos_inf, pos_inf) ==
          std::partial_ordering::equivalent);
    CHECK(compare_signed(neg_inf, neg_inf) ==
          std::partial_ordering::equivalent);
  }
  SECTION("Finite vs finite delegates to underlying SEC") {
    CHECK(compare_signed(five, seven) == std::partial_ordering::less);
    CHECK(compare_signed(seven, five) == std::partial_ordering::greater);
    CHECK(compare_signed(five, five) == std::partial_ordering::equivalent);
  }
  SECTION("NaZ is unordered with everything (including itself)") {
    CHECK(compare_signed(naz, naz) == std::partial_ordering::unordered);
    CHECK(compare_signed(naz, five) == std::partial_ordering::unordered);
    CHECK(compare_signed(five, naz) == std::partial_ordering::unordered);
    CHECK(compare_signed(naz, pos_inf) == std::partial_ordering::unordered);
  }
}

TEST_CASE(
    "SignedCardinality — INT_MAX round-trip through (-2)*(-2)/4 = identity",
    "[sets][cardinality][signed][saturation][roundtrip]") {
  // INT_MAX = 2^31 - 1.  Multiplying by -2 gives -(2^32 - 2) (magnitude
  // ~4.3 billion); multiplying again gives 4*(2^31 - 1) = 2^33 - 4
  // (~8.6 billion).  Both fit comfortably in a single std::size_t limb
  // (max ~1.8e19), so no escalation kicks in --- the operations stay on
  // the finite fragment and the round-trip recovers the original.
  const auto original = finite_signed_cardinality(INT_MAX);
  const auto neg_two = finite_signed_cardinality(-2);
  const auto four = finite_signed_cardinality(4);

  const auto step1 = original * neg_two;
  const auto step2 = step1 * neg_two;
  const auto roundtrip = step2 / four;

  // All intermediates stayed on the finite fragment.
  REQUIRE(std::holds_alternative<SEC>(step1));
  REQUIRE(std::holds_alternative<SEC>(step2));
  REQUIRE(std::holds_alternative<SEC>(roundtrip));

  // Intermediate values, computed by hand:
  //   step1 = INT_MAX * -2 = -(2^32 - 2) = -4294967294
  //   step2 = step1 * -2  =  4 * INT_MAX = 2^33 - 4 = 8589934588
  // Both magnitudes exceed UINT32_MAX (~4.29e9 vs 8.59e9 for step2),
  // confirming the value-level check exercises a regime above 32-bit
  // integer arithmetic but still inside the single-limb 64-bit
  // capacity.
  const auto expected_step1 =
      finite_signed_cardinality(-4294967294LL);  // = -2 * INT_MAX
  const auto expected_step2 =
      finite_signed_cardinality(8589934588LL);  // = 4 * INT_MAX
  CHECK(step1 == expected_step1);
  CHECK(step2 == expected_step2);

  // Sign tracking: × (-2) flips sign; × (-2) again restores positive;
  // / 4 keeps the sign.
  CHECK(std::get<SEC>(step1).negative);
  CHECK_FALSE(std::get<SEC>(step2).negative);
  CHECK_FALSE(std::get<SEC>(roundtrip).negative);

  // The round-trip equals the original.
  CHECK(roundtrip == original);
}

TEST_CASE(
    "SignedCardinality — operator< returns false for NaZ comparisons "
    "(IEEE-NaN style)",
    "[sets][cardinality][signed][saturation][order]") {
  const auto naz = SignedCardinality{NaZ{}};
  const auto five = finite_signed_cardinality(5);
  const auto seven = finite_signed_cardinality(7);

  CHECK_FALSE(naz < five);
  CHECK_FALSE(five < naz);
  CHECK_FALSE(naz < naz);

  CHECK(five < seven);
  CHECK_FALSE(seven < five);
}
