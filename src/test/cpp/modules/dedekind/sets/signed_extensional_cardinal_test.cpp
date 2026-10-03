/** @file dedekind/sets/signed_extensional_cardinal_test.cpp
 *
 * Tests for the fixed-width sign-magnitude integer carrier
 * SignedExtensionalCardinal<N>. Covers ring laws, canonical-zero
 * normalisation, signed edge cases around the minimum representable
 * value, and sign propagation through *, /, % per C++ semantics
 * (division on the one-limb carrier only).  Wider N carry + and *
 * across limbs; they are tested for that and nothing else.
 */

#include <catch2/catch_test_macros.hpp>
#include <climits>

import dedekind.sets;

using dedekind::sets::SignedExtensionalCardinal;

namespace {

using Z = SignedExtensionalCardinal<>;

}  // namespace

TEST_CASE("SignedExtensionalCardinal — canonical zero is unique",
          "[sets][cardinality][signed]") {
  constexpr Z pos_zero{0};
  constexpr Z neg_zero = -Z{0};

  STATIC_CHECK(pos_zero == neg_zero);
  STATIC_CHECK(!pos_zero.negative);
  STATIC_CHECK(!neg_zero.negative);  // canonical: negation of zero stays +0
}

TEST_CASE("SignedExtensionalCardinal — construction and equality",
          "[sets][cardinality][signed]") {
  constexpr Z three{3};
  constexpr Z minus_three{-3};
  constexpr Z three_again{3};

  STATIC_CHECK(three == three_again);
  STATIC_CHECK(three != minus_three);
  STATIC_CHECK(three.negative == false);
  STATIC_CHECK(minus_three.negative == true);
}

TEST_CASE("SignedExtensionalCardinal — ordering respects sign",
          "[sets][cardinality][signed]") {
  constexpr Z minus_five{-5};
  constexpr Z minus_three{-3};
  constexpr Z zero{0};
  constexpr Z two{2};
  constexpr Z five{5};

  STATIC_CHECK(minus_five <
               minus_three);  // both negative: larger |.| is smaller
  STATIC_CHECK(minus_three < zero);
  STATIC_CHECK(zero < two);
  STATIC_CHECK(two < five);
  STATIC_CHECK(minus_five < five);
}

TEST_CASE("SignedExtensionalCardinal — unary minus",
          "[sets][cardinality][signed]") {
  constexpr Z three{3};
  constexpr Z zero{0};

  STATIC_CHECK(-three == Z{-3});
  STATIC_CHECK(-(-three) == three);
  STATIC_CHECK(-zero == zero);
}

TEST_CASE("SignedExtensionalCardinal — addition",
          "[sets][cardinality][signed]") {
  constexpr Z two{2};
  constexpr Z three{3};
  constexpr Z minus_two{-2};
  constexpr Z minus_three{-3};

  // Same sign: magnitudes add.
  STATIC_CHECK(two + three == Z{5});
  STATIC_CHECK(minus_two + minus_three == Z{-5});

  // Opposite signs: larger wins, magnitude is the difference.
  STATIC_CHECK(three + minus_two == Z{1});
  STATIC_CHECK(two + minus_three == Z{-1});

  // Cancellation to zero is canonical.
  STATIC_CHECK(three + minus_three == Z{0});
  STATIC_CHECK(!(three + minus_three).negative);
}

TEST_CASE("SignedExtensionalCardinal — subtraction",
          "[sets][cardinality][signed]") {
  constexpr Z five{5};
  constexpr Z three{3};

  STATIC_CHECK(five - three == Z{2});
  STATIC_CHECK(three - five == Z{-2});
  STATIC_CHECK(five - five == Z{0});
  STATIC_CHECK(Z{0} - five == Z{-5});
}

TEST_CASE("SignedExtensionalCardinal — multiplication and sign rule",
          "[sets][cardinality][signed]") {
  constexpr Z two{2};
  constexpr Z three{3};
  constexpr Z minus_two{-2};
  constexpr Z minus_three{-3};

  STATIC_CHECK(two * three == Z{6});              // (+)(+) = (+)
  STATIC_CHECK(minus_two * three == Z{-6});       // (-)(+) = (-)
  STATIC_CHECK(two * minus_three == Z{-6});       // (+)(-) = (-)
  STATIC_CHECK(minus_two * minus_three == Z{6});  // (-)(-) = (+)

  // Zero absorbs sign.
  STATIC_CHECK(minus_three * Z{0} == Z{0});
  STATIC_CHECK(!(minus_three * Z{0}).negative);
}

TEST_CASE("SignedExtensionalCardinal — division and modulo",
          "[sets][cardinality][signed]") {
  constexpr Z seven{7};
  constexpr Z three{3};
  constexpr Z minus_seven{-7};
  constexpr Z minus_three{-3};

  // C++ truncation toward zero.
  STATIC_CHECK(seven / three == Z{2});
  STATIC_CHECK(minus_seven / three == Z{-2});
  STATIC_CHECK(seven / minus_three == Z{-2});
  STATIC_CHECK(minus_seven / minus_three == Z{2});

  // Modulo takes the sign of the dividend.
  STATIC_CHECK(seven % three == Z{1});
  STATIC_CHECK(minus_seven % three == Z{-1});
  STATIC_CHECK(seven % minus_three == Z{1});
  STATIC_CHECK(minus_seven % minus_three == Z{-1});
}

TEST_CASE("SignedExtensionalCardinal — ring laws on small values",
          "[sets][cardinality][signed][ring]") {
  constexpr Z a{7};
  constexpr Z b{-4};
  constexpr Z c{3};

  // Additive identity.
  STATIC_CHECK(a + Z{0} == a);
  // Additive inverse.
  STATIC_CHECK(a + (-a) == Z{0});
  // Associativity of +.
  STATIC_CHECK((a + b) + c == a + (b + c));
  // Commutativity of +.
  STATIC_CHECK(a + b == b + a);
  // Multiplicative identity.
  STATIC_CHECK(a * Z{1} == a);
  STATIC_CHECK(a * Z{-1} == -a);
  // Distributivity.
  STATIC_CHECK(a * (b + c) == a * b + a * c);
  // Associativity of *.
  STATIC_CHECK((a * b) * c == a * (b * c));
  // Commutativity of *.
  STATIC_CHECK(a * b == b * a);
}

TEST_CASE("SignedExtensionalCardinal — no overflow near signed-int-min",
          "[sets][cardinality][signed][overflow-safety]") {
  // Construction from INT_MIN must not overflow during negation; the
  // magnitude is 2^31 exactly, representable in the unsigned limb.
  constexpr Z min_int{INT_MIN};
  STATIC_CHECK(min_int.negative);
  STATIC_CHECK(min_int.magnitude !=
               SignedExtensionalCardinal<>::magnitude_type{});

  // -(INT_MIN) in the C++ type would be UB; here it is a well-defined
  // unsigned magnitude equal to 2^31.
  constexpr Z neg_min = -min_int;
  STATIC_CHECK(!neg_min.negative);
  STATIC_CHECK(neg_min.magnitude == min_int.magnitude);
}

TEST_CASE(
    "SignedExtensionalCardinal<2> — signed multiplication carries "
    "into the high limb",
    "[sets][cardinality][signed][multi-limb]") {
  // The 2-limb carrier (128-bit magnitude): the ×(-2) chain pushes the
  // magnitude into the high limb with the right sign.
  using Z2 = SignedExtensionalCardinal<2>;

  // original = 2^63 (single-limb magnitude).
  constexpr Z2 original = []() constexpr {
    Z2 z;
    z.negative = false;
    z.magnitude.limbs[0] = static_cast<Z2::magnitude_type::limb_type>(1ULL)
                           << 63;
    z.magnitude.limbs[1] = 0;
    return z;
  }();

  constexpr Z2 neg_two{-2};

  // step1 = original × (-2) = -2^64.  Magnitude = 2^64 carries into
  // limbs[1] (limbs[0]=0, limbs[1]=1) --- two-limb territory.
  constexpr Z2 step1 = original * neg_two;
  STATIC_CHECK(step1.negative);
  STATIC_CHECK(step1.magnitude.limbs[0] == 0);
  STATIC_CHECK(step1.magnitude.limbs[1] == 1);

  // step2 = step1 × (-2) = 2^65.  Magnitude (limbs[0]=0, limbs[1]=2).
  constexpr Z2 step2 = step1 * neg_two;
  STATIC_CHECK(!step2.negative);
  STATIC_CHECK(step2.magnitude.limbs[0] == 0);
  STATIC_CHECK(step2.magnitude.limbs[1] == 2);
}

TEST_CASE(
    "SignedExtensionalCardinal<2> — limb growth then shrinkage "
    "via repeated additions",
    "[sets][cardinality][signed][multi-limb][roundtrip]") {
  // User-requested round-trip:
  //   (a) start at 0;
  //   (b) keep adding the increment until the magnitude requires two
  //       limbs (limbs[1] != 0);
  //   (c) multiply by -1 (unary negation);
  //   (d) keep adding the same increment until the value reaches 0;
  //   (e) check that the magnitude is back to a single limb
  //       (limbs[1] == 0).
  //
  // Note on the increment: the user's pseudocode used +13 each step.
  // At a 64-bit `std::size_t` limb the natural-number of +13 steps
  // required to cross 2^64 is ceil(2^64 / 13) ≈ 1.4e18 --- intractable
  // as a unit test.  We scale the increment to `13 * 2^56` (= 0x0D
  // followed by 56 low-zero bits, a small odd-multiple of a power of
  // two) so the loop crosses the boundary in ~20 iterations.  The
  // *structural* invariant (limb-count grows past 1 in step (b),
  // collapses back to 1 in step (e)) is preserved; only the
  // increment magnitude is scaled to make the loop tractable.
  using Z2 = SignedExtensionalCardinal<2>;

  constexpr Z2 zero;
  // 13 * 2^56 fits comfortably in a single std::size_t limb
  // (~9.36e17, well under 2^64 ≈ 1.84e19) --- single-step addition
  // never overflows a single limb on its own.
  Z2 increment;
  increment.magnitude.limbs[0] =
      static_cast<Z2::magnitude_type::limb_type>(13ULL) << 56;

  // (a) start at 0.
  Z2 value = zero;
  REQUIRE(value == zero);
  REQUIRE(value.magnitude.limbs[0] == 0);
  REQUIRE(value.magnitude.limbs[1] == 0);

  // (b) keep adding the increment until magnitude requires two limbs.
  std::size_t step_count = 0;
  while (value.magnitude.limbs[1] == 0) {
    value = value + increment;
    ++step_count;
  }
  // The boundary crossing happens at the smallest k such that
  // k * (13 * 2^56) > 2^64, i.e.\ k = ceil(2^64 / (13 * 2^56))
  // = ceil(2^8 / 13) = ceil(256 / 13) = 20.
  CHECK(step_count == 20);
  CHECK(value.magnitude.limbs[1] != 0);
  CHECK_FALSE(value.negative);

  // (c) multiply by -1 (unary negation).
  value = -value;
  CHECK(value.negative);
  CHECK(value.magnitude.limbs[1] != 0);  // magnitude preserved

  // (d) keep adding the increment until the value reaches 0 again.
  // Each +increment moves the value upward; from a negative
  // 20-increments-deep starting point, exactly 20 additions return
  // us to canonical zero (sign-magnitude addition cancels through
  // the sign-flip on the last step).
  std::size_t shrink_steps = 0;
  while (value != zero) {
    value = value + increment;
    ++shrink_steps;
  }
  CHECK(shrink_steps == 20);

  // (e) check that the magnitude is now a single limb.
  CHECK(value.magnitude.limbs[1] == 0);
  CHECK(value.magnitude.limbs[0] == 0);  // == 0 exactly
  CHECK_FALSE(value.negative);           // canonical +0
  CHECK(value == zero);
}
