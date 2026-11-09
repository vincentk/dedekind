#include <catch2/catch_test_macros.hpp>
#include <cstdlib>  // For std::abs in the partial-commuting-square test.
#include <limits>  // For std::numeric_limits in the abs honest-extension tests.
#include <variant>  // For std::variant's operator== reached via ADL on
// SignedCardinality (an alias of std::variant<...>).  Without this include the
// CHECK assertions below fail to find the comparison
// operator — see PR #437 review thread.

import dedekind.category;
import dedekind.algebra; // AdditiveInverse + IsOrderedAdditiveGroup (#959)
import dedekind.morphologies;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::algebra;
using namespace dedekind::morphologies;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE(
    "Integer: abs_int_unsigned is the type-honest machine-layer absolute "
    "value — int → unsigned, total at INT_MIN where std::abs is UB",
    "[numbers][integer][carrier-lattice][abs][honest-extension]") {
  // abs_int_unsigned : int → unsigned is the type-honest sibling of
  // std::abs : int → int.  The unsigned codomain announces
  // non-negativity at the type level; the implementation is total
  // (works for INT_MIN where std::abs is UB).
  constexpr int int_min = std::numeric_limits<int>::min();
  // |INT_MIN| computed via unsigned modular arithmetic — well-defined
  // for any int width / two's-complement ABI.
  constexpr unsigned int_min_magnitude = 0u - static_cast<unsigned>(int_min);
  // abs_int_unsigned(INT_MIN) = |INT_MIN| — total, no UB.
  CHECK(abs_int_unsigned(int_min) == int_min_magnitude);
  // Sign-folding: distinct +n / -n map to the same unsigned magnitude
  // (so the function is NOT injective).
  CHECK(abs_int_unsigned(3) == 3u);
  CHECK(abs_int_unsigned(-3) == 3u);
  CHECK(abs_int_unsigned(0) == 0u);

  // Partial commuting square: on the non-INT_MIN fragment of int, the
  // type-honest abs_int_unsigned agrees with std::abs coerced to unsigned.
  // (At INT_MIN the std::abs side would be UB, so the comparison is
  // omitted there — the previous CHECK already pinned that case.)
  for (const int v :
       {1, -1, 42, -42, 1000, -1000, std::numeric_limits<int>::max()}) {
    CHECK(abs_int_unsigned(v) == static_cast<unsigned>(std::abs(v)));
  }
}

// ===========================================================================
// (3.5) embed_𝔹_ℕ_ — runtime witness for the variant-layer canonical
//       mono 𝔹 ↪ ℕ landing on the @c Numeral carrier.  The
//       compile-time witnesses live in @c :natural; this exercises
//       the same equalities at runtime so the arrow's lambda body
//       shows up as covered for codecov.
// ===========================================================================

TEST_CASE(
    "carrier-lattice: embed_𝔹_ℕ_ maps true to 1 and false to 0 (𝔹 ↪ ℕ via "
    "the Numeral carrier)",
    "[carrier-lattice][boolean][cardinality][embed]") {
  CHECK(embed_𝔹_ℕ_(true) == Numeral{1u});
  CHECK(embed_𝔹_ℕ_(false) == Numeral{0u});
}

TEST_CASE(
    "carrier-lattice: embed_𝔹_ℕ (set-level lift) on Singleton<true> lands at "
    "Singleton<1> in ℕ",
    "[carrier-lattice][boolean][cardinality][embed][image]") {
  // Set-level lift exercises the runtime dispatch through
  // dedekind::sets::image(arrow, Singleton) — covers the
  // forwarding-reference body for codecov.
  constexpr Singleton<bool, Boole> s_true{true};
  const auto image_set = embed_𝔹_ℕ(s_true);
  CHECK(origin(image_set) == Numeral{1u});

  constexpr Singleton<bool, Boole> s_false{false};
  const auto image_set_false = embed_𝔹_ℕ(s_false);
  CHECK(origin(image_set_false) == Numeral{0u});
}

// ===========================================================================
// (4) Embedding chain: 𝕂3 → sint → ℤ (signed counterpart to 𝔹 → uint → ℕ)
// ===========================================================================

TEST_CASE("sint: 𝕂3 → sint → ℤ chain via embed_𝕂3_ℤ_ + lift",
          "[numbers][sint][lift][embedding-chain]") {
  // Post-PR #439, embed_𝕂3_ℤ_ already lands on SignedCardinality
  // directly.  The textbook-factored chain
  //
  //   𝕂3 → std::signed_integral → ℤ
  //
  // would route through @c int (the machine layer) on the first leg
  // and then through @c embed_sint_ℤ on the
  // second; PR #439's chosen factorisation collapses the two legs
  // into one for ergonomic reasons.  Exercise the chain claim
  // directly by relating @c embed_𝕂3_ℤ_'s images to the lifted
  // canonical machine-int images @c -1 / @c 0 / @c 1, asserting
  // that both factorisations agree on the canonical value of each
  // Kleene truth-value.
  CHECK(embed_𝕂3_ℤ_(Ternary::False) == embed_sint_ℤ(-1));
  CHECK(embed_𝕂3_ℤ_(Ternary::Unknown) == embed_sint_ℤ(0));
  CHECK(embed_𝕂3_ℤ_(Ternary::True) == embed_sint_ℤ(1));
}

TEST_CASE("Integer: additive inverse -x is antitone on ℤ (#908/#959)",
          "[numbers][integer][algebra][antitone]") {
  // ℤ (SignedCardinality) is an ordered additive group, so the unary additive
  // inverse -x is ORDER-REVERSING: the algebraic dog-food of the antitone
  // reification, the same shape as the logic NegationArrow's negation on Omega.
  constexpr AdditiveInverse<SignedCardinality> neg{};
  const SignedCardinality two = finite_signed_cardinality(2);
  const SignedCardinality three = finite_signed_cardinality(3);

  // The inverse maps to the negatives.
  CHECK(neg(two) == finite_signed_cardinality(-2));
  CHECK(neg(three) == finite_signed_cardinality(-3));

  // Order-reversal: 2 <= 3, so -3 <= -2, i.e. neg(3) <= neg(2).
  CHECK(two <= three);
  CHECK(neg(three) <= neg(two));

  // The arrow is a certified antitone / variant morphism.
  STATIC_CHECK(IsAntiMonotone<AdditiveInverse<SignedCardinality>>);
  STATIC_CHECK(IsVariant<AdditiveInverse<SignedCardinality>>);

  // Self-adjoint antitone Galois connection: -x is an antitone involution
  // (-(-x) = x), so (AdditiveInverse, AdditiveInverse) is a Galois connection
  // and a <= -b iff b <= -a.  The first ANTITONE witness of IsGaloisConnection.
  STATIC_CHECK(IsGaloisConnection<AdditiveInverse<SignedCardinality>,
                                  AdditiveInverse<SignedCardinality>>);
  const SignedCardinality neg_five = finite_signed_cardinality(-5);
  // both-true side: (-5 <= -2) iff (2 <= 5).
  CHECK((neg_five <= neg(two)) == (two <= neg(neg_five)));
  // both-false side: (2 <= -3) iff (3 <= -2).
  CHECK((two <= neg(three)) == (three <= neg(two)));
}
