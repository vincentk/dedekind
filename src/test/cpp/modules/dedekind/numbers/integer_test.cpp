#include <catch2/catch_test_macros.hpp>
#include <cstdlib>  // For std::abs in the partial-commuting-square test.
#include <limits>  // For std::numeric_limits in the abs honest-extension tests.
#include <variant>  // For std::variant's operator== reached via ADL on
                    // SignedCardinality / Cardinality (aliases of
                    // std::variant<...>).  Without this include the
                    // CHECK assertions below fail to find the comparison
                    // operator — see PR #437 review thread.

import dedekind.category;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

// ===========================================================================
// Carrier-lattice round-trip: ℕ → ℤ → ℕ
// ===========================================================================
//
// Two structurally-distinct arrows take ℕ into ℤ at the variant level:
//   * unary @c -Cardinality → SignedCardinality (closure-forcing
//     negation; PR #433)
//   * binary @c Cardinality - Cardinality → SignedCardinality
//     (closure-forcing subtraction; PR #433)
// and one arrow brings ℤ back to ℕ:
//   * @c abs(SignedCardinality) → Cardinality (retraction; PR #437,
//     #436)
// The round-trip property @c abs ∘ embed = id_ℕ on the non-negative
// fragment — which is the @b retraction defining clause — is what's
// exercised below.  ℤ → ℕ → ℤ is @b not bijective: @c abs folds
// sign, so re-embedding loses the sign bit.  The asymmetry is what
// makes the pair @c (embed_uint_sint_, @c abs) a @b split @b mono / @b split
// @b epi rather than an isomorphism (see @c
// docs/design/carrier-lattice.md).

TEST_CASE("Integer: ℕ → ℤ → ℕ via unary − then abs (closes #436 round-trip)",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto n = finite_cardinality(7);
  const auto neg = -n;  // Cardinality → SignedExtensionalCardinal<>
                        // (closure-forcing negation)
  const auto back =
      abs(neg);  // SignedExtensionalCardinal<> → Cardinality (retraction)
  CHECK(back == n);
}

TEST_CASE("Integer: ℕ → ℤ → ℕ via binary − then abs",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto n = finite_cardinality(7);
  const auto z =
      finite_cardinality(0) - n;  // → -7 in SignedExtensionalCardinal<>
  const auto back = abs(z);       // → 7 in Cardinality
  CHECK(back == n);
}

TEST_CASE("Integer: ℤ → ℕ → ℤ folds sign — abs is not a bijection",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto z_neg = finite_signed_cardinality(-5);
  const auto folded = abs(z_neg);  // → 5 in Cardinality
  const auto re_embedded =
      folded - finite_cardinality(0);  // → +5 in SignedExtensionalCardinal<>
  CHECK(re_embedded == finite_signed_cardinality(5));
  CHECK_FALSE(re_embedded == z_neg);  // sign was lost on the round-trip
}

TEST_CASE("Integer: ℕ → ℤ → ℕ on canonical zero — 0 ↦ 0 ↦ 0",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto zero = finite_cardinality(0);
  CHECK(abs(-zero) == zero);
  CHECK(abs(zero - zero) == zero);
}

TEST_CASE("Integer: ℕ → ℤ → ℕ on ℵ_0 — saturating round-trip",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto inf = Cardinality{ℵ_0{}};
  CHECK(abs(-inf) == inf);
}

// ===========================================================================
// Honest extension of std::abs: our @c abs is total where @c std::abs(int)
// is undefined.  At @c INT_MIN, @c std::abs(INT_MIN) is UB (since
// @c -INT_MIN overflows the signed range), but the variant carrier
// @c SignedCardinality has no such pathology — the magnitude of @c INT_MIN
// is representable as a non-negative @c Cardinality.  This test exercises
// the case @c std::abs cannot.
// ===========================================================================

TEST_CASE(
    "Integer: abs is the honest total extension of std::abs — well-defined "
    "at INT_MIN where std::abs is UB",
    "[numbers][integer][carrier-lattice][abs][honest-extension]") {
  // The pathological value: INT_MIN.  std::abs(INT_MIN) is undefined
  // behaviour because -INT_MIN overflows the signed-int range.  Our
  // @c abs lifts INT_MIN through the variant embedding and returns a
  // finite @c Cardinality with the magnitude — total, no UB, type-
  // level non-negativity guarantee.
  //
  // Portability: compute the expected magnitude via unsigned modular
  // arithmetic so the test does not hard-code 32-bit two's-complement
  // ABI.  @c 0u @c - @c static_cast<unsigned>(int_min) is well-defined
  // for any width.
  constexpr int int_min = std::numeric_limits<int>::min();
  constexpr unsigned int_min_magnitude = 0u - static_cast<unsigned>(int_min);
  const auto z_min = embed_sint_ℤ_(int_min);
  const auto magnitude = abs(z_min);

  // Magnitude IS representable as a non-negative Cardinality (the
  // saturating ℕ-proxy carrier).  No UB, no truncation; the answer is
  // the concrete finite value @c |INT_MIN|.
  CHECK(magnitude == finite_cardinality(int_min_magnitude));
  // The answer is NOT ℵ_0 — INT_MIN is well-finite, just not negatable
  // in the int range.
  CHECK_FALSE(magnitude == Cardinality{ℵ_0{}});

  // Cross-check: at INT_MAX (where std::abs IS defined), the two
  // operations agree on the magnitude — the partial commuting square
  // holds on the non-INT_MIN fragment.
  constexpr int int_max = std::numeric_limits<int>::max();
  const auto z_max = embed_sint_ℤ_(int_max);
  CHECK(abs(z_max) == finite_cardinality(static_cast<std::size_t>(int_max)));
}

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

  // Three-way bridge: abs_int_unsigned(v) at the machine layer agrees
  // with the variant-layer abs lifted through embed_sint_ℤ_,
  // when the latter's Cardinality result is realised back to a finite
  // value.  Holds on the safe fragment of int.
  for (const int v : {0, 1, -1, 42, -42, 1000, -1000}) {
    const auto via_machine = abs_int_unsigned(v);
    const auto via_variant = abs(embed_sint_ℤ_(v));
    CHECK(via_variant == finite_cardinality(via_machine));
  }
  // At INT_MIN: machine-layer abs_int_unsigned and variant-layer abs
  // both yield |INT_MIN|, even though std::abs(INT_MIN) is UB.  The
  // two honest extensions agree where the UB-bearing original refuses
  // to commit.
  CHECK(abs_int_unsigned(int_min) == int_min_magnitude);
  CHECK(abs(embed_sint_ℤ_(int_min)) == finite_cardinality(int_min_magnitude));
  // The exported arrow form abs_ (from :cardinality) wraps the same
  // operation and exhibits the same totality at INT_MIN.  Covers the
  // arrow's lambda body for codecov.
  CHECK(abs_(embed_sint_ℤ_(int_min)) == finite_cardinality(int_min_magnitude));
  CHECK(abs_(embed_sint_ℤ_(-3)) == finite_cardinality(3));
  CHECK(abs_(embed_sint_ℤ_(0)) == finite_cardinality(0));
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
