#include <catch2/catch_test_macros.hpp>
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
// makes the pair @c (embed_ℕ_ℤ, @c abs) a @b split @b mono / @b split
// @b epi rather than an isomorphism (see @c
// docs/design/carrier-lattice.md).

TEST_CASE("Integer: ℕ → ℤ → ℕ via unary − then abs (closes #436 round-trip)",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto n = finite_cardinality(7);
  const auto neg = -n;         // ℕ → ℤ (closure-forcing negation)
  const auto back = abs(neg);  // ℤ → ℕ (retraction)
  CHECK(back == n);
}

TEST_CASE("Integer: ℕ → ℤ → ℕ via binary − then abs",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto n = finite_cardinality(7);
  const auto z = finite_cardinality(0) - n;  // → -7 in ℤ
  const auto back = abs(z);                  // → 7 in ℕ
  CHECK(back == n);
}

TEST_CASE("Integer: ℤ → ℕ → ℤ folds sign — abs is not a bijection",
          "[numbers][integer][carrier-lattice][round-trip]") {
  const auto z_neg = finite_signed_cardinality(-5);
  const auto folded = abs(z_neg);                           // → 5 in ℕ
  const auto re_embedded = folded - finite_cardinality(0);  // → +5 in ℤ
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
