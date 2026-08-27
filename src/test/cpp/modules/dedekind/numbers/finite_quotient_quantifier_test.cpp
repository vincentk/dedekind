/** @file test/cpp/modules/dedekind/numbers/finite_quotient_quantifier_test.cpp
 *
 * Runtime coverage for the finite-quotient quantifiers (the §3.1 exhibit,
 * lst:quantifiers_def).  The load-bearing facts are checked-in static_asserts
 * (invisible to Codecov) in quantifier.cppm and natural.cppm; this pairs them
 * with runtime CHECKs.
 *
 * A quantifier IS a comparison of the comprehension against a lattice bound
 * (Eqn 2): exists tests against ∅ (scheme A), forall against Ω (scheme B).
 * On a finite quotient both bounds are decided BY TYPE:
 *   - 𝔹 = Ω<bool>       finite carrier, materialised over {false, true};
 *   - ℕ = Ω<Cardinality> infinite, but isEven factors through ℤ/2ℤ =
 * Modular<2>, so ∃/∀ exhaust the two residues in finite time.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.morphologies;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::morphologies;
using namespace dedekind::order;

TEST_CASE("finite-quotient quantifiers decide exists/forall by type on 𝔹 and ℕ",
          "[numbers][quantifier][finite-quotient]") {
  SECTION("Congruence<N,R> factors through the finite quotient Modular<N>") {
    CHECK(Congruence<2, 0>{}(4u));        // 4 ≡ 0 (mod 2): even
    CHECK_FALSE(Congruence<2, 0>{}(3u));  // 3 ≢ 0 (mod 2): odd
  }

  SECTION("𝔹 = Ω<bool>: finite carrier, materialised over {false, true}") {
    auto isTrue = [](bool b) { return b; };
    CHECK(exists(Ω<bool>, isTrue));  // (A) {b | b} ≠ ∅ — true is a member
    CHECK_FALSE(forall(Ω<bool>,
                       isTrue));  // (B) {b | b} ≠ Ω — false is a counterexample
  }

  SECTION("ℕ = Ω<Cardinality>: infinite carrier, factored through ℤ/2ℤ") {
    // The telling scout sugar: in<ℕ> % Modular<2> == bound<0>  →
    // Congruence<2,0>.
    constexpr auto isEven = in<Ω<Cardinality>> % Modular<2>{} == bound<0>;
    CHECK(
        exists(Ω<Cardinality>, isEven));  // (A) ∃ even natural (residue 0 vs ∅)
    CHECK_FALSE(
        forall(Ω<Cardinality>, isEven));  // (B) ¬∀ even (residue 1 vs Ω)
  }
}
