/** @file test/cpp/modules/dedekind/numbers/finite_quotient_quantifier_test.cpp
 *
 * Runtime coverage for the finite-quotient quantifiers (the §3.1 exhibit,
 * lst:quantifiers_def).  The load-bearing facts are checked-in static_asserts
 * (invisible to Codecov) in quantifier.cppm and natural.cppm; this pairs them
 * with runtime CHECKs.
 *
 * A quantifier IS a comparison of the comprehension against a lattice bound
 * (Eqn 2): exists tests against ∅ (scheme A), forall against the input set S
 * (scheme B).  On a finite quotient both bounds are decided BY TYPE:
 *   - 𝔹 = Ω<bool>       finite carrier, materialised over {false, true};
 *   - ℕ = Ω<Cardinality> infinite, but isDivisibleBy3 factors through ℤ/3ℤ =
 * Modular<3>, so ∃/∀ exhaust the three residues in finite time.
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
    CHECK(Congruence<3, 0>{}(6u));        // 6 ≡ 0 (mod 3): divisible by 3
    CHECK_FALSE(Congruence<3, 0>{}(4u));  // 4 ≢ 0 (mod 3)
  }

  SECTION("𝔹 = Ω<bool>: finite carrier, materialised over {false, true}") {
    // Point-free membership fragment: π == fix(true_c), materialised via s | p.
    CHECK(
        exists(Ω<bool>, π == fix(true_c)));  // (A) {b | b} ≠ ∅ — true a member
    CHECK_FALSE(
        forall(Ω<bool>, π == fix(true_c)));  // (B) ≠ S — false a counterexample
  }

  SECTION("ℕ = Ω<Cardinality>: infinite carrier, factored through ℤ/3ℤ") {
    // Point-free congruence fragment π % fix(3_c) == fix(0_c) (≡ 0 mod 3); s |
    // p materialises it over the three residues of ℤ/3ℤ.
    CHECK(exists(Ω<Cardinality>,
                 π % fix(3_c) == fix(0_c)));  // (A) ∃ x ≡ 0 (residue 0 vs ∅)
    CHECK_FALSE(forall(Ω<Cardinality>,
                       π % fix(3_c) == fix(0_c)));  // (B) residues 1,2 vs S
  }
}
