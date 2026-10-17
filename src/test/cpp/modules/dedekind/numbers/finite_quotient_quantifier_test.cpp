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
 *   - 𝔹 = 𝔸<bool>       finite carrier, materialised over {false, true};
 *   - ℕ = 𝔸<Cardinality> infinite, but π % 3 == 0 binds to a residue class,
 *     whose == Ø and == 𝔸 are decided from the modulus.
 */
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

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

  SECTION("𝔹 = 𝔸<bool>: finite carrier, materialised over {false, true}") {
    // Point-free membership fragment: π == true, materialised via s | p.
    CHECK(exists(𝔹,
                 π == true));  // (A) {b | b} ≠ ∅ — true a member
    CHECK_FALSE(forall(𝔹,
                       π == true));  // (B) ≠ S — false a counterexample
  }

  SECTION("ℕ = 𝔸<Cardinality>: infinite carrier, factored through ℤ/3ℤ") {
    // The congruence π % 3 == 0 binds to the residue class of 0 mod 3.
    CHECK(exists(𝔸<Cardinality>{},
                 π % 3 == 0));  // (A) ∃ x ≡ 0 (residue 0 vs ∅)
    CHECK_FALSE(forall(𝔸<Cardinality>{},
                       π % 3 == 0));  // (B) residues 1,2 vs S
    CHECK_THROWS_AS(𝔸<Cardinality>{} & (π % -3 == -1), std::domain_error);
  }
}
