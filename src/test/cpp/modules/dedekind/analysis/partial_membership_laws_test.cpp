/** @file test/cpp/modules/dedekind/analysis/partial_membership_laws_test.cpp
 *
 * Source for the §3 Listing on PARTIAL membership (lst:membership), the
 * warm-up that precedes the decidable set algebra (lst:set-grammar).
 *
 * The primitive of the set DSL is membership, x ∈ A = χ_A(x), valued in the
 * FULL classifier Ω.  For the honest, three-valued species Ω = Ternary =
 * {True, False, Unknown}, so membership may answer Unknown.  Three facts anchor
 * the listing:
 *
 *   1. Codomain axis.  The extra point of Ω is a fixpoint of negation: Kleene ¬
 *      is negation about zero, so fix(¬) = Unknown.  The decided fragment
 *      Σ = bool ↪ Ω (Boole, the track of lst:set-grammar) has none.
 *   2. Domain axis.  fix(succ) is decided by the CARRIER: on the finite
 *      fragment the successor has no fixpoint, but the transfinite saturation
 *      point ℵ₀ is one.  Whether a construction closes on itself (hence whether
 *      membership is decided) is a property of the domain, the
 *      Galois-specialisation axis.
 *   3. Membership genuinely lands in Ω\Σ: the intensional image of a set
 *      RETURNS Unknown outright (pinned by showcase_10).
 *
 * The archetype hard predicate is Mandelbrot membership (numbers/mandelbrot),
 * semi-decidable: escape is recognisable, non-escape is not.  Its Unknown is
 * exercised at runtime (mandelbrot_stress_test); the compile-time Unknown is
 * the intensional image, whose indecision is a static observable.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("partial membership: fix(¬)=Unknown",
          "[sets][computability][membership]") {
  // ── Ω = {⊤, ⊥, U}: the extra point is the fixpoint of negation ──
  {
    // Kleene ¬ fixes Unknown (negation about zero); the decided fragment
    // Σ = {⊤, ⊥} has none; on Σ, ¬ swaps the two values.
    STATIC_CHECK(!Ternary::Unknown == Ternary::Unknown);  // fix(¬) = U
    STATIC_CHECK(!Ternary::True == Ternary::False);       // no fixpoint on Σ
    STATIC_CHECK(!Ternary::False == Ternary::True);
  }
}
