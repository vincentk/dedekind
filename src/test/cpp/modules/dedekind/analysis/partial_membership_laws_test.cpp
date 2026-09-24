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
 *   3. Membership genuinely lands in Ω\Σ: a comprehension over an uncountable
 *      carrier (ℝ, tagged ℶ₁) is CLASSIFIED undecidable (its logic species is
 *      Kleene, so == is withheld), and the intensional image of a set
 *      RETURNS Unknown outright.
 *
 * The archetype hard predicate is Mandelbrot membership (numbers/mandelbrot),
 * semi-decidable: escape is recognisable, non-escape is not.  Its Unknown is
 * exercised at runtime (mandelbrot_stress_test); the compile-time Unknown here
 * is the intensional image, whose indecision is a static observable.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::numbers;
using namespace dedekind::order;

TEST_CASE("partial membership: fix(¬)=Unknown and undecidable classification",
          "[sets][computability][membership]") {
  // ── Ω = {⊤, ⊥, U}: the extra point is the fixpoint of negation ──
  {
    // Kleene ¬ fixes Unknown (negation about zero); the decided fragment
    // Σ = {⊤, ⊥} has none; on Σ, ¬ swaps the two values.
    static_assert(!Ternary::Unknown == Ternary::Unknown);  // fix(¬) = U
    static_assert(!Ternary::True == Ternary::False);       // no fixpoint on Σ
    static_assert(!Ternary::False == Ternary::True);
  }
  // ── fix(succ): the DOMAIN decides: ℵ₀ is a fixpoint, the finite fragment
  //    has none.  Whether the successor closes on itself (hence whether the
  //    normal form / membership is decided) is a property of the carrier, not
  //    of the arrow, the Galois-specialisation axis. ──
  {
    constexpr cardinality_succ succ{};
    static_assert(succ(finite_cardinality(3)) !=
                  finite_cardinality(3));     // none finite
    static_assert(succ(aleph_0) == aleph_0);  // fix(succ) = ℵ₀ (saturation)
  }
  // ── membership CLASSIFIED into Ω: a comprehension over ℝ (ℶ₁) ──
  {
    // ℝ_d is tagged ℶ₁ (uncountable) but has Boole logic.  The Set{} wrap runs
    // NaturalLogic<Halfspace> in deduction, routing the ℶ₁ carrier to Kleene,
    // so the wrapped set withholds decidable membership.
    constexpr auto gt = Set{ℝ_d | (χ > bound<5.0>)};  // {r ∈ ℝ | r > 5}
    // χ_gt : ℝ → Ω, not ℝ → Σ; the type system withholds decidable membership
    static_assert(std::same_as<typename decltype(gt)::logic_species, Kleene>);
    static_assert(!HasDecidableMembership<decltype(gt)>);
  }
  // ── membership RETURNS Unknown: the intensional image of a set ──
  {
    constexpr auto gt5 =
        Set{ℕ | (χ > fix(5_c))};  // {n ∈ ℕ | n > 5} : decidable (ℵ₀)
    static_assert(HasDecidableMembership<decltype(gt5)>);
    constexpr cardinality_succ succ;
    constexpr auto s = arrow<Cardinality, Cardinality>(succ);  // ℕ → ℕ
    constexpr auto img = image(s, gt5);  // {y | ∃n. n>5 ∧ y=n+1} : Ω
    static_assert(std::same_as<typename decltype(img)::logic_species, Kleene>);
    static_assert(!HasDecidableMembership<decltype(img)>);
    // honestly Unknown, though set-theoretically 7 ∈ image(succ, {n>5})
    static_assert(img(finite_cardinality(7)) == Ternary::Unknown);
  }
  CHECK(true);  // runtime anchor for coverage
}
