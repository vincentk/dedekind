/** @file dedekind/order/set_expression_exhibit_test.cpp
 *
 * THE EXHIBIT (epic #888, paper §3 + README): a non-trivial set expression
 * that the C++ type system COLLAPSES at compile time — the compile-time
 * set-algebra showcase.  Two overlapping halfspaces on ℤ,
 *
 *     A = { x | x < 5 }        B = { x | x > -5 }
 *
 * collapse in BOTH lattice directions, and the collapse is proven at
 * compile time (the result TYPE changes), not merely computed at runtime:
 *
 *     A & B  ⟶  the bounded open interval (-5, 5)   (halfspace ∧ ⟹ interval)
 *     A | B  ⟶  the universe 𝔸                       (opposing cover ⟹
 * universe)
 *
 * This is the STRUCTURAL (lattice-law) axis of the exhibit.  A second TEST_CASE
 * (added with #892) shows the reducer's residual behaviour: an irreducible
 * meet / join MATERIALIZES as a MeetSet / JoinSet carrying both operands,
 * absorption reads through that materialization, and the complement is a
 * certified involution (!!A ≡ A).  The decidability axis
 * (undecidable-in-general ⟶ decidable-on-a-bounded-subset, #860/#858/#861) and
 * the Python mirror (#886) are the sibling sub-items of #888.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

namespace set_expression_exhibit {

// A = { x < 5 } (downward, strict);  B = { x > -5 } (upward, strict).
using A =
    Halfspace<int, 5, Direction::Downward, Strictness::Strict, ClassicalLogic>;
using B =
    Halfspace<int, -5, Direction::Upward, Strictness::Strict, ClassicalLogic>;

// ── Compile-time collapse proof (the "wow"): the result TYPE is the reduced
//    form, not an unevaluated AndPredicate / OrPredicate. ──────────────────

// A ∧ B ⟹ the bounded interval (-5, 5).  structured_and is what Set::operator&
// routes through for a halfspace meet.
using MeetForm = std::decay_t<decltype(structured_and(A{}, B{}))>;
static_assert(MeetForm::lower_pivot == -5,
              "A & B collapsed to the interval with lower bound -5.");
static_assert(MeetForm::upper_pivot == 5,
              "A & B collapsed to the interval with upper bound 5.");
static_assert(MeetForm::lower_strictness == Strictness::Strict &&
                  MeetForm::upper_strictness == Strictness::Strict,
              "the open interval (-5, 5).");
// Nine integers -4..4 — decided at compile time.
static_assert(structured_and(A{}, B{}).size() == 9u,
              "|(-5, 5) ∩ ℤ| = 9, decided at compile time.");

// A ∨ B ⟹ the universe.  The two opposing halfspaces cover ℤ (they overlap on
// (-5, 5)), so the join collapses to UniversalSet — NOT an OrPredicate.
// structured_or's opposing overload wants (Upward, Downward) order.
using JoinForm = std::decay_t<decltype(structured_or(B{}, A{}))>;
static_assert(std::same_as<JoinForm, UniversalSet<int, ClassicalLogic>>,
              "A | B collapsed to the universe 𝔸 (opposing halfspaces cover).");

}  // namespace set_expression_exhibit

// ── Membership: the collapsed sets classify exactly as the original
//    expression would, so the compile-time optimization is meaning-preserving.
//    All bona fide constexpr (Jlt) — STATIC_CHECK, decided at compile time.
TEST_CASE(
    "Exhibit: two overlapping halfspaces collapse (structural axis, #888)",
    "[order][sets][exhibit][collapse]") {
  using namespace set_expression_exhibit;
  constexpr Set<int, ClassicalLogic, A> SA{A{}};
  constexpr Set<int, ClassicalLogic, B> SB{B{}};

  SECTION("A & B is the open interval (-5, 5)") {
    constexpr auto meet = SA & SB;
    // The collapse IS the point: pin the reduced TYPE, not just membership —
    // else the exhibit would still pass if operator& stopped collapsing.
    STATIC_CHECK(std::same_as<std::decay_t<decltype(meet)>, MeetForm>);
    STATIC_CHECK(meet(0));
    STATIC_CHECK(meet(4));
    STATIC_CHECK(meet(-4));
    STATIC_CHECK_FALSE(meet(5));   // boundary excluded (strict)
    STATIC_CHECK_FALSE(meet(-5));  // boundary excluded (strict)
    STATIC_CHECK_FALSE(meet(100));
  }

  SECTION("A | B is the universe 𝔸 (covers ℤ)") {
    constexpr auto join = SA | SB;
    STATIC_CHECK(std::same_as<std::decay_t<decltype(join)>,
                              UniversalSet<int, ClassicalLogic>>);
    STATIC_CHECK(join(0));
    STATIC_CHECK(join(100));
    STATIC_CHECK(join(-100));
    STATIC_CHECK(join(5));
    STATIC_CHECK(join(-5));
  }

  // Complement laws (the lattice ⊥/⊤): the collapse also fires the boundary
  // reductions A & ~A → Ø and A | ~A → 𝔸 (~ = set complement; ~{x<5} = {x≥5}).
  SECTION(
      "A & ~A collapses to Ø (contradiction), A | ~A to 𝔸 (excluded middle)") {
    constexpr auto contradiction = SA & ~SA;
    STATIC_CHECK(std::same_as<std::decay_t<decltype(contradiction)>,
                              Ø<int, ClassicalLogic>>);
    STATIC_CHECK_FALSE(contradiction(0));
    STATIC_CHECK_FALSE(contradiction(4));
    STATIC_CHECK_FALSE(contradiction(5));
    constexpr auto excluded_middle = SA | ~SA;
    STATIC_CHECK(std::same_as<std::decay_t<decltype(excluded_middle)>,
                              UniversalSet<int, ClassicalLogic>>);
    STATIC_CHECK(excluded_middle(0));
    STATIC_CHECK(excluded_middle(4));
    STATIC_CHECK(excluded_middle(5));
  }
}

// ── The #892 axis: when a meet / join does NOT collapse to a boundary or an
//    interval, it MATERIALIZES as a MeetSet / JoinSet that carries both operand
//    sets (the AST is the set).  The reducer still reads through that
//    materialization boundary for absorption, and the complement is a certified
//    involution.  These are the capabilities #892 added on top of the glb / lub
//    and boundary collapses above.
TEST_CASE(
    "Exhibit: irreducible meet/join materialize, absorption, involution (#892)",
    "[order][sets][exhibit][collapse][892]") {
  using namespace set_expression_exhibit;
  // Lo = {x ≥ 5}, Hi = {x ≤ 2}: a disjoint pair with a gap at 3, 4, so their
  // union has no boundary or interval normal form to collapse into.
  using Lo = Halfspace<int, 5, Direction::Upward, Strictness::NonStrict,
                       ClassicalLogic>;
  using Hi = Halfspace<int, 2, Direction::Downward, Strictness::NonStrict,
                       ClassicalLogic>;
  constexpr Set<int, ClassicalLogic, Lo> SLo{Lo{}};
  constexpr Set<int, ClassicalLogic, Hi> SHi{Hi{}};

  SECTION("an irreducible union materializes as a JoinSet carrying both sets") {
    constexpr auto uni = SLo | SHi;  // {x ≥ 5} ∪ {x ≤ 2}, a genuine gap at 3, 4
    STATIC_CHECK(std::same_as<std::decay_t<decltype(uni)>,
                              JoinSet<Set<int, ClassicalLogic, Lo>,
                                      Set<int, ClassicalLogic, Hi>>>);
    STATIC_CHECK(uni(7));        // in Lo
    STATIC_CHECK(uni(1));        // in Hi
    STATIC_CHECK_FALSE(uni(3));  // the gap
  }

  SECTION("absorption reads through the materialized node: A & (A | B) → A") {
    // The union above is a JoinSet, yet the reducer still recognises Lo as one
    // of its operands, so the meet absorbs to Lo rather than nesting a MeetSet.
    constexpr auto absorbed = SLo & (SLo | SHi);
    STATIC_CHECK(std::same_as<std::decay_t<decltype(absorbed)>,
                              Set<int, ClassicalLogic, Lo>>);
    STATIC_CHECK(absorbed(7));
    STATIC_CHECK_FALSE(absorbed(1));  // 1 is in Hi = B, not in Lo = A
  }

  SECTION("complement is a certified involution: !!A ≡ A by type") {
    // A second complement peels the first rather than nesting it, so the double
    // negation eliminates and the original set TYPE is recovered (the
    // :involution witness that ¬ is involutive on the classical logic).
    STATIC_CHECK(std::same_as<std::decay_t<decltype(!!SLo)>,
                              std::decay_t<decltype(SLo)>>);
    STATIC_CHECK((!!SLo)(7) == SLo(7));
    STATIC_CHECK((!!SLo)(1) == SLo(1));
  }
}
