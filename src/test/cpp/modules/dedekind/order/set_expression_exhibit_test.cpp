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
 * This is the STRUCTURAL (lattice-law) axis of the exhibit.  The decidability
 * axis (undecidable-in-general ⟶ decidable-on-a-bounded-subset, #860/#858/#861)
 * and the Python mirror (#886) are the sibling sub-items of #888.
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

// ── Runtime behaviour: the collapsed sets classify exactly as the original
//    expression would, so the compile-time optimization is meaning-preserving.
TEST_CASE(
    "Exhibit: two overlapping halfspaces collapse (structural axis, #888)",
    "[order][sets][exhibit][collapse]") {
  using namespace set_expression_exhibit;
  constexpr Set<int, ClassicalLogic, A> SA{A{}};
  constexpr Set<int, ClassicalLogic, B> SB{B{}};

  SECTION("A & B is the open interval (-5, 5)") {
    constexpr auto meet = SA & SB;
    CHECK(meet(0));
    CHECK(meet(4));
    CHECK(meet(-4));
    CHECK_FALSE(meet(5));   // boundary excluded (strict)
    CHECK_FALSE(meet(-5));  // boundary excluded (strict)
    CHECK_FALSE(meet(100));
  }

  SECTION("A | B is the universe 𝔸 (covers ℤ)") {
    constexpr auto join = SA | SB;
    CHECK(join(0));
    CHECK(join(100));
    CHECK(join(-100));
    CHECK(join(5));
    CHECK(join(-5));
  }

  // Complement laws (the lattice ⊥/⊤): the collapse also fires the boundary
  // reductions A & ~A → Ø and A | ~A → 𝔸 (~ = set complement; ~{x<5} = {x≥5}).
  SECTION(
      "A & ~A collapses to Ø (contradiction), A | ~A to 𝔸 (excluded middle)") {
    constexpr auto contradiction = SA & ~SA;
    CHECK_FALSE(contradiction(0));
    CHECK_FALSE(contradiction(4));
    CHECK_FALSE(contradiction(5));
    constexpr auto excluded_middle = SA | ~SA;
    CHECK(excluded_middle(0));
    CHECK(excluded_middle(4));
    CHECK(excluded_middle(5));
  }
}
