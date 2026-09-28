/** @file dedekind/order/set_expression_exhibit_test.cpp
 *
 * THE EXHIBIT (epic #888, paper §3 + README): a non-trivial set expression the
 * C++ type system COLLAPSES at compile time.  Two overlapping halfspaces on ℤ,
 *
 *     A = { x | x < 5 }        B = { x | x > -5 }
 *
 * collapse the meet to the bounded open interval (-5, 5).  The collapse is a
 * VALUE collapse, not a type collapse: the pivot rides in the halfspace
 * INSTANCE, so `structured_and(A, B)` folds to a `SetVal` (kind-tagged: Empty /
 * Halfspace / Singleton / Interval) whose bounds are value fields.  The point
 * is the OPTIONALITY: the SAME `constexpr` folds at compile time (a
 * `static_assert` on `meet.kind` / `meet.lo`) AND runs at runtime (the
 * identical `meet.contains(x)`).  A value pivot cannot dispatch a distinct
 * return TYPE per outcome, so the meet returns one unified `SetVal`.
 *
 * The crossing JOIN (A | B) does NOT structurally collapse: the opposing cover
 * cannot be dispatched on a runtime pivot, so the union is the honest
 * point-wise set that still DECIDES membership (it covers ℤ).  This mirrors the
 * ℕ leg of pruning_lattice_laws_test.
 *
 * The former type-level term-reducer exhibits (Meet/Join materialisation
 * + absorption + involution; subobject distributivity to DNF) reduced
 * halfspaces by reading their pivots FROM THE TYPE, which value-carrying pivots
 * preclude. Those two axes retired; reconciliation is tracked in #970 (the
 * reducer machinery itself still serves the interval-based paths).
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

// A = { x < 5 } (downward, strict);  B = { x > -5 } (upward, strict).  The
// pivot rides in the VALUE, so A / B are instances, not distinct types.
using ADown = Halfspace<int, Direction::Downward, Strictness::Strict, Boole>;
using BUp = Halfspace<int, Direction::Upward, Strictness::Strict, Boole>;
constexpr ADown A{5};
constexpr BUp B{-5};

// ── Compile-time collapse proof (the "wow"): the reduced form is a VALUE that
//    folds at constexpr, not an unevaluated AndPredicate. ────────────────────
constexpr auto meet = structured_and(A, B);  // {x<5} ∩ {x>-5}
static_assert(meet.kind == SetKind::Interval,
              "A & B collapsed to the bounded interval (-5, 5).");
static_assert(meet.lo == -5, "A & B collapsed to lower bound -5.");
static_assert(meet.hi == 5, "A & B collapsed to upper bound 5.");
static_assert(meet.sl == Strictness::Strict && meet.su == Strictness::Strict,
              "the open interval (-5, 5).");

}  // namespace set_expression_exhibit

// ── Membership: the collapsed value classifies exactly as the original
//    expression would, so the compile-time optimization is meaning-preserving.
//    The SAME constexpr `meet` decides membership at compile time
//    (STATIC_CHECK) and at runtime — that optionality is the payoff.
TEST_CASE("Exhibit: two overlapping halfspaces collapse (value-first, #888)",
          "[order][sets][exhibit][collapse]") {
  using namespace set_expression_exhibit;

  SECTION("A & B is the open interval (-5, 5), decided at compile time") {
    // The collapse IS the point: pin the reduced KIND + bounds, not just
    // membership — else the exhibit would pass even if the meet stopped
    // collapsing.
    STATIC_CHECK(meet.kind == SetKind::Interval);
    STATIC_CHECK(meet.contains(0));
    STATIC_CHECK(meet.contains(4));
    STATIC_CHECK(meet.contains(-4));
    STATIC_CHECK_FALSE(meet.contains(5));   // boundary excluded (strict)
    STATIC_CHECK_FALSE(meet.contains(-5));  // boundary excluded (strict)
    STATIC_CHECK_FALSE(meet.contains(100));
    // |(-5, 5) ∩ ℤ| = 9 (the integers -4..4), counted from the value bounds.
    int inhabitants = 0;
    for (int x = -10; x <= 10; ++x)
      if (meet.contains(x)) ++inhabitants;
    CHECK(inhabitants == 9);
  }

  SECTION("A | B covers ℤ (opposing cover, decided by membership)") {
    // Value-carrying cannot dispatch the opposite-direction cover-vs-gap on a
    // runtime pivot, so the union is the honest point-wise set (not a
    // structural 𝔸); it still covers every element.
    constexpr auto join = A | B;
    STATIC_CHECK(static_cast<bool>(join(0)));
    STATIC_CHECK(static_cast<bool>(join(100)));
    STATIC_CHECK(static_cast<bool>(join(-100)));
    STATIC_CHECK(static_cast<bool>(join(5)));
    STATIC_CHECK(static_cast<bool>(join(-5)));
  }

  // Complement laws (the lattice ⊥/⊤): ~ = the opposite halfspace with the same
  // pivot (~{x<5} = {x≥5}).  The MEET collapses STRUCTURALLY to the empty
  // SetVal kind; the JOIN is decided by membership (the crossing cover).
  SECTION("A & ~A → Ø (contradiction), A | ~A covers ℤ (excluded middle)") {
    constexpr auto contradiction = structured_and(A, ~A);  // {x<5} ∩ {x≥5}
    STATIC_CHECK(contradiction.kind == SetKind::Empty);
    STATIC_CHECK_FALSE(contradiction.contains(0));
    STATIC_CHECK_FALSE(contradiction.contains(4));
    STATIC_CHECK_FALSE(contradiction.contains(5));
    constexpr auto excluded_middle = A | ~A;  // {x<5} ∪ {x≥5}
    STATIC_CHECK(static_cast<bool>(excluded_middle(0)));
    STATIC_CHECK(static_cast<bool>(excluded_middle(4)));
    STATIC_CHECK(static_cast<bool>(excluded_middle(5)));
  }

  CHECK(true);  // runtime anchor for coverage
}
