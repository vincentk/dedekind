/** @file dedekind/order/set_expression_exhibit_test.cpp
 *
 * THE EXHIBIT (epic #888, paper §3 + README): a non-trivial set expression the
 * C++ type system COLLAPSES at compile time.  Two overlapping halfspaces on ℤ,
 *
 *     A = { x | x < 5 }        B = { x | x > -5 }
 *
 * collapse the meet to the bounded open interval (-5, 5).  The collapse is a
 * VALUE collapse, not a type collapse: the pivot rides in the halfspace
 * INSTANCE, so `structured_and(A, B)` folds to a `Interval`, a cut whose sides
 * are value fields.  The point
 * is the OPTIONALITY: the SAME `constexpr` folds at compile time (a
 * `static_assert` that `meet` is the interval (-5, 5)) AND runs at runtime (the
 * identical `meet.contains(x)`).  A value pivot cannot dispatch a distinct
 * return TYPE per outcome, so the meet returns one unified `Interval`.
 *
 * The crossing JOIN (A | B) is a run list, the normal form of unions of
 * intervals: one type whatever the pivots, so the opposing cover collapses on
 * the values to 𝔸, and the complement laws A ∩ ¬A = Ø, A ∪ ¬A = 𝔸 hold on it.
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
constexpr auto A =
    Interval<int>::half(5, Direction::Downward, Strictness::Strict);
constexpr auto B =
    Interval<int>::half(-5, Direction::Upward, Strictness::Strict);

// ── Compile-time collapse proof (the "wow"): the reduced form is a VALUE that
//    folds at constexpr, not an unevaluated AndPredicate. ────────────────────
constexpr auto meet = structured_and(A, B);  // {x<5} ∩ {x>-5}
static_assert(meet == Interval<int>::bounded(-5, Strictness::Strict, 5,
                                             Strictness::Strict),
              "A & B collapsed to the bounded interval (-5, 5).");

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
    STATIC_CHECK(meet == Interval<int>::bounded(-5, Strictness::Strict, 5,
                                                Strictness::Strict));
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

  SECTION("A | B covers ℤ: the union is a run list, and it is 𝔸") {
    // The opposite-direction cover is decided on the run list's values, so the
    // union of {x < 5} and {x > -5} is the universe itself.
    STATIC_CHECK((A | B) == 𝔸<int>{});
    STATIC_CHECK(static_cast<bool>((A | B)(100)));
    STATIC_CHECK(static_cast<bool>((A | B)(-100)));
  }

  // Complement laws (the lattice ⊥/⊤): ~{x<5} is the part above, {x≥5}.  Both
  // collapse on the run list: the meet to Ø, the join to 𝔸.
  SECTION("A & ~A → Ø (contradiction), A | ~A → 𝔸 (excluded middle)") {
    STATIC_CHECK((A & ~A) == Ø<int>{});  // {x<5} ∩ {x≥5}
    STATIC_CHECK((A | ~A) == 𝔸<int>{});  // {x<5} ∪ {x≥5}
    STATIC_CHECK(~A == (𝔸<int>{} | (π >= 5)));
  }

  CHECK(true);  // runtime anchor for coverage
}
