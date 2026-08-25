/** @file test/cpp/modules/dedekind/analysis/quantifier_emptiness_test.cpp
 *
 * The quantifier machinery, tested once, in its two decidable regimes.  The
 * definition is a set operation: emptiness of a comprehension, `Ø == …` (∃ its
 * negation, ∀ the double negation).  The two regimes live at two @b layers,
 * and the honest point is that the split is architectural, not hand-waved:
 *   (a) COMPILE time: the `&` meet combinator (dedekind.sets) dispatches to the
 *       halfspace `structured_and` @b specialization (dedekind.order), which
 *       collapses two disjoint halfspaces to `Ø` at the TYPE level, so
 *       `Ø == (gt5 & lt3)` is a static_assert.  The collapse is the downstream
 *       specialization firing, reachable at any call site below `order`.
 *   (b) RUN time: `set(S, P)` (dedekind.sets) filters an IsExtensional carrier,
 *       and `Ø == …` decides emptiness via size() — pure `sets`, no
 *       specialization needed.
 * A genuinely opaque, non-extensional operand has no suitable overload: a
 * compile error, which is the honest Rice wall.  Source for the §3 Listing.
 */
#include <catch2/catch_test_macros.hpp>
#include <unordered_set>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::numbers;
using namespace dedekind::order;

TEST_CASE("Quantifier machinery: Ø == comprehension, two regimes",
          "[sets][quantifier][emptiness]") {
  // (a) COMPILE time, order specialization: the `&` meet combinator dispatches
  //     to the halfspace structured_and specialization (dedekind.order), which
  //     collapses two disjoint halfspaces to Ø at the TYPE level.  This is the
  //     counterexample set of ∀x>5. x≥3, namely {x>5 ∧ x<3}, decided empty at
  //     compile time.  We spell the bare `&` (not a set(...) wrapper): the open
  //     combinator's specialization is reachable only at a call site below
  //     order, and an upstream sets-layer wrapper would freeze the lookup.
  constexpr auto gt5 = Set{in<ℕ> | in<ℕ> > bound<5>};
  constexpr auto lt3 = Set{in<ℕ> | in<ℕ> < bound<3>};
  static_assert(Ø<Cardinality>{} == (gt5 & lt3),
                "{x>5 ∧ x<3} collapses to Ø at compile time (order layer).");

  // (b) RUN time, pure sets: set(S, P)'s extensional arm filters an
  //     IsExtensional carrier, and Ø == … decides emptiness via size().  No
  //     specialization needed — the combinator alone suffices at this layer.
  const std::unordered_set<int> dom{1, 2, 3, 4, 5};
  CHECK(Ø<int>{} == set(dom, [](const int& x) { return x > 100; }));  // ∅
  CHECK_FALSE(Ø<int>{} ==
              set(dom, [](const int& x) { return x > 3; }));  // {4,5}
}
