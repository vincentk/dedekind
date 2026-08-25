/** @file test/cpp/modules/dedekind/analysis/quantifier_emptiness_test.cpp
 *
 * The quantifier machinery, tested once, in its two decidable regimes.  The
 * definition is a set operation: a quantifier is `Ø == comprehension` (∃ its
 * negation, ∀ the double negation).  Which `operator==` overload resolves it
 * is where the two regimes show:
 *   (a) a transparent halfspace predicate collapses at COMPILE time
 *       (structured_and → Ø), so the comparison is a static_assert;
 *   (b) an opaque lambda over an IsExtensional carrier resolves at RUN time
 *       via size(), a different overload of the same `==`.
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
  // (a) TRANSPARENT predicate (halfspace): emptiness is decided at COMPILE time
  //     by structured_and, so `Ø == …` is a static_assert.
  //     ∀x>5. x≥3  ⟺  the counterexample set {x>5 ∧ x<3} reduces to Ø.
  //     ∀x>5. x≥3  ⟺  the counterexample set {x>5 ∧ x<3} reduces to Ø, and
  //     that reduction is a TYPE identity decided at compile time.
  //     set(S, P) is the ETCS refinement; its intensional arm routes the meet
  //     through structured_and, collapsing {x>5 ∧ x<3} to Ø at COMPILE time.
  constexpr auto gt5 = Set{in<ℕ> | in<ℕ> > bound<5>};
  constexpr auto lt3 = Set{in<ℕ> | in<ℕ> < bound<3>};
  static_assert(
      Ø<Cardinality>{} == set(gt5, lt3),
      "set(gt5, lt3) = {x>5 ∧ x<3} is empty, decided at compile time.");

  // (b) OPAQUE lambda over an IsExtensional carrier: set(S, P)'s extensional
  // arm
  //     filters, and emptiness is decided at RUN time by size().  Same set(...)
  //     surface, the coproduct arm chosen structurally by the carrier.
  const std::unordered_set<int> dom{1, 2, 3, 4, 5};
  CHECK(Ø<int>{} == set(dom, [](const int& x) { return x > 100; }));  // ∅
  CHECK_FALSE(Ø<int>{} ==
              set(dom, [](const int& x) { return x > 3; }));  // {4,5}
}
