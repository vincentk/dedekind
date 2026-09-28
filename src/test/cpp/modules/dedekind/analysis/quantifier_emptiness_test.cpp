/** @file test/cpp/modules/dedekind/analysis/quantifier_emptiness_test.cpp
 *
 * The quantifier machinery, tested once, in its two decidable regimes.  The
 * definition is a set operation: emptiness of a comprehension, `Ø == …` (∃ its
 * negation, ∀ the double negation).  The two regimes live at two @b layers,
 * and the honest point is that the split is architectural, not hand-waved:
 *   (a) COMPILE time: the `&` meet combinator (dedekind.sets) dispatches to the
 *       halfspace `structured_and` @b specialization (dedekind.order), which
 *       folds two disjoint halfspaces to the empty set value-first, so
 *       `(gt5 & lt3).kind == Empty` is a static_assert.  The fold is the
 *       downstream specialization firing, reachable at any call site below
 *       `order`.
 *   (b) RUN time: `set(S, P)` (dedekind.sets) filters an IsExtensional carrier,
 *       and `Ø == …` decides emptiness via size() — pure `sets`, no
 *       specialization needed.
 * A genuinely opaque, non-extensional operand has no suitable overload: a
 * compile error, which is the honest Rice wall.  Source for the §3 Listing.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>
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
  //     folds two disjoint halfspaces to the empty set VALUE-FIRST (the pivots
  //     are constexpr data, so the fold is a constant expression, not a type
  //     collapse).  This is the counterexample set of ∀x>5. x≥3, namely
  //     {x>5 ∧ x<3}, decided empty at compile time on the bare point-free
  //     grammar; no `Set{}` wrap is involved.
  constexpr auto gt5 = ℕ | (π > fix(5_c));
  constexpr auto lt3 = ℕ | (π < fix(3_c));
  static_assert((gt5 & lt3).kind == SetKind::Empty,
                "{x>5 ∧ x<3} folds to the empty set at compile time.");
  CHECK((gt5 & lt3).kind == SetKind::Empty);  // runtime (Codecov)

  // (b) RUN time, pure sets: over an enumerable domain, set(S, P) is a lazy
  //     views::filter and Ø == … decides emptiness by begin == end, short-
  //     circuiting at the first witness.  No specialization needed.
  const std::unordered_set<int> dom{1, 2, 3, 4, 5};

  //     The definition: ∃ is non-emptiness of the comprehension.
  CHECK(Ø<int>{} == set(dom, [](const int& x) { return x > 100; }));  // ∅ : ∄
  CHECK_FALSE(Ø<int>{} ==
              set(dom, [](const int& x) { return x > 3; }));  // {4,5}

  //     The surface: exists / forall read that same emptiness.  forall is the
  //     ¬∃¬ dual, so it holds iff the counterexample set is empty.
  CHECK(exists(dom, [](const int& x) { return x > 3; }));  // 4 witnesses
  CHECK_FALSE(exists(dom, [](const int& x) { return x > 100; }));  // none
  CHECK(forall(dom, [](const int& x) { return x <= 5; }));  // no counterexample
  CHECK_FALSE(forall(dom, [](const int& x) { return x > 2; }));  // 1,2 counter
}

namespace {
/** @brief An arbitrary (non-halfspace) membership predicate, so `{𝔸 | gt_ten}`
 *  stays a bare @c Comprehension rather than folding to a @c Halfspace.  Named
 *  (not a lambda) per the house style. */
struct gt_ten {
  constexpr bool operator()(int n) const { return n > 10; }
};
}  // namespace

// #895: a bare `Comprehension` (the point-free `A | pred` set-builder result)
// is a first-class set-node, so the free set-complement `!` / `~` applies to it
// directly. Before #895, `!(A | pred)` had no set-complement path (it fell to
// `category::operator!`, a formal Morphism A → Ω) and needed a defensive
// `Set{}` wrap.  Here the bare comprehension is complemented with no wrapper.
TEST_CASE("Bare comprehension carries the set-complement (#895)",
          "[sets][comprehension][complement]") {
  using Comp = Comprehension<Universe<int>, gt_ten>;
  constexpr Comp comp{Universe<int>{}, gt_ten{}};

  // The set complement ~ is the reducer's Not node over the comprehension: a
  // genuine set-complement subobject, not a formal arrow.  ~~ peels back.
  constexpr auto ncomp = ~comp;
  static_assert(std::same_as<std::remove_cvref_t<decltype(ncomp)>,
                             dedekind::category::Not<Comp>>,
                "~(A | pred) is the set-complement Not<Comprehension>.");
  static_assert(std::same_as<std::remove_cvref_t<decltype(~~comp)>, Comp>,
                "~~ peels back to the bare comprehension (involution).");

  // Membership is complemented pointwise, checked at runtime (Codecov).
  CHECK_FALSE(static_cast<bool>(comp(5)));    // 5 ∉ {n | n>10}
  CHECK(static_cast<bool>(ncomp(5)));         // 5 ∈ ¬{n | n>10}
  CHECK(static_cast<bool>(comp(15)));         // 15 ∈ {n | n>10}
  CHECK_FALSE(static_cast<bool>(ncomp(15)));  // 15 ∉ ¬{n | n>10}
}
