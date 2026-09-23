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
  //     collapses two disjoint halfspaces to Ø at the TYPE level.  This is the
  //     counterexample set of ∀x>5. x≥3, namely {x>5 ∧ x<3}, decided empty at
  //     compile time.  The operands are Set-wrapped halfspaces; the MEET is
  //     spelled bare (no `set(gt5 & lt3, …)` around the result).  `Set{…} &
  //     Set{…}` reaches the order-layer halfspace structured_and via ADL and
  //     collapses the disjoint pair to `Ø<Cardinality>`.  Since #895 the BARE
  //     point-free spelling `(ℕ | …) & (ℕ | …)` collapses to the SAME
  //     `Ø<Cardinality>` (the disjoint halfspace-meet is canonicalised through
  //     the empty set, no longer the raw `EmptyPredicate<Cardinality>` that had
  //     no `== Ø<Cardinality>`), so the `Set{}` wrap is no longer required for
  //     the comparison — both spellings are witnessed just below.
  constexpr auto gt5 = Set{ℕ | (π > fix(5_c))};
  constexpr auto lt3 = Set{ℕ | (π < fix(3_c))};
  static_assert(Ø<Cardinality>{} == (gt5 & lt3),
                "{x>5 ∧ x<3} collapses to Ø at compile time (order layer).");

  //     #895: the same collapse holds on the BARE point-free grammar, no
  //     `Set{}` wrapper — the exact case that forced the wrap before #895.
  constexpr auto gt5_bare = ℕ | (π > fix(5_c));
  constexpr auto lt3_bare = ℕ | (π < fix(3_c));
  static_assert(Ø<Cardinality>{} == (gt5_bare & lt3_bare),
                "bare {x>5} ∩ {x<3} == Ø<Cardinality> (no Set{} wrap). #895");
  CHECK(Ø<Cardinality>{} == (gt5_bare & lt3_bare));  // runtime (Codecov)

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
// directly — before #895 `!(A | pred)` had no set-complement path (it fell to
// `category::operator!`, a formal Morphism A → Ω) and needed a defensive
// `Set{}` wrap.  Here the bare comprehension is complemented with no wrapper.
TEST_CASE("Bare comprehension carries the set-complement (#895)",
          "[sets][comprehension][complement]") {
  using Comp = Comprehension<UniversalSet<int>, gt_ten>;
  constexpr Comp comp{UniversalSet<int>{}, gt_ten{}};

  // The complement routes through the set `operator!` and materialises as a
  // plain Set whose predicate is the negated comprehension — a genuine
  // set-complement, not a formal arrow.
  constexpr auto ncomp = !comp;
  static_assert(
      std::same_as<std::remove_cvref_t<decltype(ncomp)>,
                   Set<int, dedekind::category::Boole, NegatedPredicate<Comp>>>,
      "!(A | pred) is the set-complement of the bare comprehension.");
  static_assert(std::same_as<std::remove_cvref_t<decltype(~comp)>,
                             std::remove_cvref_t<decltype(ncomp)>>,
                "~ aliases ! on a bare comprehension.");

  // Membership is complemented pointwise, checked at runtime (Codecov).
  CHECK_FALSE(static_cast<bool>(comp(5)));    // 5 ∉ {n | n>10}
  CHECK(static_cast<bool>(ncomp(5)));         // 5 ∈ ¬{n | n>10}
  CHECK(static_cast<bool>(comp(15)));         // 15 ∈ {n | n>10}
  CHECK_FALSE(static_cast<bool>(ncomp(15)));  // 15 ∉ ¬{n | n>10}
}
