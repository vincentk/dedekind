/** @file test/cpp/modules/dedekind/analysis/quantifier_emptiness_test.cpp
 *
 * The quantifier machinery: emptiness of a comprehension, `Ø == …` (∃ its
 * negation, ∀ the double negation).  The `&` meet combinator (dedekind.sets)
 * dispatches to the halfspace `structured_and` @b specialization
 * (dedekind.order), which folds two disjoint halfspaces to the empty set
 * value-first, so `(gt5 & lt3) == Ø` is a static_assert.  A genuinely opaque
 * operand has no suitable overload: a compile error, which is the honest Rice
 * wall.  Source for the §3 Listing.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::numbers;
using namespace dedekind::order;

TEST_CASE("Quantifier machinery: Ø == comprehension, folded at compile time",
          "[sets][quantifier][emptiness]") {
  // (a) COMPILE time, order specialization: the `&` meet combinator dispatches
  //     to the halfspace structured_and specialization (dedekind.order), which
  //     folds two disjoint halfspaces to the empty set VALUE-FIRST (the pivots
  //     are constexpr data, so the fold is a constant expression, not a type
  //     collapse).  This is the counterexample set of ∀x>5. x≥3, namely
  //     {x>5 ∧ x<3}, decided empty at compile time on the bare point-free
  //     grammar; no `Set{}` wrap is involved.
  constexpr auto gt5 = ℕ & (π > 5);
  constexpr auto lt3 = ℕ & (π < 3);
  static_assert((gt5 & lt3) == Ø<Cardinality>{},
                "{x>5 ∧ x<3} folds to the empty set at compile time.");
  CHECK((gt5 & lt3) == Ø<Cardinality>{});  // runtime (Codecov)
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
  using Comp = Comprehension<𝔸<int>, gt_ten>;
  constexpr Comp comp{𝔸<int>{}, gt_ten{}};

  // The set complement ~ is the reducer's Not node over the comprehension: a
  // genuine set-complement subobject, not a formal arrow.  ~~ peels back.
  constexpr auto ncomp = ~comp;
  static_assert(std::same_as<std::remove_cvref_t<decltype(ncomp)>,
                             dedekind::category::Not<Comp>>,
                "~(A & pred) is the set-complement Not<Comprehension>.");
  static_assert(std::same_as<std::remove_cvref_t<decltype(~~comp)>, Comp>,
                "~~ peels back to the bare comprehension (involution).");

  // Membership is complemented pointwise, checked at runtime (Codecov).
  CHECK_FALSE(static_cast<bool>(comp(5)));    // 5 ∉ {n | n>10}
  CHECK(static_cast<bool>(ncomp(5)));         // 5 ∈ ¬{n | n>10}
  CHECK(static_cast<bool>(comp(15)));         // 15 ∈ {n | n>10}
  CHECK_FALSE(static_cast<bool>(ncomp(15)));  // 15 ∉ ¬{n | n>10}
}
