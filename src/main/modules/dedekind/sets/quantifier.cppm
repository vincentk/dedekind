/**
 * @file dedekind/sets/quantifier.cppm
 * @partition :quantifier
 * @brief Bounded first-order quantifiers as logic-species-aware reductions
 *        over a range: @f$\forall@f$ is a @f$\bigwedge@f$, @f$\exists@f$ a
 *        @f$\bigvee@f$, over the truth object @f$\Omega@f$.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section quantifier__The_Single_Source
 * @c forall / @c exists fold a predicate over any @c std::ranges::input_range,
 * accumulating in the predicate's own truth object @f$\Omega@f$ (not a bare
 * @c bool) and short-circuiting at the absorbing element (@c False for
 * @f$\wedge@f$, @c True for @f$\vee@f$).  This is the @b single implementation
 * of bounded quantification in the library: the @c sequences layer's
 * @c Path -shaped @c forall / @c exists delegate here (a @c Path exposes an
 * iterator range), so there is one fold, not two.  Keeping it range-generic
 * --- rather than specialised to a concrete finite container --- states the
 * general concept (quantify over @b any enumerable domain) at the right
 * altitude; the domain is reached through the @c std iterator interface.
 *
 * @section quantifier__Decidability
 * A total fold terminates only over a @b finite range.  Over an unbounded
 * intensional domain, general first-order quantification is undecidable at
 * compile time (Rice's theorem); the library therefore restricts the
 * combinators to enumerable (@c IsExtensional / @c IsEnumerated) domains.
 * This partition is the mechanical realisation of that restriction.
 *
 * @build_order after :cardinality
 * @dependency :category
 */
module;

#include <functional>  // std::invoke
#include <ranges>      // std::ranges::input_range, range_value_t
#include <utility>     // std::forward

export module dedekind.sets:quantifier;

import dedekind.category; // LogicalMap, OmegaOf, LogicOf, the Logic species

namespace dedekind::sets {

/**
 * @brief @f$\forall x \in R.\; \text{pred}(x)@f$ --- the @f$\bigwedge@f$
 *        reduction of @c pred over the range @c R, in @c pred's truth object.
 *
 * @details Logic-species-aware: accumulates in @c OmegaOf<Pred, T> and
 * short-circuits at the absorbing element @c Logic::False.  Terminates for a
 * finite range; over an infinite range it is the co-recursive @f$\wedge@f$
 * that the enumerability restriction (@c IsExtensional) exists to forbid.
 */
export template <std::ranges::input_range R, typename Pred>
  requires dedekind::category::LogicalMap<Pred, std::ranges::range_value_t<R>>
constexpr auto forall(R&& range, Pred&& pred) {
  using T = std::ranges::range_value_t<R>;
  using Omega = dedekind::category::OmegaOf<Pred, T>;
  using Logic = dedekind::category::LogicOf<Omega>;

  auto predicate = std::forward<Pred>(pred);
  Omega witness = Logic::True;
  for (const auto& x : range) {
    witness = Logic::AND(witness, std::invoke(predicate, x));
    if (witness == Logic::False) break;  // short-circuit: ⊥ is absorbing
  }
  return witness;
}

/**
 * @brief @f$\exists x \in R.\; \text{pred}(x)@f$ --- the @f$\bigvee@f$
 *        reduction of @c pred over the range @c R, in @c pred's truth object.
 *
 * @details Dual of @c forall: accumulates in @c OmegaOf<Pred, T> and
 * short-circuits at the absorbing element @c Logic::True.
 */
export template <std::ranges::input_range R, typename Pred>
  requires dedekind::category::LogicalMap<Pred, std::ranges::range_value_t<R>>
constexpr auto exists(R&& range, Pred&& pred) {
  using T = std::ranges::range_value_t<R>;
  using Omega = dedekind::category::OmegaOf<Pred, T>;
  using Logic = dedekind::category::LogicOf<Omega>;

  auto predicate = std::forward<Pred>(pred);
  Omega witness = Logic::False;
  for (const auto& x : range) {
    witness = Logic::OR(witness, std::invoke(predicate, x));
    if (witness == Logic::True) break;  // short-circuit: ⊤ is absorbing
  }
  return witness;
}

/** @section quantifier__Formal_Verification */

// ∀ / ∃ over a finite integer range (std::views::iota), ClassicalLogic Ω.
static_assert(forall(std::views::iota(2, 8), [](int x) { return x > 1; }),
              "every element of [2,8) is > 1.");
static_assert(!forall(std::views::iota(0, 3), [](int x) { return x > 0; }),
              "0 ∈ [0,3) is not > 0, so ∀ fails.");
static_assert(exists(std::views::iota(0, 5), [](int x) { return x == 3; }),
              "3 ∈ [0,5), so ∃ holds.");
static_assert(!exists(std::views::iota(0, 3), [](int x) { return x > 9; }),
              "no element of [0,3) is > 9.");

}  // namespace dedekind::sets
