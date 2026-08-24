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

/**
 * @section quantifier__Combinators
 * @c ForAll / @c Exists are the quantifier @b combinators: they bind the
 * @b inner variable @c y over the enumerable domain @c dom and leave a
 * predicate in the @b outer variable @c x.  @c Exists(dom, p2) is
 * @f$\lambda x.\;\exists y \in \mathrm{dom}.\; p_2(y,x)@f$; @c ForAll dually.
 * The result is an ordinary unary predicate, so it drops straight into a
 * @c Set comprehension --- this is how a bounded quantifier defines a new set.
 * Both are built on the range-generic @c exists / @c forall above (one fold),
 * and the domain is any @c std::ranges::input_range: the general concept
 * reached through the @c std iterator interface, not a fixed container.
 */
export template <std::ranges::input_range Dom, typename P2>
constexpr auto Exists(Dom dom, P2 p2) {
  return [dom, p2](const auto& x) {
    return exists(dom, [&p2, &x](const auto& y) { return p2(y, x); });
  };
}

export template <std::ranges::input_range Dom, typename P2>
constexpr auto ForAll(Dom dom, P2 p2) {
  return [dom, p2](const auto& x) {
    return forall(dom, [&p2, &x](const auto& y) { return p2(y, x); });
  };
}

/** @section quantifier__Formal_Verification_Combinators */

// { x | ∃ y ∈ {6} : x*y == 42 } selects x = 7 --- the existential combinator
// binds y, leaving a predicate in x that the compiler collapses.
inline constexpr auto has_factor_of_42_in_6 =
    Exists(std::views::single(6), [](int y, int x) { return x * y == 42; });
static_assert(has_factor_of_42_in_6(7), "7·6 == 42, so 7 satisfies ∃y∈{6}.");
static_assert(!has_factor_of_42_in_6(8), "8·6 != 42.");

// { x | ∀ y ∈ {2,3} : x % y == 0 } selects the common multiples of 2 and 3.
inline constexpr auto divisible_by_2_and_3 =
    ForAll(std::views::iota(2, 4), [](int y, int x) { return x % y == 0; });
static_assert(divisible_by_2_and_3(6), "6 is divisible by both 2 and 3.");
static_assert(!divisible_by_2_and_3(9), "9 is not divisible by 2.");

}  // namespace dedekind::sets
