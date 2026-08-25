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

#include <algorithm>    // std::ranges::count_if (structural characterisation)
#include <functional>   // std::invoke
#include <ranges>       // std::ranges::input_range, range_value_t
#include <type_traits>  // std::remove_cvref_t (set(S,P) predicate dispatch)
#include <utility>      // std::forward, std::move

export module dedekind.sets:quantifier;

import dedekind.category; // LogicalMap, OmegaOf, LogicOf, the Logic species
import :boundaries;  // Ø — the emptiness anchor the quantifiers compare against
import :expressions;  // Set, operator& (the structured_and refinement)
import :extensional;  // filter (the extensional refinement)

namespace dedekind::sets {

/**
 * @brief @c set(S, P): the ETCS refinement @f$\{x \in S \mid P(x)\}@f$ over an
 *        @b extensional carrier --- a callable predicate applied to an
 *        enumerable domain, realised as a @c filter that preserves @c size().
 *
 * @details This is the @b runtime refinement combinator (pure @c dedekind.sets:
 * no order specialization needed).  The @b intensional meet of two predicate
 * sets is spelled @c s @c & @c p directly: @c & is the open meet combinator
 * whose halfspace collapse is a downstream @c dedekind.order specialization,
 * and it must be invoked where that specialization is reachable (below
 * @c order) --- re-wrapping it in an upstream @c sets factory would freeze the
 * lookup and silently drop the collapse.  So the two refinement regimes have
 * two honest surfaces: @c & (intensional, order-specialized, compile time) and
 * @c set (extensional, sets-only, run time).
 */
export template <typename S, typename P>
  requires(!dedekind::category::IsSet<std::remove_cvref_t<P>>) &&
          requires(P& p, const S& s) { filter(p, s); }
constexpr auto set(const S& s, P p) {
  return filter(std::move(p), s);
}

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

// The quantifier domain is a genuine set: a std::views range lifts to IsSet
// via ambient_set (Jlt --- the set IS its membership test), so Exists / ForAll
// range over the elements of an ETCS set, not a bare container.
inline constexpr auto six = std::views::single(6);
static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set(six))>,
    "the existential's domain {6} is an ETCS set.");
inline constexpr auto two_three = std::views::iota(2, 4);
static_assert(dedekind::category::IsSet<
                  decltype(dedekind::category::ambient_set(two_three))>,
              "the universal's domain {2,3} is an ETCS set.");

// { x | ∃ y ∈ {6} : x*y == 42 } selects x = 7 --- the existential combinator
// binds y, leaving a predicate in x that the compiler collapses.
inline constexpr auto has_factor_of_42_in_6 =
    Exists(six, [](int y, int x) { return x * y == 42; });
static_assert(has_factor_of_42_in_6(7), "7·6 == 42, so 7 satisfies ∃y∈{6}.");
static_assert(!has_factor_of_42_in_6(8), "8·6 != 42.");

// { x | ∀ y ∈ {2,3} : x % y == 0 } selects the common multiples of 2 and 3.
inline constexpr auto divisible_by_2_and_3 =
    ForAll(two_three, [](int y, int x) { return x % y == 0; });
static_assert(divisible_by_2_and_3(6), "6 is divisible by both 2 and 3.");
static_assert(!divisible_by_2_and_3(9), "9 is not divisible by 2.");

/**
 * @section quantifier__Structural_Characterisation
 * The Bourbaki (structural) reading: a quantifier @b is a set operation.
 * @f$\exists@f$ is non-emptiness of the comprehension; @f$\forall@f$ is
 * emptiness of its complement (no counterexample --- the @f$\neg\exists\neg@f$
 * dual):
 * @f[ \exists x \in S.\,P(x) \iff \{x\in S \mid P(x)\} \neq \varnothing,
 *     \quad \forall x \in S.\,P(x) \iff \{x\in S \mid \neg P(x)\} =
 * \varnothing. @f] The short-circuiting @f$\bigvee@f$/@f$\bigwedge@f$
 * reductions above are the efficient implementation; the identities below
 * witness that they coincide with this extensional characterisation, so it is a
 * @b theorem, not prose. (Cardinality of a comprehension is well defined only
 * on a finite domain --- the same @c IsExtensional gate the reductions carry.)
 */
namespace {
inline constexpr auto q_dom = std::views::iota(0, 6);
inline constexpr auto q_pred = [](int x) { return x > 3; };
inline constexpr auto q_neg = [](int x) { return !(x > 3); };  // ¬P
}  // namespace

// ∃x∈S. P(x)  ⟺  {x∈S | P(x)} ≠ ∅
static_assert(static_cast<bool>(exists(q_dom, q_pred)) ==
              (std::ranges::count_if(q_dom, q_pred) != 0));
// ∀x∈S. P(x)  ⟺  {x∈S | ¬P(x)} = ∅   (no counterexample)
static_assert(static_cast<bool>(forall(q_dom, q_pred)) ==
              (std::ranges::count_if(q_dom, q_neg) == 0));

}  // namespace dedekind::sets
