/**
 * @file dedekind/sets/quantifier.cppm
 * @partition :quantifier
 * @brief Bounded first-order quantifiers as @b set operations: @f$\exists@f$ is
 *        non-emptiness of a comprehension, @f$\forall@f$ its
 * @f$\neg\exists\neg@f$ dual.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section quantifier__The_Definition
 * A quantifier @b is a set operation (the Bourbaki / structural reading).  We
 * take that literally as the definition, not merely as a characterisation:
 * @f[ \exists x \in S.\,P(x) \;:\equiv\; \{x\in S \mid P(x)\} \neq \varnothing,
 *     \qquad
 *     \forall x \in S.\,P(x) \;:\equiv\; \neg\,\exists x \in S.\,\neg P(x). @f]
 * In code this is @c !(Ø{} @c == @c set(S,P)) and @c !exists(S, @c !P): the
 * comprehension is @c set(S,P), the emptiness test is an overload of
 * @c Ø::operator== , and @c forall is the double negation of @c exists.  There
 * is no separate fold; the reduction lives entirely in how @c Ø::operator==
 * decides emptiness, and the Rice boundary is therefore a @b type check (an
 * operand with no suitable @c == overload is a compile error) rather than
 * hand-waved prose.
 *
 * @section quantifier__Two_Regimes
 * The two decidable regimes are the two @c Ø::operator== overloads, chosen
 * structurally by the comprehension's shape:
 *   - @b compile @b time: a transparent halfspace meet collapses to @c Ø at the
 *     TYPE level via the @c dedekind.order @c structured_and specialization, so
 *     @c Ø @c == @c (gt5 @c & @c lt3) is a @c static_assert.  That collapse is
 *     an @b order-layer capability (it needs the halfspace types), reached
 *     through the @c & meet combinator, not through @c set (see below).
 *   - @b run @b time: over an enumerable domain, @c set(S,P) is a lazy
 *     @c std::views::filter and @c Ø @c == decides emptiness by @c begin @c ==
 *     @c end, short-circuiting at the first witness.
 * A genuinely opaque, non-enumerable operand matches no @c == overload: the
 * honest Rice wall, a compile error rather than a fabricated answer.
 *
 * @build_order after :cardinality
 * @dependency :category
 */
module;

#include <ranges>       // std::views::filter, input_range, range_value_t
#include <type_traits>  // std::remove_cvref_t (set(S,P) predicate dispatch)
#include <utility>      // std::move

export module dedekind.sets:quantifier;

import dedekind.category; // IsSet, ambient_set (the domain-is-a-set witness)
import :boundaries;       // Ø — the emptiness anchor the quantifiers compare to

namespace dedekind::sets {

/**
 * @brief @c set(S, P): the ETCS refinement @f$\{x \in S \mid P(x)\}@f$ over an
 *        enumerable domain, as a lazy @c std::views::filter.
 *
 * @details This is the @b runtime refinement combinator (pure @c dedekind.sets:
 * no order specialization).  It is deliberately lazy: emptiness is decided by
 * @c Ø::operator== advancing @c filter's @c begin() to the first witness, so
 * @c exists short-circuits without materialising the comprehension.  The
 * @b intensional meet of two predicate sets is spelled @c s @c & @c p directly:
 * @c & is the open meet combinator whose halfspace collapse is a downstream
 * @c dedekind.order specialization, reachable only where that specialization is
 * (below @c order).  Re-wrapping it in an upstream @c sets factory would freeze
 * the ADL lookup and silently drop the collapse, so the two refinement regimes
 * keep two honest surfaces: @c & (intensional, order-specialized, compile time)
 * and @c set (extensional, sets-only, run time).
 */
export template <typename S, typename P>
  requires std::ranges::input_range<S> &&
           (!dedekind::category::IsSet<std::remove_cvref_t<P>>)
constexpr auto set(const S& s, P p) {
  return std::views::filter(s, std::move(p));
}

/**
 * @brief @f$\exists x \in S.\; P(x)@f$ --- non-emptiness of the comprehension
 *        @c set(S,P).
 *
 * @details @c !(Ø == set(S,P)).  The Rice boundary is the availability of an
 * @c Ø::operator== overload for the comprehension: an enumerable domain
 * resolves at run time (@c begin @c == @c end), a transparent collapse at
 * compile time, and anything else is a compile error.
 */
export template <std::ranges::input_range S, typename P>
  requires(!dedekind::category::IsSet<std::remove_cvref_t<P>>)
constexpr bool exists(const S& s, P p) {
  using V = std::ranges::range_value_t<S>;
  return !(Ø<V>{} == set(s, std::move(p)));
}

/**
 * @brief @f$\forall x \in S.\; P(x)@f$ --- the @f$\neg\exists\neg@f$ dual: no
 *        counterexample.
 *
 * @details @c !exists(S, !P): @f$\forall@f$ holds iff the counterexample set
 * @f$\{x\in S \mid \neg P(x)\}@f$ is empty.  Building @f$\forall@f$ on
 * @f$\exists@f$ keeps a single emptiness primitive.
 */
export template <std::ranges::input_range S, typename P>
  requires(!dedekind::category::IsSet<std::remove_cvref_t<P>>)
constexpr bool forall(const S& s, P p) {
  return !exists(s, [p = std::move(p)](const auto& x) { return !p(x); });
}

/** @section quantifier__Formal_Verification */

// ∀ / ∃ over a finite integer range (std::views::iota) — the enumerable
// (runtime-shaped) regime, evaluated here at compile time.
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
 * Both are built on the set-operation @c exists / @c forall above, and the
 * domain is any @c std::ranges::input_range: the general concept reached
 * through the @c std iterator interface, not a fixed container.
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

}  // namespace dedekind::sets
