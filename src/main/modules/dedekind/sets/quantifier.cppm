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
import :expressions;  // FiniteBooleanSet — finite-carrier materialisation (𝔹)

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
// @c S&& is a forwarding reference over a @c viewable_range: an rvalue range is
// moved into an owning @c filter_view (no dangling), an lvalue is referenced.
export template <std::ranges::viewable_range S, typename P>
  requires(!dedekind::category::IsSet<std::remove_cvref_t<P>>)
constexpr auto set(S&& s, P p) {
  return std::views::filter(std::forward<S>(s), std::move(p));
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

/**
 * @section quantifier__Finite_Quotient
 * @brief Quantifiers over a @b finite carrier, decided @b by type through the
 *        finite quotient rather than by enumeration.
 *
 * @details The extensional carrier @f$\mathbb{B}@f$ is the trivial case: its
 * two values @b are the quotient.  @c set materialises the predicate over
 * @c false and @c true into a @c FiniteBooleanSet, and @c Ø::operator== reads
 * off the two stored truths (@f$\varnothing@f$ iff neither is a member).  No
 * range, no @c begin/end walk: the proof is exhaustion of a 2-element quotient.
 * The infinite, periodic case (@c isEven factoring through @c Modular<2>) is
 * the same mechanism over @f$\mathbb{Z}/N\mathbb{Z}@f$; see the §3.1 exhibit.
 */
export template <typename L, typename P>
constexpr auto set(const UniversalSet<bool, L, Finite>&, P p) {
  return FiniteBooleanSet<L>{p(false), p(true)};
}

// A quantifier is one comparison of the comprehension @c set(S,P) against a
// lattice bound (Eqn 2): @c exists tests against @c ∅ (scheme A), @c forall
// against the input set @c S (scheme B); each recovers its partner by
// complementing the predicate, @c !P.  Generic over any @c IsSet domain @c S
// and @c IsPredicate query @c P for which a @c set() comprehension is defined;
// the per-carrier @c set() overloads (bool here, ℤ/Nℤ in :numbers) do the work.
export template <dedekind::category::IsSet S, dedekind::category::IsPredicate P>
  requires requires(const S& s, P p) { set(s, p); }
constexpr bool exists(const S& s, P p) {
  return !(Ø<typename S::Domain, typename S::logic_species>{} ==
           set(s, std::move(p)));  // (A): {x ∈ S | P(x)} ≠ ∅
}

export template <dedekind::category::IsSet S, dedekind::category::IsPredicate P>
  requires requires(const S& s, P p) { set(s, p); }
constexpr bool forall(const S& s, P p) {
  return set(s, std::move(p)) == s;  // (B): {x ∈ S | P(x)} == S
}

/** @section quantifier__Formal_Verification */

// The finite-quotient witnesses (Ω<bool> with a structural Singleton predicate,
// Ω<Cardinality> with a Congruence) live in :numbers, where the structural
// IsPredicate operands (dedekind.order Singleton, morphologies Congruence) are
// in scope.  Source for the §3.1 exhibit (lst:quantifiers_def).

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
 * @section quantifier__Extrema
 * @brief @c max / @c min --- the @b order analogue of the quantifiers ---
 * and @c argmax / @c argmin --- the same filter with @c ≤ pulled back
 * through a map @c f.
 *
 * @details Where @c exists / @c forall read a @b truth bound off a
 * comprehension (∅ / S; Eqn 2), @c max / @c min read an @b order bound.
 * @c max(s) is the greatest-element set
 * @f$\{x\in s \mid \forall a\in s.\; a\le x\}@f$ --- a comprehension whose
 * membership test is a bare @c forall over @c ≤ --- and @c min is its
 * @b order dual, exactly as @c forall is the @c ¬∃¬ dual of @c exists.
 * The result is itself a first-class @c IsSet, so membership is @c max(s)(x):
 * empty when there is no greatest element, a singleton for a total order,
 * larger under ties or a partial order.  @c argmax pulls @c ≤ back through
 * @c f, reading the order @f$f(a)\le f(x)@f$ that @c f induces on the domain,
 * with no inverse of @c f required; the result is the @b fibre
 * @f$f^{-1}(\max f(s))@f$, so unique-optimiser (a function) vs ties
 * (a relation) is just its cardinality.  Decidability is the quantifier's
 * own: a finite / finite-quotient domain settles the @c forall, else the
 * honest Rice wall at the call site (for @c argmax over an infinite domain,
 * the pluggable @b retract of §3.3.1).
 *
 * The domain is any @c IsEnumerableSet: a genuine set (it lifts to @c IsSet)
 * that is also @b walkable, since the dominance @c forall ranges over it.
 * A bare @c IsSet is membership-only and cannot be enumerated; the finite
 * exhibits use a lifted @c iota, which is both.
 */
export template <typename S>
concept IsEnumerableSet =
    std::ranges::forward_range<S> &&
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set(
        std::declval<S>()))>;

export template <IsEnumerableSet S>
constexpr auto max(S s) {
  using T = std::ranges::range_value_t<S>;
  auto dom = dedekind::category::ambient_set(s);  // the domain as an IsSet
  // { x ∈ s | ∀ a ∈ s. a ≤ x }
  auto dominates = [dom, s](const T& x) {
    return dom(x) && forall(s, [&x](const T& a) { return a <= x; });
  };
  return Set<T, typename decltype(dom)::logic_species, decltype(dominates)>{
      dominates};
}

export template <IsEnumerableSet S>
constexpr auto min(S s) {
  using T = std::ranges::range_value_t<S>;
  auto dom = dedekind::category::ambient_set(s);
  // { x ∈ s | ∀ a ∈ s. x ≤ a } --- the order dual
  auto dominated = [dom, s](const T& x) {
    return dom(x) && forall(s, [&x](const T& a) { return x <= a; });
  };
  return Set<T, typename decltype(dom)::logic_species, decltype(dominated)>{
      dominated};
}

export template <IsEnumerableSet S, typename F>
constexpr auto argmax(S s, F f) {
  using T = std::ranges::range_value_t<S>;
  auto dom = dedekind::category::ambient_set(s);
  // { x ∈ s | ∀ a ∈ s. f(a) ≤ f(x) }
  auto dominates = [dom, s, f](const T& x) {
    return dom(x) && forall(s, [&x, &f](const T& a) { return f(a) <= f(x); });
  };
  return Set<T, typename decltype(dom)::logic_species, decltype(dominates)>{
      dominates};
}

export template <IsEnumerableSet S, typename F>
constexpr auto argmin(S s, F f) {
  using T = std::ranges::range_value_t<S>;
  auto dom = dedekind::category::ambient_set(s);
  // { x ∈ s | ∀ a ∈ s. f(x) ≤ f(a) } --- the order dual
  auto dominated = [dom, s, f](const T& x) {
    return dom(x) && forall(s, [&x, &f](const T& a) { return f(x) <= f(a); });
  };
  return Set<T, typename decltype(dom)::logic_species, decltype(dominated)>{
      dominated};
}

/** @section quantifier__Formal_Verification_Extrema */

// max / min of the finite set [2,6) (a lifted iota): 5 is the greatest
// element, 2 the least.  Membership is the extremum test --- max(s)(x).
static_assert(max(std::views::iota(2, 6))(5), "5 = max [2,6).");
static_assert(!max(std::views::iota(2, 6))(4), "4 is not the greatest.");
static_assert(min(std::views::iota(2, 6))(2), "2 = min [2,6).");
static_assert(!min(std::views::iota(2, 6))(3), "3 is not the least.");

// argmax of the concave f(x) = x·(6−x) over [0,7): the unique peak is x = 3,
// so the fibre is the singleton {3} --- argmax is a function here.
static_assert(argmax(std::views::iota(0, 7),
                     [](int x) { return x * (6 - x); })(3),
              "3 = argmax x·(6−x): the unique optimiser (a function).");
static_assert(!argmax(std::views::iota(0, 7),
                      [](int x) { return x * (6 - x); })(2),
              "2 ∉ argmax x·(6−x).");
// A genuine tie: f(x) = x mod 2 is maximised by every odd argument, so the
// fibre {1,3,5} has more than one element --- argmax is a proper relation.
static_assert(argmax(std::views::iota(0, 7), [](int x) { return x % 2; })(1) &&
                  argmax(std::views::iota(0, 7),
                         [](int x) { return x % 2; })(5),
              "{1,3,5} ⊆ argmax (x mod 2): a tie (a proper relation).");
// argmin of the concave f sits at both ends: the fibre is {0,6}.
static_assert(argmin(std::views::iota(0, 7),
                     [](int x) { return x * (6 - x); })(0) &&
                  argmin(std::views::iota(0, 7),
                         [](int x) { return x * (6 - x); })(6),
              "{0,6} ⊆ argmin x·(6−x).");

}  // namespace dedekind::sets
