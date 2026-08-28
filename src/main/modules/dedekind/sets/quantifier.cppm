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

/**
 * @section quantifier__Extrema
 * @brief @c maximum / @c minimum --- the @b order analogue of the quantifiers.
 *
 * @details Where @c exists / @c forall filter a set and read a @b truth bound
 * (∅ / S; Eqn 2), @c maximum / @c minimum filter a set and read an @b order
 * bound.  @c maximum(s) is the greatest-element set
 * @f$\{x\in s \mid \forall x'\in s.\; x'\le x\}@f$ --- a comprehension whose
 * predicate is a @c ForAll over @c ≤ --- and @c minimum is its @b order dual
 * (reverse @c ≤), exactly as @c forall is the De Morgan dual @c ¬∃¬ of
 * @c exists.  The result is a @b set: empty when there is no greatest element,
 * a singleton for a total order's max, possibly larger on a partial order; the
 * @b value is the element of that singleton, when it exists.  Decidability is
 * the quantifier's own --- over a finite / finite-quotient carrier the
 * @c ForAll settles, otherwise it is the honest Rice wall at the call site.
 * Here @c ≤ is a bare comparison predicate; §3.3 reveals it as a @b relation
 * and re-spells these point-free.
 */
export template <std::ranges::viewable_range S>
constexpr auto maximum(S&& s) {
  // dominates(b) = ∀ a ∈ s. a ≤ b   (b is ≥ every element)
  auto dominates = ForAll(s, [](const auto& a, const auto& b) { return a <= b; });
  return set(std::forward<S>(s), std::move(dominates));
}
export template <std::ranges::viewable_range S>
constexpr auto minimum(S&& s) {
  // dominated(b) = ∀ a ∈ s. b ≤ a   (b is ≤ every element) --- the order dual
  auto dominated = ForAll(s, [](const auto& a, const auto& b) { return b <= a; });
  return set(std::forward<S>(s), std::move(dominated));
}

/**
 * @section quantifier__Argmax
 * @brief @c argmax / @c argmin --- the extremum with @c ≤ pulled back through a
 * function @c f.
 *
 * @details @c maximum reads the carrier's own order; @c argmax reads the order
 * @f$f(x')\le f(x)@f$ that @c f induces on its domain.  So @c argmax is
 * literally @c maximum with @c ≤ replaced by the pullback comparison, the
 * forward filter
 * @f$\arg\max_{\!f} := \{x\in \mathrm{dom} \mid \forall x'\in\mathrm{dom}.\;
 *   f(x')\le f(x)\}@f$, no inverse of @c f required.
 * Point-free (§3.3) this is the relation @c dom*dom filtered by
 * @f$f(\pi_1)\le f(\pi_2)@f$, universally quantified over @f$\pi_1@f$.
 * The result is the @b fibre @f$f^{-1}(\max f(\mathrm{dom}))@f$, the whole
 * domain-subset achieving the max, so its degenerate cases are just its
 * @b cardinality: @c ∅ (no maximum), a @b singleton (a unique optimiser ⟺
 * @c argmax is a function), a @b larger set (ties ⟺ @c argmax is a relation).
 * Decidability is again the quantifier's: finite / finite-quotient domain
 * settles; an infinite non-enumerable domain needs @c f's own structure ---
 * the pluggable @b retract of §3.3.1 --- else the honest Rice wall.
 */
export template <std::ranges::viewable_range S, typename F>
constexpr auto argmax(S&& s, F f) {
  // dominates_f(x) = ∀ x' ∈ s. f(x') ≤ f(x)
  auto dom_f = ForAll(s, [f](const auto& xp, const auto& x) { return f(xp) <= f(x); });
  return set(std::forward<S>(s), std::move(dom_f));
}
export template <std::ranges::viewable_range S, typename F>
constexpr auto argmin(S&& s, F f) {
  // dominated_f(x) = ∀ x' ∈ s. f(x) ≤ f(x')   --- the order dual
  auto dom_f = ForAll(s, [f](const auto& xp, const auto& x) { return f(x) <= f(xp); });
  return set(std::forward<S>(s), std::move(dom_f));
}

/** @section quantifier__Formal_Verification_Extrema */

// The sole element of a singleton extremum over a finite range (the extremum
// is a lazy filter_view, so it is walked, not queried via exists).
constexpr auto only_element = [](auto&& r) {
  int count = 0, value = 0;
  for (auto x : r) {
    ++count;
    value = x;
  }
  return std::pair{count, value};
};

// max / min of a finite integer range are its greatest / least element (as a
// singleton set): max [2,6) = {5}, min [2,6) = {2}.
static_assert(only_element(maximum(std::views::iota(2, 6))) == std::pair{1, 5},
              "max [2,6) = {5}.");
static_assert(only_element(minimum(std::views::iota(2, 6))) == std::pair{1, 2},
              "min [2,6) = {2}.");

// argmax of the concave f(x) = x·(6−x) over {0,…,6}: the unique peak is x = 3,
// so the fibre is the singleton {3} --- argmax is a function here.
static_assert(only_element(argmax(std::views::iota(0, 7),
                                  [](int x) { return x * (6 - x); })) ==
                  std::pair{1, 3},
              "argmax x·(6−x) over {0..6} = {3}.");
// A genuine tie: f(x) = x mod 2 is maximised by every odd x, so the fibre
// {1,3,5} has three elements --- argmax is a proper relation, not a function.
static_assert(only_element(argmax(std::views::iota(0, 7),
                                  [](int x) { return x % 2; })) ==
                  std::pair{3, 5},
              "argmax (x mod 2) over {0..6} = {1,3,5} (a tie).");
// argmin of the same concave f: the minima sit at both ends {0,6}, fibre size 2.
static_assert(only_element(argmin(std::views::iota(0, 7),
                                  [](int x) { return x * (6 - x); })) ==
                  std::pair{2, 6},
              "argmin x·(6−x) over {0..6} = {0,6}.");

}  // namespace dedekind::sets
