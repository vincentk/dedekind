/**
 * @file dedekind/category/periodic.cppm
 * @partition :periodic
 * @brief Periodic arrows on the counts and the counts they license: an arrow
 *        @c f is periodic with period @c p when @f$f(k + p) = f(k)@f$, a law
 *        declared at a generator and inferred at a composite, so that a count
 *        along a chain of any length is @c (n div p) periods and a remainder
 *        (the Jlt paper's tensor exhibit, #1104).
 *
 * @details Three readings of one idea, kept apart by name.  @c :total's
 *          @c is_periodic<T, Op> (@c IsPeriodic) says an @b operation wraps,
 *          @c (T, Op) cyclic, of order @c cyclic_order; @c morphologies'
 *          @c Modular<N> is the cyclic @b carrier @f$\mathbb{Z}/N\mathbb{Z}@f$;
 *          and here a @b sequence repeats, @f$f(k + p) = f(k)@f$, which is to
 *          say @c f factors through the quotient
 *          @f$\mathbb{N} \to \mathbb{Z}/p\mathbb{Z}@f$,
 *          @f$f = g \circ (k \mapsto k \bmod p)@f$: @c congruent(m, r) is
 *          that quotient followed by @c equals(r).  What periodicity needs is
 *          a translation, not an order: @c f has period @c p when it is
 *          invariant under the action of @c p, @f$f(k + p) = f(k)@f$, the
 *          counts acting on themselves by @c + (@c :action's
 *          @c acting_monoid<std::plus<std::size_t>>; the affine reading of
 *          a space acted on by its vectors is @c geometry:affine's).  The
 *          order enters with the extent, the bound below which the law
 *          holds, and with the counts by consecutive periods.  A law holds on
 *          the whole word because the counts themselves wrap only at the
 *          word's @c cyclic_order; a shift narrows it (@c extent).
 *
 *          The counts are any @c IsCountWord, @c std::size_t by default.  The
 *          period is a @b value the arrow carries, read by @c period(f), and
 *          @c HasPeriod<F> says only that @c F carries one: a period that is
 *          a type would leave nothing for a caller to choose, and the Python
 *          bindings choose it.  @c opaque(f) hides the law, so that the plain
 *          fold runs where a test checks the law against it.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>  // std::ranges::fold_left
#include <array>
#include <bit>     // std::bit_width: the bits fast doubling reads; std::rotl
#include <bitset>  // the bit words wider than a machine word
#include <concepts>
#include <cstddef>
#include <cstdint>  // the bit words a machine word holds
#include <cstdlib>  // std::abort: a modulus of 0 is refused
#include <functional>
#include <iterator>  // std::permutable: the words a symmetry reorders
#include <limits>    // the extent of a law that holds on the whole word
#include <numeric>
#include <optional>  // std::lcm: the period of a join or a meet
#include <ranges>
#include <string_view>  // the letters of an alphabet, as written
#include <tuple>        // a letter and its image, zipped
#include <type_traits>
#include <utility>
#include <vector>  // the residue histograms of a modular section

export module dedekind.category:periodic;

import :morphism;              // IsArrow, Dom, Cod, Compose
import :chain;                 // the counting chain, IsIntegerChain
import :nno;                   // Prior: the default chain's step
import :discrete;              // equals: an arrow into the truth values
import :lattice;               // Meet, Join: the nodes & and | build
import :cartesian_bicategory;  // Tensor, Merge: the outer product
import :involution;            // Reverse, commutes: the symmetries of a word
import :posetal;               // is_involution: a law asked on the value

namespace dedekind::category {

/** @brief An arrow on the counts with a declared period, read by
 *  @c period(f): @f$f(k + p) = f(k)@f$ for every @c k.  @tparam F the arrow. */
export template <typename F>
concept HasPeriod = IsArrow<F> && IsCountWord<Dom<F>> && requires(const F& f) {
  { period(f) } -> std::same_as<Dom<F>>;
};

/** @brief Inferred, not declared: an arrow after a periodic one is periodic
 *  with the same period, whatever the second arrow is.
 *  @param c the composite.  @return the first leg's period. */
export template <HasPeriod F, IsArrow G>
constexpr Dom<F> period(const Compose<F, G>& c) {
  return period(c.f);
}

/** @brief Where a period's law holds: @f$f(k + p) = f(k)@f$ for every
 *  @c k with @c k + p below the extent.  A generator's law holds on the whole
 *  word; a shift narrows it, since the word does not wrap for the law.
 *  @param f the arrow.  @return the extent. */
export template <HasPeriod F>
constexpr Dom<F> extent(const F&) {
  return std::numeric_limits<Dom<F>>::max();
}
/** @param c the composite.  @return the first leg's extent. */
export template <HasPeriod F, IsArrow G>
constexpr Dom<F> extent(const Compose<F, G>& c) {
  return extent(c.f);
}

namespace detail_periodic {
/** @brief Refuses a modulus of 0, which has no residues: not a constant
 *  expression at compile time, the end of the program at run time. */
[[noreturn]] constexpr void refuse_zero_modulus() { std::abort(); }
/** @brief Refuses a count past the extent of the law it would use. */
[[noreturn]] constexpr void refuse_beyond_extent() { std::abort(); }
/** @brief Refuses a period the word cannot hold. */
[[noreturn]] constexpr void refuse_unrepresentable_period() { std::abort(); }
/** @brief Refuses a power of two the word cannot hold. */
[[noreturn]] constexpr void refuse_beyond_word() { std::abort(); }
/** @brief Refuses a modulus past the square root of the word's order, whose
 *  residues' products would leave the word. */
[[noreturn]] constexpr void refuse_wide_modulus() { std::abort(); }
/** @brief The least common multiple of two periods, refused where the word
 *  cannot hold it, which @c std::lcm leaves undefined.  @param a @param b
 *  the periods, positive.  @return their lcm. */
template <IsCountWord N>
constexpr N checked_lcm(N a, N b) {
  const N g = std::gcd(a, b);
  const N q = static_cast<N>(a / g);
  if (q > std::numeric_limits<N>::max() / b) refuse_unrepresentable_period();
  return static_cast<N>(q * b);
}
/** @brief A chain of the counts under an arrow. */
template <IsCountWord N, IsArrow F>
using Along = unfold_view<N, Mapped<N, StepCoalgebra<N, Prior<N>>, F>>;
}  // namespace detail_periodic

/** @brief The residue class @f$\{k \mid k \equiv r \pmod m\}@f$ as a
 *  predicate on the counts, periodic with period @c m: the quotient onto
 *  @c Modular<m>'s carrier, then the test for @c r.  @tparam N the counts. */
export template <IsCountWord N = std::size_t>
struct Congruent {
  using Domain = N;
  using Codomain = bool;
  N modulus{1};
  N residue{0};
  /** @param k a count.  @return whether @c k is congruent to the residue. */
  constexpr bool operator()(N k) const { return k % modulus == residue; }
  constexpr bool operator==(const Congruent&) const = default;
};
/** @param c the residue class.  @return its modulus. */
export template <IsCountWord N>
constexpr N period(const Congruent<N>& c) {
  return c.modulus;
}
/** @brief The residue class of @c r modulo @c m.  @tparam N the counts.
 *  @param m the modulus, positive.  @param r the residue, taken modulo @c m. */
export template <IsCountWord N = std::size_t>
constexpr Congruent<N> congruent(std::type_identity_t<N> m,
                                 std::type_identity_t<N> r) {
  if (m == 0) detail_periodic::refuse_zero_modulus();
  return {m, static_cast<N>(r % m)};
}
/** @brief The even counts, the residue class of 0 modulo 2.  Its reading on
 *  ℕ as a sequence, a path into the truth values, is
 *  @c dedekind::sequences::is_even (@c sequences:samples). */
export inline constexpr Congruent<> is_even{2, 0};

namespace detail_periodic {
/** @brief One step of fast doubling, @f$(F(j), F(j+1))@f$ modulo @c m to
 *  @f$(F(2j), F(2j+1))@f$ or @f$(F(2j+1), F(2j+2))@f$ by bit @c i of @c k:
 *  @f$F(2j) = F(j)(2F(j+1) - F(j))@f$, @f$F(2j+1) = F(j)^2 + F(j+1)^2@f$. */
template <IsCountWord N>
struct Double {
  N k;
  N m;
  N w;  // the bits of k, read from the top: step i reads bit w-1-i
  constexpr std::array<N, 2> operator()(std::array<N, 2> p, N i) const {
    const N a = p[0], b = p[1];
    const N c = static_cast<N>(a * ((2 * b + m - a) % m) % m);
    const N d = static_cast<N>((a * a % m + b * b % m) % m);
    return ((k >> (w - 1 - i)) & 1U) != 0
               ? std::array<N, 2>{d, static_cast<N>((c + d) % m)}
               : std::array<N, 2>{c, d};
  }
};
/** @brief One step of the search for the Pisano period: the pair
 *  @f$(F(j), F(j+1))@f$ modulo @c m, and the least @c j at which it is
 *  @f$(0, 1)@f$ again, once found. */
template <IsCountWord N>
struct Pisano {
  N m;
  constexpr std::array<N, 3> operator()(std::array<N, 3> s, N j) const {
    if (s[2] != 0) return s;
    const N a = s[1], b = static_cast<N>((s[0] + s[1]) % m);
    return {a, b, (a == 0 && b == 1 % m) ? j : N{0}};
  }
};
}  // namespace detail_periodic

namespace detail_periodic {
/** @brief A modulus for the Fibonacci numbers in the word @c N: positive and
 *  at most @f$2^{w/2}@f$ for a word of @c w bits, so that a product of two
 *  residues, below @f$m^2@f$, and the Pisano bound @f$6m@f$ stay in the word;
 *  refused otherwise.  @param m the modulus.  @return @c m. */
template <IsCountWord N>
constexpr N fibonacci_modulus(N m) {
  if (m == 0) refuse_zero_modulus();
  if (m > (N{1} << (std::numeric_limits<N>::digits / 2))) {
    refuse_wide_modulus();
  }
  return m;
}
}  // namespace detail_periodic

/** @brief The Fibonacci numbers modulo @c m, @f$k \mapsto F(k) \bmod m@f$,
 *  by fast doubling, @f$\log k@f$ steps; the modulus is at most the square
 *  root of the word's order, so that the products stay in the word, and a
 *  wider one is refused where the arrow is read.  @tparam N the counts. */
export template <IsCountWord N = std::size_t>
struct FibonacciMod {
  using Domain = N;
  using Codomain = N;
  N modulus{1};
  /** @param k a count.  @return @f$F(k) \bmod m@f$. */
  constexpr N operator()(N k) const {
    const N m = detail_periodic::fibonacci_modulus(modulus);
    const auto w = static_cast<N>(std::bit_width(k));
    return std::ranges::fold_left(std::views::iota(N{0}, w),
                                  std::array<N, 2>{0, static_cast<N>(1 % m)},
                                  detail_periodic::Double<N>{k, m, w})[0];
  }
  constexpr bool operator==(const FibonacciMod&) const = default;
};
/** @brief The Pisano period @f$\pi(m)@f$, the period of the Fibonacci
 *  numbers modulo @c m, a theorem: it exists and is at most @f$6m@f$, so a
 *  search of that length finds it.  @param f the arrow.  @return its period. */
export template <IsCountWord N>
constexpr N period(const FibonacciMod<N>& f) {
  const N m = detail_periodic::fibonacci_modulus(f.modulus);
  return std::ranges::fold_left(
      std::views::iota(N{1}, static_cast<N>(6 * m + 1)),
      std::array<N, 3>{0, static_cast<N>(1 % m), 0},
      detail_periodic::Pisano<N>{m})[2];
}
/** @brief The Fibonacci numbers modulo @c m.  @tparam N the counts.
 *  @param m the modulus, positive and at most @f$2^{w/2}@f$ for a word of
 *  @c w bits. */
export template <IsCountWord N = std::size_t>
constexpr FibonacciMod<N> fibonacci_mod(std::type_identity_t<N> m) {
  return {detail_periodic::fibonacci_modulus<N>(m)};
}

/** @brief An arrow with its laws hidden: called as @c f, with no period, so
 *  that a count along it is the plain fold.  What a test runs beside the law
 *  to check it.  @tparam F the arrow. */
export template <IsArrow F>
struct Opaque {
  using Domain = Dom<F>;
  using Codomain = Cod<F>;
  F f;
  constexpr Codomain operator()(const Domain& x) const { return f(x); }
};
/** @param f the arrow.  @return @c f, its laws hidden. */
export template <IsArrow F>
constexpr Opaque<F> opaque(F f) {
  return {f};
}

namespace detail_periodic {
/** @brief The Iverson bracket, @c bool to the counts, a semiring map. */
template <IsCountWord N>
struct Iverson {
  constexpr N operator()(N m, bool b) const {
    return static_cast<N>(m + (b ? 1 : 0));
  }
};
}  // namespace detail_periodic

/** @brief The count of a range of truth values: the plain fold.
 *  @tparam N the counts.  @param r the range.  @return how many are true. */
export template <IsCountWord N = std::size_t, std::ranges::input_range R>
  requires std::convertible_to<std::ranges::range_value_t<R>, bool>
constexpr N count(R r) {
  return std::ranges::fold_left(std::move(r), N{0},
                                detail_periodic::Iverson<N>{});
}

namespace detail_periodic {
/** @brief The plain count of @c f(0), ..., @c f(m - 1).  @param m how many.
 *  @param f the predicate. */
template <IsCountWord N, IsArrow F>
constexpr N count_below(N m, const F& f) {
  return count<N>(m >> chain<N>() >> opaque(f));
}
}  // namespace detail_periodic
/** @brief The count along the chain of @c n under a periodic predicate:
 *  @c (n div p) periods and a remainder, by periodicity; the chain's order
 *  does not matter, since the counts commute.  More specialised than the
 *  plain fold, so resolution prefers it.  @param v the chain under the
 *  predicate.  @return how many of @c f(0), ..., @c f(n - 1) are true. */
export template <IsCountWord N, HasPeriod F>
  requires std::same_as<Dom<F>, N> && std::convertible_to<Cod<F>, bool>
constexpr N count(const detail_periodic::Along<N, F>& v) {
  const F f = v.coalgebra().f;
  const N n = v.seed(), p = period(f);
  if (n > extent(f)) detail_periodic::refuse_beyond_extent();
  if (n < p) return detail_periodic::count_below(n, f);  // less than a period
  return static_cast<N>((n / p) * detail_periodic::count_below(p, f) +
                        detail_periodic::count_below(static_cast<N>(n % p), f));
}

/** @brief The count along a chain restricted by value to @c [lo, hi) under a
 *  periodic predicate: the law's count below @c hi less the law's count below
 *  @c lo, two calls whatever the bounds.  @param v the restricted chain under
 *  the predicate.  @return how many of @c f(lo), ..., @c f(hi - 1) are
 *  true. */
export template <IsCountWord N, HasPeriod F>
  requires std::same_as<Dom<F>, N> && std::convertible_to<Cod<F>, bool>
constexpr N count(
    const unfold_view<std::optional<N>, detail_chain::SliceStep<N, F>>& v) {
  const auto step = v.coalgebra();
  if (!v.seed()) return N{0};  // a default view: empty, as the plain fold
  const N lo = *v.seed(), hi = step.hi;
  if (lo >= hi) return N{0};
  return static_cast<N>(count(hi >> chain<N>() >> step.f) -
                        count(lo >> chain<N>() >> step.f));
}

/** @brief The sum of a range of counts: the plain fold.  @param r the range.
 *  @return the sum, modulo the word. */
export template <std::ranges::input_range R>
  requires IsCountWord<std::ranges::range_value_t<R>>
constexpr std::ranges::range_value_t<R> sum(R r) {
  using N = std::ranges::range_value_t<R>;
  return std::ranges::fold_left(std::move(r), N{0}, std::plus<N>{});
}
namespace detail_periodic {
/** @brief The plain sum of @c f(0), ..., @c f(m - 1).  @param m how many.
 *  @param f the arrow. */
template <IsCountWord N, IsArrow F>
constexpr N sum_below(N m, const F& f) {
  return sum(m >> chain<N>() >> opaque(f));
}
}  // namespace detail_periodic
/** @brief The sum along the chain of @c n under a periodic arrow into the
 *  counts: @c (n div p) copies of one period's sum, one multiplication by the
 *  semimodule law, and a remainder.  @param v the chain under the arrow.
 *  @return @c f(0) + ... + @c f(n - 1), modulo the word. */
export template <IsCountWord N, HasPeriod F>
  requires std::same_as<Dom<F>, N> && std::same_as<Cod<F>, N>
constexpr N sum(const detail_periodic::Along<N, F>& v) {
  const F f = v.coalgebra().f;
  const N n = v.seed(), p = period(f);
  if (n > extent(f)) detail_periodic::refuse_beyond_extent();
  if (n < p) return detail_periodic::sum_below(n, f);  // less than a period
  return static_cast<N>((n / p) * detail_periodic::sum_below(p, f) +
                        detail_periodic::sum_below(static_cast<N>(n % p), f));
}

/** @brief A translation of the counts, @f$k \mapsto k + j@f$: after it, a
 *  periodic arrow keeps its period, on an extent narrowed by @c j.
 *  @tparam N the counts. */
export template <IsCountWord N = std::size_t>
struct Shift {
  using Domain = N;
  using Codomain = N;
  N by{0};
  /** @param k a count.  @return @c k + by. */
  constexpr N operator()(N k) const { return static_cast<N>(k + by); }
  constexpr bool operator==(const Shift&) const = default;
};
/** @tparam N the counts.  @param j the translation.  @return
 *  @f$k \mapsto k + j@f$. */
export template <IsCountWord N = std::size_t>
constexpr Shift<N> shift(std::type_identity_t<N> j) {
  return {j};
}
/** @param c a translation, then a periodic arrow.  @return its period. */
export template <IsCountWord N, HasPeriod G>
constexpr N period(const Compose<Shift<N>, G>& c) {
  return period(c.g);
}
/** @param c a translation, then a periodic arrow.  @return the second leg's
 *  extent, less the translation. */
export template <IsCountWord N, HasPeriod G>
constexpr N extent(const Compose<Shift<N>, G>& c) {
  const N e = extent(c.g);
  return e > c.f.by ? static_cast<N>(e - c.f.by) : N{0};
}

namespace detail_periodic {
/** @brief A periodic predicate on the counts. */
template <typename P>
concept IsPeriodicPredicate = HasPeriod<P> && std::same_as<Cod<P>, bool>;
}  // namespace detail_periodic
/** @brief The join of two periodic predicates, the lattice's @c Join node:
 *  periodic with the least common multiple of their periods.
 *  @param p @param q the predicates.  @return @c p or @c q. */
export template <detail_periodic::IsPeriodicPredicate P,
                 detail_periodic::IsPeriodicPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>>
constexpr Join<P, Q> operator|(P p, Q q) {
  return {std::move(p), std::move(q)};
}
/** @brief The meet, dual to the join.  @param p @param q the predicates.
 *  @return @c p and @c q. */
export template <detail_periodic::IsPeriodicPredicate P,
                 detail_periodic::IsPeriodicPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>>
constexpr Meet<P, Q> operator&(P p, Q q) {
  return {std::move(p), std::move(q)};
}
/** @param j the join.  @return the lcm of its legs' periods. */
export template <HasPeriod P, HasPeriod Q>
constexpr Dom<P> period(const Join<P, Q>& j) {
  return detail_periodic::checked_lcm(period(j.lhs), period(j.rhs));
}
/** @param m the meet.  @return the lcm of its legs' periods. */
export template <HasPeriod P, HasPeriod Q>
constexpr Dom<P> period(const Meet<P, Q>& m) {
  return detail_periodic::checked_lcm(period(m.lhs), period(m.rhs));
}
/** @param j the join.  @return the lesser of its legs' extents. */
export template <HasPeriod P, HasPeriod Q>
constexpr Dom<P> extent(const Join<P, Q>& j) {
  return std::min(extent(j.lhs), extent(j.rhs));
}
/** @param m the meet.  @return the lesser of its legs' extents. */
export template <HasPeriod P, HasPeriod Q>
constexpr Dom<P> extent(const Meet<P, Q>& m) {
  return std::min(extent(m.lhs), extent(m.rhs));
}

/** @param n the complement of a periodic predicate.  @return its period:
 *  negation reads one value, so keeps the period. */
export template <HasPeriod P>
constexpr Dom<P> period(const Not<P>& n) {
  return period(n.base);
}
/** @param n the complement of a periodic predicate.  @return its extent. */
export template <HasPeriod P>
constexpr Dom<P> extent(const Not<P>& n) {
  return extent(n.base);
}

namespace detail_periodic {
/** @brief The window's step: how many are true. */
template <IsCountWord N>
struct CountStep {
  constexpr N operator()(N m, bool b) const {
    return static_cast<N>(m + (b ? 1 : 0));
  }
};
/** @brief The window's step: whether any is true. */
struct AnyStep {
  constexpr bool operator()(bool a, bool b) const { return a || b; }
};
/** @brief One step of a window at @c k: the step applied to @c f(k + j). */
template <IsArrow F, typename Step>
struct Window {
  F f;
  Dom<F> k;
  template <std::semiregular Acc>
  constexpr Acc operator()(Acc acc, Dom<F> j) const {
    return Step{}(acc, f(static_cast<Dom<F>>(k + j)));
  }
};
}  // namespace detail_periodic
/** @brief A window over a periodic predicate, SQL's window function over
 *  @c ROWS @c BETWEEN @c CURRENT @c ROW @c AND @c w - 1 @c FOLLOWING:
 *  @f$k \mapsto@f$ the step folded over @f$f(k), \ldots, f(k + w - 1)@f$.  A
 *  finite combination of translations, so periodic with the same period.
 *  @tparam F the predicate.  @tparam Step the fold.  @tparam Acc its result. */
export template <detail_periodic::IsPeriodicPredicate F, std::semiregular Step,
                 std::semiregular Acc>
struct Rolling {
  using Domain = Dom<F>;
  using Codomain = Acc;
  F f;
  Dom<F> width{1};
  /** @param k a count.  @return the window's fold at @c k. */
  constexpr Acc operator()(Dom<F> k) const {
    return std::ranges::fold_left(std::views::iota(Dom<F>{0}, width), Acc{},
                                  detail_periodic::Window<F, Step>{f, k});
  }
};
/** @param r the window.  @return the predicate's period. */
export template <detail_periodic::IsPeriodicPredicate F, std::semiregular Step,
                 std::semiregular Acc>
constexpr Dom<F> period(const Rolling<F, Step, Acc>& r) {
  return period(r.f);
}
/** @param r the window.  @return the predicate's extent, less the window; an
 *  empty window reads nothing, so keeps the whole extent. */
export template <detail_periodic::IsPeriodicPredicate F, std::semiregular Step,
                 std::semiregular Acc>
constexpr Dom<F> extent(const Rolling<F, Step, Acc>& r) {
  const Dom<F> e = extent(r.f);
  if (r.width == 0) return e;
  return e >= r.width ? static_cast<Dom<F>>(e - (r.width - 1)) : Dom<F>{0};
}
/** @brief How many of @c w consecutive values are true, SQL's
 *  @c COUNT @c OVER a window.  @param f the predicate.  @param w the width. */
export template <detail_periodic::IsPeriodicPredicate F>
constexpr Rolling<F, detail_periodic::CountStep<Dom<F>>, Dom<F>> rolling_count(
    F f, std::type_identity_t<Dom<F>> w) {
  return {std::move(f), w};
}
/** @brief Whether any of @c w consecutive values is true.  @param f the
 *  predicate.  @param w the width. */
export template <detail_periodic::IsPeriodicPredicate F>
constexpr Rolling<F, detail_periodic::AnyStep, bool> rolling_any(
    F f, std::type_identity_t<Dom<F>> w) {
  return {std::move(f), w};
}

// --- The outer product of predicates along chains, and its counts ----------
namespace detail_periodic {
/** @brief The meet of a pair of truth values: the spider's merge on 𝔹. */
using Both = Merge<bool, std::logical_and<bool>>;
/** @brief An entry of an outer product: the legs' predicates in parallel,
 *  then their meet, @c Tensor followed by @c Merge. */
template <typename T>
struct box_node : std::false_type {};
template <typename A, typename B>
struct box_node<Compose<Tensor<A, B>, Both>> : std::true_type {
  using left = A;
  using right = B;
};
/** @brief How many legs an entry has. */
template <typename T>
struct box_legs : std::integral_constant<std::size_t, 1> {};
template <typename A, typename B>
struct box_legs<Compose<Tensor<A, B>, Both>>
    : std::integral_constant<std::size_t,
                             box_legs<A>::value + box_legs<B>::value> {};
}  // namespace detail_periodic

/** @brief The outer product of predicates along chains, @f$T(i_1, \ldots,
 *  i_K) = f_1(i_1) \wedge \cdots \wedge f_K(i_K)@f$ over the box of all
 *  indices, a rank-one tensor never built: its entry is the legs' @c Tensor
 *  followed by the merge, and the box's sides are the chains' lengths.  In
 *  the pictures of tensor networks (Bridgeman and Chubb, "Hand-waving and
 *  Interpretive Dance: An Introductory Course on Tensor Networks", J. Phys. A
 *  50, 2017), each count contracts a leg's index against a covector: all ones
 *  for the box, a residue class for a section.
 *  @tparam T the entry.  @tparam K the number of legs.  @tparam N the counts.
 */
export template <IsArrow T, std::size_t K, IsCountWord N = std::size_t>
struct Box {
  T entry;
  std::array<N, K> sides;
};
/** @param a @param b two chains under predicates.  @return their outer
 *  product. */
export template <IsCountWord N, IsArrow F, IsArrow G>
  requires std::same_as<Cod<F>, bool> && std::same_as<Cod<G>, bool>
constexpr auto operator*(const detail_periodic::Along<N, F>& a,
                         const detail_periodic::Along<N, G>& b) {
  using E = Compose<Tensor<F, G>, detail_periodic::Both>;
  return Box<E, 2, N>{E{Tensor<F, G>{a.coalgebra().f, b.coalgebra().f},
                        detail_periodic::Both{}},
                      {a.seed(), b.seed()}};
}
/** @param x an outer product.  @param b one more chain under a predicate.
 *  @return their outer product. */
export template <IsArrow T, std::size_t K, IsCountWord N, IsArrow G>
  requires std::same_as<Cod<G>, bool>
constexpr auto operator*(const Box<T, K, N>& x,
                         const detail_periodic::Along<N, G>& b) {
  using E = Compose<Tensor<T, G>, detail_periodic::Both>;
  std::array<N, K + 1> sides{};
  std::ranges::copy(x.sides, sides.begin());
  sides[K] = b.seed();
  return Box<E, K + 1, N>{
      E{Tensor<T, G>{x.entry, b.coalgebra().f}, detail_periodic::Both{}},
      sides};
}

namespace detail_periodic {
/** @brief Two arrays, one after the other. */
template <IsCountWord N, std::size_t A, std::size_t B>
constexpr std::array<N, A + B> join_arrays(const std::array<N, A>& a,
                                           const std::array<N, B>& b) {
  std::array<N, A + B> out{};
  std::ranges::copy(a, out.begin());
  std::ranges::copy(b, out.begin() + A);
  return out;
}
/** @brief Each leg's count, the legs read left to right from @c at. */
template <IsArrow T, std::size_t K, IsCountWord N>
constexpr std::array<N, box_legs<T>::value> counts_of(
    const T& t, const std::array<N, K>& sides, std::size_t at) {
  if constexpr (box_node<T>::value) {
    using L = typename box_node<T>::left;
    return join_arrays(counts_of(t.f.r, sides, at),
                       counts_of(t.f.s, sides, at + box_legs<L>::value));
  } else {
    return {count<N>(sides[at] >> chain<N>() >> t)};
  }
}
/** @brief One residue's count on a leg, appended to its histogram. */
template <IsArrow T, IsCountWord N>
struct Residue {
  T t;
  N n;
  N m;
  constexpr std::vector<N> operator()(std::vector<N> h, N r) const {
    h.push_back(count<N>(n >> chain<N>() >>
                         Meet<T, Congruent<N>>{t, congruent<N>(m, r)}));
    return h;
  }
};
/** @brief Each leg's histogram over the residues modulo @c m, the legs read
 *  left to right from @c at. */
template <IsArrow T, std::size_t K, IsCountWord N>
constexpr std::vector<std::vector<N>> residues_of(const T& t,
                                                  const std::array<N, K>& sides,
                                                  std::size_t at, N m) {
  if constexpr (box_node<T>::value) {
    using L = typename box_node<T>::left;
    auto hs = residues_of(t.f.r, sides, at, m);
    const auto rs = residues_of(t.f.s, sides, at + box_legs<L>::value, m);
    hs.insert(hs.end(), rs.begin(), rs.end());
    return hs;
  } else {
    return {std::ranges::fold_left(std::views::iota(N{0}, m), std::vector<N>{},
                                   Residue<T, N>{t, sides[at], m})};
  }
}
/** @brief One term of a cyclic convolution: the residues @c i and @c j of
 *  two histograms add to their sum modulo @c m. */
template <IsCountWord N>
struct CyclicTerm {
  std::vector<N> a;
  std::vector<N> b;
  N m;
  N i;
  constexpr std::vector<N> operator()(std::vector<N> c, N j) const {
    const std::size_t at = (static_cast<std::size_t>(i) + j) % m;
    c[at] = static_cast<N>(c[at] + a[i] * b[j]);
    return c;
  }
};
/** @brief One row of a cyclic convolution: residue @c i of the first
 *  histogram against every residue of the second. */
template <IsCountWord N>
struct CyclicRow {
  std::vector<N> a;
  std::vector<N> b;
  N m;
  constexpr std::vector<N> operator()(std::vector<N> c, N i) const {
    return std::ranges::fold_left(std::views::iota(N{0}, m), std::move(c),
                                  CyclicTerm<N>{a, b, m, i});
  }
};
/** @brief The cyclic convolution of the histogram so far with one more leg's,
 *  in the counts' semiring, the ring of polynomials modulo @f$x^m - 1@f$. */
template <IsCountWord N>
struct Convolve {
  N m;
  constexpr std::vector<N> operator()(const std::vector<N>& acc,
                                      const std::vector<N>& h) const {
    return std::ranges::fold_left(std::views::iota(N{0}, m),
                                  std::vector<N>(m, N{0}),
                                  CyclicRow<N>{acc, h, m});
  }
};
}  // namespace detail_periodic

/** @brief Each leg's count over its chain, the factors of the box's count.
 *  @param b the outer product.  @return the counts, leg by leg. */
export template <IsArrow T, std::size_t K, IsCountWord N>
constexpr std::array<N, K> count_factors(const Box<T, K, N>& b) {
  return detail_periodic::counts_of(b.entry, b.sides, 0);
}
/** @brief The count over the box, SQL's @c COUNT(*) over a cross join: by
 *  distributivity the product of the legs' counts, contracting the rank-one
 *  tensor against the all-ones covector on each leg.  Exact beyond the word
 *  only as its factors (@c count_factors); this is its residue.
 *  @param b the outer product.  @return the count, modulo the word. */
export template <IsArrow T, std::size_t K, IsCountWord N>
constexpr N count(const Box<T, K, N>& b) {
  return std::ranges::fold_left(count_factors(b), N{1}, std::multiplies<N>{});
}
/** @brief The count over the section @f$i_1 + \cdots + i_K \equiv r
 *  \pmod m@f$ of the box, SQL's @c GROUP @c BY a computed key: the cyclic
 *  convolution of the legs' histograms over the residues, @c m numbers a
 *  leg, in the counts' semiring.  @param b the outer product.  @param m the
 *  modulus, positive.  @param r the residue.  @return the count, modulo the
 *  word. */
export template <IsArrow T, std::size_t K, IsCountWord N>
constexpr N count_section(const Box<T, K, N>& b, std::type_identity_t<N> m,
                          std::type_identity_t<N> r) {
  if (m == 0) detail_periodic::refuse_zero_modulus();
  std::vector<N> unit(m, N{0});
  unit[0] = 1;
  return std::ranges::fold_left(
      detail_periodic::residues_of(b.entry, b.sides, 0, m), unit,
      detail_periodic::Convolve<N>{m})[r % m];
}

/** @section periodic__Tensor_Powers
 *  @brief Words over an alphabet as a tensor, not as a sequence: the tensor
 *  power @f$\Sigma^n@f$ of a filtered alphabet, its symmetric sections, and
 *  the free monoid on it truncated below a length, each counted as a product
 *  of factors that never overflow where the total does.  Periodicity above is
 *  invariance under a translation of the counts; a symmetry here is
 *  invariance under a permutation of the positions of a word. */

/** @brief A factor of a count, @f$b^e@f$: a base raised to an exponent.
 *  @tparam N the counts. */
export template <IsCountWord N = std::size_t>
struct Factor {
  N base;
  N exponent;
  constexpr bool operator==(const Factor&) const = default;
};
/** @brief The cycles of a permutation of positions of one length, and how
 *  many there are: one term of its cycle type.  @tparam N the counts. */
export template <IsCountWord N = std::size_t>
struct Cycles {
  N length;
  N count;
  constexpr bool operator==(const Cycles&) const = default;
};

namespace detail_periodic {
/** @brief One letter into a packed set of bytes. */
struct SetByte {
  constexpr std::bitset<256> operator()(std::bitset<256> m, char c) const {
    return m.set(static_cast<unsigned char>(c));
  }
};
/** @brief One letter and its image into a table on the bytes. */
struct MapByte {
  constexpr std::pair<std::array<std::byte, 256>, std::size_t> operator()(
      std::pair<std::array<std::byte, 256>, std::size_t> s,
      std::tuple<char, char> p) const {
    s.first[static_cast<unsigned char>(std::get<0>(p))] =
        std::byte{static_cast<unsigned char>(std::get<1>(p))};
    return s;
  }
};
}  // namespace detail_periodic

/** @brief A subobject of the bytes given by its letters, @f$\chi : \Sigma
 *  \to \Omega@f$ as a packed word of 256 bits: the predicate a filtered
 *  alphabet is built from, @c letters("ACGT"). */
export struct Letters {
  using Domain = std::byte;
  using Codomain = bool;
  std::bitset<256> members;
  constexpr bool operator()(std::byte b) const {
    return members.test(std::to_integer<unsigned char>(b));
  }
  constexpr bool operator==(const Letters&) const = default;
};
/** @param alphabet the letters, as written.  @return the predicate that
 *  keeps them. */
export constexpr Letters letters(std::string_view alphabet) {
  return {std::ranges::fold_left(alphabet, std::bitset<256>{},
                                 detail_periodic::SetByte{})};
}
/** @brief An arrow on the bytes given by a table, each letter of @c from to
 *  the letter at the same place of @c to, every other byte fixed:
 *  @c letter_map("ACGT", "TGCA") is the complement of a base, an
 *  involution. */
export struct LetterMap {
  using Domain = std::byte;
  using Codomain = std::byte;
  std::array<std::byte, 256> table;
  constexpr std::byte operator()(std::byte b) const {
    return table[std::to_integer<unsigned char>(b)];
  }
  constexpr bool operator==(const LetterMap&) const = default;
};
namespace detail_periodic {
/** @brief One byte into the identity table. */
struct FixByte {
  constexpr std::array<std::byte, 256> operator()(std::array<std::byte, 256> t,
                                                  std::size_t i) const {
    t[i] = std::byte{static_cast<unsigned char>(i)};
    return t;
  }
};
/** @brief The identity table on the bytes. */
constexpr std::array<std::byte, 256> identity_table() {
  return std::ranges::fold_left(
      std::views::iota(std::size_t{0}, std::size_t{256}),
      std::array<std::byte, 256>{}, FixByte{});
}
/** @brief Refuses a letter map whose letters and images differ in number. */
[[noreturn]] constexpr void refuse_unequal_letters() { std::abort(); }
}  // namespace detail_periodic
/** @param from the letters moved.  @param to their images, in the same
 *  order, as many.  @return the arrow. */
export constexpr LetterMap letter_map(std::string_view from,
                                      std::string_view to) {
  if (from.size() != to.size()) detail_periodic::refuse_unequal_letters();
  return {std::ranges::fold_left(
              std::views::zip(from, to),
              std::pair{detail_periodic::identity_table(), std::size_t{0}},
              detail_periodic::MapByte{})
              .first};
}

/** @brief The tensor power @f$\Sigma^n@f$: the words of length @c n over the
 *  letters of a chain that a predicate keeps, a box of @c n legs, each the
 *  filtered alphabet, never built.  @tparam C the letters.  @tparam F the
 *  predicate.  @tparam N the counts. */
export template <HasNNOStep C, IsArrow F, IsCountWord N = std::size_t>
  requires std::totally_ordered<C> && std::same_as<Dom<F>, C> &&
           std::convertible_to<Cod<F>, bool>
struct Power {
  Slice<C> letters;
  F member;
  N length;
};
/** @param leg a chain of letters under a predicate, the filtered alphabet.
 *  @param n the length.  @return the words of length @c n over it. */
export template <IsCountWord N = std::size_t, HasNNOStep C, IsArrow F>
  requires std::totally_ordered<C> && std::same_as<Dom<F>, C> &&
           std::convertible_to<Cod<F>, bool>
constexpr Power<C, F, N> power(
    const unfold_view<std::optional<C>, detail_chain::SliceStep<C, F>>& leg,
    std::type_identity_t<N> n) {
  const auto step = leg.coalgebra();
  return {slice(leg.seed().value_or(step.hi), step.hi), step.f, n};
}

/** @brief The tensor power of an arrow on the letters, @f$\tau^{\times n}@f$:
 *  @c tau applied to every letter of a word, the functor @f$(-)^n@f$ on
 *  arrows beside @c power on the alphabet, Listing 4's power one level up,
 *  @c length copies of @c tau under @f$\times@f$.  It commutes with every
 *  permutation of the positions, which is naturality read on words.
 *  @tparam T the arrow on the letters. */
export template <IsArrow T>
  requires std::same_as<Dom<T>, Cod<T>>
struct PowerMap {
  using Domain = std::vector<Dom<T>>;
  using Codomain = std::vector<Dom<T>>;
  T tau;
  std::size_t length;
  constexpr Domain operator()(Domain w) const {
    std::ranges::transform(w, w.begin(), tau);
    return w;
  }
  constexpr bool operator==(const PowerMap&) const = default;
};
/** @param tau an arrow on the letters.  @param n the length.  @return
 *  @c tau on every letter of the words of length @c n. */
export template <IsArrow T>
  requires std::same_as<Dom<T>, Cod<T>>
constexpr PowerMap<T> power(const T& tau, std::size_t n) {
  return {tau, n};
}

/** @brief The rotation of the positions of a word by @c shift, @f$i \mapsto
 *  i + d \bmod n@f$, the symmetry of a power @f$u^j@f$: on a word, the letter
 *  at @c i moves to @c i + d, as @c std::rotl moves a bit.  A family of
 *  arrows, one per word type, as @c Reverse is.  @tparam N the counts. */
export template <IsCountWord N = std::size_t>
struct Rotation {
  N shift;
  template <std::ranges::random_access_range W>
    requires std::semiregular<W> &&
             std::permutable<std::ranges::iterator_t<W>> &&
             std::ranges::sized_range<W>
  constexpr W operator()(W s) const {
    const auto n = static_cast<std::size_t>(std::ranges::size(s));
    if (n == 0) return s;
    const std::size_t k = static_cast<std::size_t>(shift) % n;
    std::ranges::rotate(
        s, std::ranges::begin(s) +
               static_cast<std::ranges::range_difference_t<W>>((n - k) % n));
    return s;
  }
  constexpr bool operator==(const Rotation&) const = default;
};
/** @param d the shift.  @return the rotation of the positions by @c d. */
export template <IsCountWord N = std::size_t>
constexpr Rotation<N> rotate(std::type_identity_t<N> d) {
  return {d};
}
/** @brief The cycle type of the reversal of @c n positions: @f$\lfloor n/2
 *  \rfloor@f$ pairs, and the middle where @c n is odd.  @param n the length.
 *  @return its cycles. */
export template <IsCountWord N>
constexpr std::vector<Cycles<N>> cycle_type(Reverse, N n) {
  std::vector<Cycles<N>> c{Cycles<N>{2, static_cast<N>(n / 2)}};
  if (n % 2 == 1) c.push_back(Cycles<N>{1, 1});
  return c;
}
/** @brief The cycle type of a rotation of @c n positions by @c d: @f$g =
 *  \gcd(n, d)@f$ cycles of length @f$n / g@f$, the identity where @c d is a
 *  multiple of @c n.  @param r the rotation.  @param n the length.  @return
 *  its cycles. */
export template <IsCountWord N>
constexpr std::vector<Cycles<N>> cycle_type(const Rotation<N>& r, N n) {
  if (n == 0) return {};
  const N g = std::gcd(n, static_cast<N>(r.shift % n));  // n where d ≡ 0
  return {Cycles<N>{static_cast<N>(n / g), g}};
}

/** @brief A permutation of positions with a known cycle type.
 *  @tparam S the permutation.  @tparam N the counts. */
export template <typename S, typename N>
concept HasCycleType = IsCountWord<N> && requires(const S& s, N n) {
  { cycle_type(s, n) } -> std::same_as<std::vector<Cycles<N>>>;
};

/** @brief A map applied to every letter commutes with every permutation of
 *  the positions, naturality read on words: @f$\sigma \circ \tau^{\times n} =
 *  \tau^{\times n} \circ \sigma@f$.  So the reverse complement is one map,
 *  written in either order, and an involution where @c tau is one
 *  (@c is_involution, @c :posetal). */
template <IsArrow T, typename W>
  requires std::same_as<Dom<T>, Cod<T>>
struct commutes<PowerMap<T>, Morphism<W, W, Reverse>> : std::true_type {};
template <IsArrow T, typename W, IsCountWord N>
  requires std::same_as<Dom<T>, Cod<T>>
struct commutes<PowerMap<T>, Morphism<W, W, Rotation<N>>> : std::true_type {};

namespace detail_periodic {
/** @brief Whether a letter map, taken twice, returns one more byte. */
struct ReturnsTwice {
  LetterMap t;
  constexpr bool operator()(bool ok, std::size_t i) const {
    const std::byte b{static_cast<unsigned char>(i)};
    return ok && t(t(b)) == b;
  }
};
/** @brief Refuses a symmetry and a power of different lengths. */
[[noreturn]] constexpr void refuse_unequal_lengths() { std::abort(); }
}  // namespace detail_periodic
/** @brief Whether a letter map is an involution, decided on its table: a
 *  law of the value, the 256 bytes taken twice, as a law of a finite
 *  structure is decided.  @param t the map.  @return whether @f$t \circ t =
 *  \mathrm{id}@f$. */
export constexpr bool is_involution(const LetterMap& t) {
  return std::ranges::fold_left(
      std::views::iota(std::size_t{0}, std::size_t{256}), true,
      detail_periodic::ReturnsTwice{t});
}
/** @brief A map on every letter is an involution where the map on one is.
 *  @param p the power of the map.  @return whether it is one. */
export template <IsArrow T>
  requires std::same_as<Dom<T>, Cod<T>>
constexpr bool is_involution(const PowerMap<T>& p) {
  return is_involution(p.tau);
}

/** @brief The section of a tensor power fixed by a permutation @c sigma of
 *  the positions and an arrow @c tau on the letters, @f$x_{\sigma(i)} =
 *  \tau(x_i)@f$: the legs merged along each cycle of @c sigma, the spider's
 *  @c Merge through @c tau, the words de Bruijn's power group element
 *  @f$(\sigma, \tau)@f$ fixes.  @tparam P the power.  @tparam S the
 *  permutation.  @tparam T the arrow on the letters. */
export template <typename P, typename S, IsArrow T>
struct Symmetric {
  P power;
  S sigma;
  T tau;
};
/** @param p a tensor power.  @param sigma a permutation of its positions,
 *  @c reverse or @c rotate(d).  @return the words it fixes. */
export template <HasNNOStep C, IsArrow F, IsCountWord N, typename S>
  requires HasCycleType<S, N>
constexpr Symmetric<Power<C, F, N>, S, Identity<C>> symmetric(
    const Power<C, F, N>& p, S sigma) {
  return {p, sigma, Identity<C>{}};
}
/** @param p a tensor power.  @param sigma a permutation of its positions,
 *  named on its words.  @return the words it fixes. */
export template <HasNNOStep C, IsArrow F, IsCountWord N, typename S>
  requires HasCycleType<S, N>
constexpr Symmetric<Power<C, F, N>, S, Identity<C>> symmetric(
    const Power<C, F, N>& p,
    const Morphism<std::vector<C>, std::vector<C>, S>& sigma) {
  return {p, sigma.transform, Identity<C>{}};
}
/** @param p a tensor power.  @param g an arrow on every letter, then a
 *  permutation of the positions: @c power(complement, n) >> reverse.
 *  @return the words it fixes. */
export template <HasNNOStep C, IsArrow F, IsCountWord N, IsArrow T, typename S>
  requires std::same_as<Dom<T>, C> && HasCycleType<S, N>
constexpr Symmetric<Power<C, F, N>, S, T> symmetric(
    const Power<C, F, N>& p,
    const Compose<PowerMap<T>, Morphism<std::vector<C>, std::vector<C>, S>>&
        g) {
  if (g.f.length != p.length) detail_periodic::refuse_unequal_lengths();
  return {p, g.g.transform, g.f.tau};
}
/** @param p a tensor power.  @param g a permutation of the positions, then
 *  an arrow on every letter: the same section, the two commuting.
 *  @return the words it fixes. */
export template <HasNNOStep C, IsArrow F, IsCountWord N, IsArrow T, typename S>
  requires std::same_as<Dom<T>, C> && HasCycleType<S, N>
constexpr Symmetric<Power<C, F, N>, S, T> symmetric(
    const Power<C, F, N>& p,
    const Compose<Morphism<std::vector<C>, std::vector<C>, S>, PowerMap<T>>&
        g) {
  if (g.g.length != p.length) detail_periodic::refuse_unequal_lengths();
  return {p, g.f.transform, g.g.tau};
}

/** @brief An outer product of two counted tensors, its count the product of
 *  theirs.  @tparam A @tparam B the factors. */
export template <typename A, typename B>
struct Outer {
  A left;
  B right;
};
/** @brief A tensor whose count is a product of factors. */
export template <typename X>
concept HasFactors = requires(const X& x) {
  { count_factors(x) } -> std::same_as<std::vector<Factor<>>>;
};
/** @param a @param b two counted tensors.  @return their outer product. */
export template <HasFactors A, HasFactors B>
constexpr Outer<A, B> operator*(const A& a, const B& b) {
  return {a, b};
}
/** @param o an outer product.  @return its count, factored: both factors'
 *  factors. */
export template <HasFactors A, HasFactors B>
constexpr std::vector<Factor<>> count_factors(const Outer<A, B>& o) {
  auto fs = count_factors(o.left);
  const auto gs = count_factors(o.right);
  fs.insert(fs.end(), gs.begin(), gs.end());
  return fs;
}

/** @brief The free monoid on a filtered alphabet, truncated below a length:
 *  @f$\Sigma^{<L} = \coprod_{n < L} \Sigma^n@f$, the coproduct of the tensor
 *  powers, concatenation of words its operation.  @tparam C the letters.
 *  @tparam F the predicate.  @tparam N the counts. */
export template <HasNNOStep C, IsArrow F, IsCountWord N = std::size_t>
struct FreeMonoid {
  Slice<C> letters;
  F member;
  N below;
};
/** @param leg a chain of letters under a predicate.  @param below the bound
 *  on the length, excluded.  @return the words shorter than it. */
export template <IsCountWord N = std::size_t, HasNNOStep C, IsArrow F>
  requires std::totally_ordered<C> && std::same_as<Dom<F>, C> &&
           std::convertible_to<Cod<F>, bool>
constexpr FreeMonoid<C, F, N> free_monoid(
    const unfold_view<std::optional<C>, detail_chain::SliceStep<C, F>>& leg,
    std::type_identity_t<N> below) {
  const auto p = power<N>(leg, below);
  return {p.letters, p.member, below};
}

namespace detail_periodic {
/** @brief A product kept while the word holds it: none from the first
 *  factor that would leave the word. */
template <IsCountWord N>
struct CheckedTimes {
  N b;
  constexpr std::optional<N> operator()(std::optional<N> acc, N) const {
    if (!acc || (b != 0 && *acc > std::numeric_limits<N>::max() / b)) {
      return std::nullopt;
    }
    return static_cast<N>(*acc * b);
  }
};
/** @brief One factor into the product, checked. */
template <IsCountWord N>
struct CheckedFactor {
  constexpr std::optional<N> operator()(std::optional<N> acc,
                                        const Factor<N>& f) const {
    if (!acc) return std::nullopt;
    return std::ranges::fold_left(std::views::iota(N{0}, f.exponent), acc,
                                  CheckedTimes<N>{f.base});
  }
};
/** @brief One length's count into the sum, checked. */
template <HasNNOStep C, IsArrow F, IsCountWord N>
struct CheckedLength {
  Slice<C> letters;
  F member;
  constexpr std::optional<N> operator()(std::optional<N> acc, N n) const {
    if (!acc) return std::nullopt;
    const auto c = std::ranges::fold_left(
        count_factors(Power<C, F, N>{letters, member, n}), std::optional<N>{1},
        CheckedFactor<N>{});
    if (!c || *c > std::numeric_limits<N>::max() - *acc) return std::nullopt;
    return static_cast<N>(*acc + *c);
  }
};
}  // namespace detail_periodic
namespace detail_periodic {
/** @brief One factor into the product: @f$b^e@f$ is @c e copies of @c b
 *  under @c *, which @c concat on copies computes by squaring, Listing 4's
 *  overload at the monoid @f$(\mathbb{N}/2^w, \times)@f$; the factors are
 *  then the plain fold, one multiplication each. */
template <IsCountWord N>
struct TimesPower {
  constexpr N operator()(N acc, const Factor<N>& f) const {
    return static_cast<N>(acc * concat(std::views::repeat(f.base, f.exponent),
                                       std::multiplies<N>{}));
  }
};
}  // namespace detail_periodic
/** @brief The count of a factored tensor, modulo the word: a monoid
 *  @c concat under @c * over the factors, each a @c concat on copies of a
 *  popcount, by squaring.  Agrees with @c exact_count wherever the word
 *  holds the count.  @param fs the factors.  @return the count, modulo the
 *  word. */
export template <IsCountWord N>
constexpr N count(const std::vector<Factor<N>>& fs) {
  return std::ranges::fold_left(fs, N{1}, detail_periodic::TimesPower<N>{});
}
/** @brief The count of a factored tensor, exactly, or none where the word
 *  cannot hold it: never wrapped, the discipline of @c checked_lcm.
 *  @param fs the factors.  @return the count, or none. */
export template <IsCountWord N>
constexpr std::optional<N> exact_count(const std::vector<Factor<N>>& fs) {
  return std::ranges::fold_left(fs, std::optional<N>{1},
                                detail_periodic::CheckedFactor<N>{});
}
/** @param m the truncated free monoid.  @return how many words are shorter
 *  than its bound, exactly, or none where the word cannot hold it: then they
 *  are no sequence a word can index. */
export template <HasNNOStep C, IsArrow F, IsCountWord N>
constexpr std::optional<N> exact_count(const FreeMonoid<C, F, N>& m) {
  return std::ranges::fold_left(
      std::views::iota(N{0}, m.below), std::optional<N>{0},
      detail_periodic::CheckedLength<C, F, N>{m.letters, m.member});
}

/** @brief @f$k \mapsto 2^k@f$ on the counts, refused past the word: the
 *  window of the bit words.  @tparam N the counts. */
export template <IsCountWord N = std::size_t>
struct Pow2 {
  using Domain = N;
  using Codomain = N;
  constexpr N operator()(N k) const {
    if (k >= static_cast<N>(std::numeric_limits<N>::digits)) {
      detail_periodic::refuse_beyond_word();
    }
    return static_cast<N>(N{1} << k);
  }
};
/** @brief @f$k \mapsto 2^k@f$ on @c std::size_t. */
export inline constexpr Pow2<> pow2{};

namespace detail_periodic {
/** @brief The bit word of width @c W: a machine word where one has the
 *  width, @c std::bitset otherwise. */
template <std::size_t W>
struct bit_word {
  using type = std::bitset<W>;
};
template <>
struct bit_word<8> {
  using type = std::uint8_t;
};
template <>
struct bit_word<16> {
  using type = std::uint16_t;
};
template <>
struct bit_word<32> {
  using type = std::uint32_t;
};
template <>
struct bit_word<64> {
  using type = std::uint64_t;
};
/** @brief The low @c n bits of a machine word: the pattern, its padding
 *  zero. */
template <std::unsigned_integral B>
constexpr B low_bits(std::size_t n) {
  if (n >= static_cast<std::size_t>(std::numeric_limits<B>::digits)) {
    return static_cast<B>(~B{0});
  }
  return static_cast<B>(static_cast<B>(B{1} << n) - B{1});
}
/** @brief The low @c n bits of a @c std::bitset. */
template <std::size_t W>
constexpr std::bitset<W> low_bits_of(std::size_t n) {
  return n >= W ? ~std::bitset<W>{} : ~std::bitset<W>{} >> (W - n);
}
/** @brief A pattern of @c n bits in a machine word, rotated by @c d within
 *  them, the padding kept zero. */
template <std::unsigned_integral B>
  requires(std::numeric_limits<B>::digits <= 64)
constexpr B rotated(B x, std::size_t n, std::size_t d) {
  const std::size_t k = d % n;
  if (k == 0) return x;
  if (n == static_cast<std::size_t>(std::numeric_limits<B>::digits)) {
    return std::rotl(x, static_cast<int>(k));  // the whole word: one rotate
  }
  // within the low n bits: shifted as a 64-bit word, a concrete type, so that
  // >> is the shift and never the arrow's application
  const auto w = static_cast<std::uint64_t>(x);
  return static_cast<B>(((w << k) | (w >> (n - k))) & low_bits<B>(n));
}
/** @brief A pattern of @c n bits in a @c std::bitset, rotated by @c d within
 *  them, the padding kept zero. */
template <std::size_t W>
constexpr std::bitset<W> rotated(const std::bitset<W>& x, std::size_t n,
                                 std::size_t d) {
  const std::size_t k = d % n;
  if (k == 0) return x;
  return ((x << k) | (x >> (n - k))) & low_bits_of<W>(n);
}
}  // namespace detail_periodic
/** @brief The bit word of width @c W, chosen by the width: the machine word
 *  for 8, 16, 32 and 64 bits, @c std::bitset otherwise.  @tparam W the
 *  width. */
export template <std::size_t W>
using BitWord = typename detail_periodic::bit_word<W>::type;
/** @brief The bit patterns of width @c n as a chain: the counts below
 *  @f$2^n@f$, which binary numeration makes the words of @f$\mathbb{B}^n@f$,
 *  bit @c i the letter at position @c i.  No pattern is built: an arrow along
 *  the chain reads it.  Refused past the word, where the patterns are no
 *  sequence a @c std::size_t can index.  Counting by the laws never builds
 *  a pattern at any width; a pattern is packed only where an operation reads
 *  it, at any width known at compile time, and at run time up to
 *  @c max_packed_width (wider, in blocks, is #1109).  @param n the width.
 *  @return the chain of patterns. */
export constexpr auto bit_patterns(std::size_t n) {
  return pow2(n) >> chain<std::size_t>();
}
/** @brief A pattern, given by its count, in the bit word of width @c W, the
 *  padding above it zero: the switch of representation, from the counts to
 *  the word whose API has the rotations and masks.  A section of @c unpack
 *  on the counts below @f$2^W@f$.  @tparam W the width. */
export template <std::size_t W>
  requires(W > 0)
struct Pack {
  using Domain = std::size_t;
  using Codomain = BitWord<W>;
  constexpr Codomain operator()(std::size_t k) const { return Codomain(k); }
};
/** @brief A pattern packed into the bit word of width @c W. */
export template <std::size_t W>
  requires(W > 0)
inline constexpr Pack<W> pack{};
/** @brief A machine word read back as the count of its pattern: the
 *  retraction of @c pack, @f$\mathit{unpack} \circ \mathit{pack} =
 *  \mathrm{id}@f$ on the counts below @f$2^W@f$.  Up to a machine word,
 *  since a count is one.  @tparam W the width. */
export template <std::size_t W>
  requires(W > 0 && W <= 64)
struct Unpack {
  using Domain = BitWord<W>;
  using Codomain = std::size_t;
  constexpr std::size_t operator()(Domain x) const {
    return static_cast<std::size_t>(x);
  }
};
/** @brief A machine word read back as a count. */
export template <std::size_t W>
  requires(W > 0 && W <= 64)
inline constexpr Unpack<W> unpack{};

/** @brief The widest bit word the library precompiles: 512 bits, the widest
 *  common vector register, a @c std::bitset at every width above 64. */
export inline constexpr std::size_t max_packed_width = 512;
/** @brief A width known only at run time, dispatched to the narrowest
 *  precompiled bit word that holds it: 8, 16, 32 or 64 bits, a machine word,
 *  or 128, 256 or 512, a @c std::bitset.  @c f is called with the width as a
 *  type, so it can name @c BitWord of it.  Refused past @c max_packed_width.
 *  @param n the width.  @param f what to do in that word.  @return its
 *  result. */
export template <typename F>
  requires std::invocable<const F&, std::integral_constant<std::size_t, 8>>
constexpr auto with_width(std::size_t n, const F& f) {
  using W8 = std::integral_constant<std::size_t, 8>;
  if (n <= 8) return f(W8{});
  if (n <= 16) return f(std::integral_constant<std::size_t, 16>{});
  if (n <= 32) return f(std::integral_constant<std::size_t, 32>{});
  if (n <= 64) return f(std::integral_constant<std::size_t, 64>{});
  if (n <= 128) return f(std::integral_constant<std::size_t, 128>{});
  if (n <= 256) return f(std::integral_constant<std::size_t, 256>{});
  if (n > max_packed_width) detail_periodic::refuse_beyond_word();
  return f(std::integral_constant<std::size_t, 512>{});
}
/** @brief A pattern of even width @c length, packed into the bit word of
 *  width @c W, that is a square @f$u u@f$: fixed by the rotation by half its
 *  width, within its width, the padding apart.  For a width @f$2^k@f$ these
 *  are exactly the proper powers, the patterns that are not primitive
 *  (Lothaire, @e Combinatorics on Words, §1.3): every proper divisor of
 *  @f$2^k@f$ divides @f$2^{k-1}@f$.  @tparam W the word. */
export template <std::size_t W>
  requires(W > 0)
struct IsSquare {
  using Domain = BitWord<W>;
  using Codomain = bool;
  std::size_t length = W;
  constexpr bool operator()(const Domain& x) const {
    return length % 2 == 0 && length <= W &&
           detail_periodic::rotated(x, length, length / 2) == x;
  }
};

namespace detail_periodic {
/** @brief A letter after @c tau taken @c steps times. */
template <IsArrow T, IsCountWord N>
struct Iterate {
  T tau;
  constexpr Dom<T> operator()(Dom<T> a, N) const { return tau(a); }
};
/** @brief Whether the letter @c tau takes @c steps times to is kept. */
template <IsArrow F, IsArrow T, IsCountWord N>
  requires std::same_as<Dom<T>, Dom<F>> && std::same_as<Cod<T>, Dom<F>>
struct KeptAfter {
  using Domain = Dom<F>;
  using Codomain = bool;
  F member;
  T tau;
  N steps;
  constexpr bool operator()(const Domain& a) const {
    return static_cast<bool>(member(std::ranges::fold_left(
        std::views::iota(N{0}, steps), a, Iterate<T, N>{tau})));
  }
};
/** @brief Whether @c tau taken @c steps times returns the letter. */
template <IsArrow T, IsCountWord N>
  requires std::same_as<Cod<T>, Dom<T>> && std::equality_comparable<Dom<T>>
struct ReturnsAfter {
  using Domain = Dom<T>;
  using Codomain = bool;
  T tau;
  N steps;
  constexpr bool operator()(const Domain& a) const {
    return std::ranges::fold_left(std::views::iota(N{0}, steps), a,
                                  Iterate<T, N>{tau}) == a;
  }
};
/** @brief A machine word with bit @c i set to @c b. */
template <std::unsigned_integral B>
constexpr B with_bit(B x, std::size_t i, bool b) {
  return b ? static_cast<B>(x | static_cast<B>(B{1} << i)) : x;
}
/** @brief A @c std::bitset with bit @c i set to @c b. */
template <std::size_t W>
constexpr std::bitset<W> with_bit(std::bitset<W> x, std::size_t i, bool b) {
  return x.set(i, b);
}
/** @brief One letter's bit into the packed characteristic function. */
template <std::size_t W>
struct SetBit {
  constexpr std::pair<BitWord<W>, std::size_t> operator()(
      std::pair<BitWord<W>, std::size_t> s, bool b) const {
    return {with_bit(s.first, s.second, b), s.second + 1};
  }
};
/** @brief The characteristic function of a predicate over a chain of
 *  letters, packed into the bit word of width @c W, bit @c i the @c i-th
 *  letter: a subobject of the alphabet as a word. */
template <std::size_t W, HasNNOStep C, IsArrow G>
  requires std::same_as<Dom<G>, C>
constexpr BitWord<W> packed_chi(const Slice<C>& letters, const G& g) {
  return std::ranges::fold_left(
             letters >> g, std::pair<BitWord<W>, std::size_t>{}, SetBit<W>{})
      .first;
}
}  // namespace detail_periodic

/** @brief The number of bits set in a machine word: the hardware's count,
 *  one instruction where the target has it.  @param x the word.  @return
 *  how many of its bits are set. */
export template <std::unsigned_integral B>
  requires(!std::same_as<B, bool>)
constexpr std::size_t popcount(B x) {
  return static_cast<std::size_t>(std::popcount(x));
}
/** @brief The number of bits set in a @c std::bitset.  @param x the word.
 *  @return how many of its bits are set. */
export template <std::size_t W>
constexpr std::size_t popcount(const std::bitset<W>& x) {
  return x.count();
}

namespace detail_periodic {
/** @brief The letters that fill a merged cycle of @c length legs through
 *  @c tau, counted in the bit word of width @c W: the spider's merge on the
 *  truth values is @f$\wedge@f$, so packed it is @c &, one mask per step
 *  around the cycle and one for the letters @c tau returns, and one popcount.
 */
template <HasNNOStep C, IsArrow F, IsArrow T, IsCountWord N>
struct CycleCount {
  Slice<C> letters;
  F member;
  T tau;
  N length;
  /** @brief One more step's mask into the meet. */
  template <std::size_t W>
  struct AndStep {
    Slice<C> letters;
    F member;
    T tau;
    constexpr BitWord<W> operator()(const BitWord<W>& m, N j) const {
      return static_cast<BitWord<W>>(
          m & packed_chi<W>(letters, KeptAfter<F, T, N>{member, tau, j}));
    }
  };
  template <std::size_t W>
  constexpr N operator()(std::integral_constant<std::size_t, W>) const {
    return static_cast<N>(popcount(std::ranges::fold_left(
        std::views::iota(N{0}, length),
        packed_chi<W>(letters, ReturnsAfter<T, N>{tau, length}),
        AndStep<W>{letters, member, tau})));
  }
};
/** @brief How many letters a chain has. */
template <HasNNOStep C, IsCountWord N>
constexpr N letters_in(const Slice<C>& letters) {
  return count<N>(letters >> constant<C>(true));
}
/** @brief One merged leg's factor: the letters that fill a cycle, by
 *  popcount in the narrowest word that holds the alphabet, raised to the
 *  number of such cycles. */
template <HasNNOStep C, IsArrow F, IsArrow T, IsCountWord N>
struct CycleFactor {
  Slice<C> letters;
  F member;
  T tau;
  constexpr std::vector<Factor<N>> operator()(std::vector<Factor<N>> fs,
                                              const Cycles<N>& c) const {
    fs.push_back(Factor<N>{
        with_width(letters_in<C, N>(letters),
                   CycleCount<C, F, T, N>{letters, member, tau, c.length}),
        c.count});
    return fs;
  }
};
}  // namespace detail_periodic

/** @brief The factors of a tensor power's count: the filtered alphabet's
 *  count, its packed characteristic function's popcount, raised to the
 *  length, by the box law.  @param p the power.  @return its count,
 *  factored. */
export template <HasNNOStep C, IsArrow F, IsCountWord N>
constexpr std::vector<Factor<N>> count_factors(const Power<C, F, N>& p) {
  return {
      Factor<N>{with_width(detail_periodic::letters_in<C, N>(p.letters),
                           detail_periodic::CycleCount<C, F, Identity<C>, N>{
                               p.letters, p.member, Identity<C>{}, N{1}}),
                p.length}};
}
/** @brief The factors of a symmetric section's count: one merged leg per
 *  cycle of @c sigma, so the box law after the merges, the count Pólya's
 *  theorem gives for the words a permutation fixes.  @param s the section.
 *  @return its count, factored. */
export template <HasNNOStep C, IsArrow F, IsCountWord N, typename S, IsArrow T>
  requires HasCycleType<S, N>
constexpr std::vector<Factor<N>> count_factors(
    const Symmetric<Power<C, F, N>, S, T>& s) {
  const auto& p = s.power;
  return std::ranges::fold_left(
      cycle_type(s.sigma, p.length), std::vector<Factor<N>>{},
      detail_periodic::CycleFactor<C, F, T, N>{p.letters, p.member, s.tau});
}

}  // namespace dedekind::category
