/**
 * @file dedekind/category/periodic.cppm
 * @partition :periodic
 * @brief Periodic arrows on the counts and the counts they license: an arrow
 *        @c f is periodic with period @c p when @f$f(k + p) = f(k)@f$, a law
 *        declared at a generator and inferred at a composite, so that a count
 *        along a chain of any length is @c (n div p) periods and a remainder
 *        (the Jlt paper's flagship, #1104).
 *
 * @details The period is a @b value the arrow carries, read by @c period(f),
 *          and @c HasPeriod<F> says only that @c F carries one: a period that
 *          is a type would leave nothing for a caller to choose, and the
 *          Python bindings choose it.  @c opaque(f) hides the law, so that the
 *          plain fold runs where a test checks the law against it.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>  // std::ranges::fold_left
#include <array>
#include <bit>  // std::bit_width: the bits fast doubling reads
#include <concepts>
#include <cstddef>
#include <cstdlib>  // std::abort: a modulus of 0 is refused
#include <functional>
#include <limits>   // the extent of a law that holds on the whole word
#include <numeric>  // std::lcm: the period of a join or a meet
#include <ranges>
#include <type_traits>
#include <utility>
#include <vector>  // the residue histograms of a modular section

export module dedekind.category:periodic;

import :morphism;              // IsArrow, Dom, Cod, Compose
import :chain;                 // the counting chain, unfold_view, Mapped
import :nno;                   // Prior: the default chain's step
import :discrete;              // equals: an arrow into the truth values
import :lattice;               // Meet, Join: the nodes & and | build
import :cartesian_bicategory;  // Tensor, Merge: the outer product

namespace dedekind::category {

/** @brief An arrow on the counts with a declared period, read by
 *  @c period(f): @f$f(k + p) = f(k)@f$ for every @c k.  @tparam F the arrow. */
export template <typename F>
concept HasPeriod =
    IsArrow<F> && std::same_as<Dom<F>, std::size_t> && requires(const F& f) {
      { period(f) } -> std::same_as<std::size_t>;
    };

/** @brief Inferred, not declared: an arrow after a periodic one is periodic
 *  with the same period, whatever the second arrow is.
 *  @param c the composite.  @return the first leg's period. */
export template <HasPeriod F, IsArrow G>
constexpr std::size_t period(const Compose<F, G>& c) {
  return period(c.f);
}

/** @brief Where a period's law holds: @f$f(k + p) = f(k)@f$ for every
 *  @c k with @c k + p below the extent.  A generator's law holds on the whole
 *  word; a shift narrows it, since the word does not wrap for the law.
 *  @param f the arrow.  @return the extent. */
export template <HasPeriod F>
constexpr std::size_t extent(const F&) {
  return std::numeric_limits<std::size_t>::max();
}
/** @param c the composite.  @return the first leg's extent. */
export template <HasPeriod F, IsArrow G>
constexpr std::size_t extent(const Compose<F, G>& c) {
  return extent(c.f);
}

namespace detail_periodic {
/** @brief Refuses a modulus of 0, which has no residues: not a constant
 *  expression at compile time, the end of the program at run time. */
[[noreturn]] constexpr void refuse_zero_modulus() { std::abort(); }
/** @brief Refuses a count past the extent of the law it would use. */
[[noreturn]] constexpr void refuse_beyond_extent() { std::abort(); }
}  // namespace detail_periodic

/** @brief The residue class @f$\{k \mid k \equiv r \pmod m\}@f$ as a
 *  predicate on the counts, periodic with period @c m. */
export struct Congruent {
  using Domain = std::size_t;
  using Codomain = bool;
  std::size_t modulus{1};
  std::size_t residue{0};
  /** @param k a count.  @return whether @c k is congruent to the residue. */
  constexpr bool operator()(std::size_t k) const {
    return k % modulus == residue;
  }
  constexpr bool operator==(const Congruent&) const = default;
};
/** @param c the residue class.  @return its modulus. */
export constexpr std::size_t period(const Congruent& c) { return c.modulus; }
/** @brief The residue class of @c r modulo @c m.  @param m the modulus,
 *  positive.  @param r the residue, taken modulo @c m. */
export constexpr Congruent congruent(std::size_t m, std::size_t r) {
  if (m == 0) detail_periodic::refuse_zero_modulus();
  return {m, r % m};
}
/** @brief The even counts, the residue class of 0 modulo 2.  Its reading on
 *  ℕ as a sequence, a path into the truth values, is
 *  @c dedekind::sequences::is_even (@c sequences:samples). */
export inline constexpr Congruent is_even{2, 0};

namespace detail_periodic {
/** @brief One step of fast doubling, @f$(F(j), F(j+1))@f$ modulo @c m to
 *  @f$(F(2j), F(2j+1))@f$ or @f$(F(2j+1), F(2j+2))@f$ by bit @c i of @c k:
 *  @f$F(2j) = F(j)(2F(j+1) - F(j))@f$, @f$F(2j+1) = F(j)^2 + F(j+1)^2@f$. */
struct Double {
  std::size_t k;
  std::size_t m;
  std::size_t w;  // the bits of k, read from the top: step i reads bit w-1-i
  constexpr std::array<std::size_t, 2> operator()(std::array<std::size_t, 2> p,
                                                  std::size_t i) const {
    const std::size_t a = p[0], b = p[1];
    const std::size_t c = a * ((2 * b + m - a) % m) % m;
    const std::size_t d = (a * a + b * b) % m;
    return ((k >> (w - 1 - i)) & 1U) != 0
               ? std::array<std::size_t, 2>{d, (c + d) % m}
               : std::array<std::size_t, 2>{c, d};
  }
};
/** @brief One step of the search for the Pisano period: the pair
 *  @f$(F(j), F(j+1))@f$ modulo @c m, and the least @c j at which it is
 *  @f$(0, 1)@f$ again, once found. */
struct Pisano {
  std::size_t m;
  constexpr std::array<std::size_t, 3> operator()(std::array<std::size_t, 3> s,
                                                  std::size_t j) const {
    if (s[2] != 0) return s;
    const std::size_t a = s[1], b = (s[0] + s[1]) % m;
    return {a, b, (a == 0 && b == 1 % m) ? j : 0};
  }
};
}  // namespace detail_periodic

/** @brief The Fibonacci numbers modulo @c m, @f$k \mapsto F(k) \bmod m@f$,
 *  by fast doubling, @f$\log k@f$ steps; the modulus is below @f$2^{32}@f$
 *  so that the products stay in the word. */
export struct FibonacciMod {
  using Domain = std::size_t;
  using Codomain = std::size_t;
  std::size_t modulus{1};
  /** @param k a count.  @return @f$F(k) \bmod m@f$. */
  constexpr std::size_t operator()(std::size_t k) const {
    const auto w = static_cast<std::size_t>(std::bit_width(k));
    return std::ranges::fold_left(std::views::iota(std::size_t{0}, w),
                                  std::array<std::size_t, 2>{0, 1 % modulus},
                                  detail_periodic::Double{k, modulus, w})[0];
  }
  constexpr bool operator==(const FibonacciMod&) const = default;
};
/** @brief The Pisano period @f$\pi(m)@f$, the period of the Fibonacci
 *  numbers modulo @c m, a theorem: it exists and is at most @f$6m@f$, so a
 *  search of that length finds it.  @param f the arrow.  @return its period. */
export constexpr std::size_t period(const FibonacciMod& f) {
  const std::size_t m = f.modulus;
  return std::ranges::fold_left(std::views::iota(std::size_t{1}, 6 * m + 1),
                                std::array<std::size_t, 3>{0, 1 % m, 0},
                                detail_periodic::Pisano{m})[2];
}
/** @brief The Fibonacci numbers modulo @c m.  @param m the modulus, positive
 *  and below @f$2^{32}@f$. */
export constexpr FibonacciMod fibonacci_mod(std::size_t m) {
  if (m == 0) detail_periodic::refuse_zero_modulus();
  return {m};
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
struct Iverson {
  constexpr std::size_t operator()(std::size_t m, bool b) const {
    return m + (b ? 1 : 0);
  }
};
}  // namespace detail_periodic

/** @brief The count of a range of truth values: the plain fold.
 *  @param r the range.  @return how many are true. */
export template <std::ranges::input_range R>
  requires std::convertible_to<std::ranges::range_value_t<R>, bool>
constexpr std::size_t count(R r) {
  return std::ranges::fold_left(std::move(r), std::size_t{0},
                                detail_periodic::Iverson{});
}

namespace detail_periodic {
/** @brief The plain count of @c f(0), ..., @c f(m - 1).  @param m how many.
 *  @param f the predicate. */
template <IsArrow F>
constexpr std::size_t count_below(std::size_t m, const F& f) {
  return count(m >> chain<std::size_t>() >> opaque(f));
}
}  // namespace detail_periodic
/** @brief The count along the chain of @c n under a periodic predicate:
 *  @c (n div p) periods and a remainder, by periodicity; the chain's order
 *  does not matter, since the counts commute.  More specialised than the
 *  plain fold, so resolution prefers it.  @param v the chain under the
 *  predicate.  @return how many of @c f(0), ..., @c f(n - 1) are true. */
export template <HasPeriod F>
  requires std::convertible_to<Cod<F>, bool>
constexpr std::size_t count(
    const unfold_view<
        std::size_t,
        Mapped<std::size_t, StepCoalgebra<std::size_t, Prior<std::size_t>>, F>>&
        v) {
  const F f = v.coalgebra().f;
  const std::size_t n = v.seed(), p = period(f);
  if (n > extent(f)) detail_periodic::refuse_beyond_extent();
  return (n / p) * detail_periodic::count_below(p, f) +
         detail_periodic::count_below(n % p, f);
}

/** @brief The sum of a range of counts: the plain fold.  @param r the range.
 *  @return the sum, modulo the word. */
export template <std::ranges::input_range R>
  requires std::same_as<std::ranges::range_value_t<R>, std::size_t>
constexpr std::size_t sum(R r) {
  return std::ranges::fold_left(std::move(r), std::size_t{0},
                                std::plus<std::size_t>{});
}
namespace detail_periodic {
/** @brief The plain sum of @c f(0), ..., @c f(m - 1).  @param m how many.
 *  @param f the arrow. */
template <IsArrow F>
constexpr std::size_t sum_below(std::size_t m, const F& f) {
  return sum(m >> chain<std::size_t>() >> opaque(f));
}
}  // namespace detail_periodic
/** @brief The sum along the chain of @c n under a periodic arrow into the
 *  counts: @c (n div p) copies of one period's sum, one multiplication by the
 *  semimodule law, and a remainder.  @param v the chain under the arrow.
 *  @return @c f(0) + ... + @c f(n - 1), modulo the word. */
export template <HasPeriod F>
  requires std::same_as<Cod<F>, std::size_t>
constexpr std::size_t sum(
    const unfold_view<
        std::size_t,
        Mapped<std::size_t, StepCoalgebra<std::size_t, Prior<std::size_t>>, F>>&
        v) {
  const F f = v.coalgebra().f;
  const std::size_t n = v.seed(), p = period(f);
  if (n > extent(f)) detail_periodic::refuse_beyond_extent();
  return (n / p) * detail_periodic::sum_below(p, f) +
         detail_periodic::sum_below(n % p, f);
}

/** @brief A translation of the counts, @f$k \mapsto k + j@f$: after it, a
 *  periodic arrow keeps its period, on an extent narrowed by @c j. */
export struct Shift {
  using Domain = std::size_t;
  using Codomain = std::size_t;
  std::size_t by{0};
  /** @param k a count.  @return @c k + by. */
  constexpr std::size_t operator()(std::size_t k) const { return k + by; }
  constexpr bool operator==(const Shift&) const = default;
};
/** @param j the translation.  @return @f$k \mapsto k + j@f$. */
export constexpr Shift shift(std::size_t j) { return {j}; }
/** @param c a translation, then a periodic arrow.  @return its period. */
export template <HasPeriod G>
constexpr std::size_t period(const Compose<Shift, G>& c) {
  return period(c.g);
}
/** @param c a translation, then a periodic arrow.  @return the second leg's
 *  extent, less the translation. */
export template <HasPeriod G>
constexpr std::size_t extent(const Compose<Shift, G>& c) {
  const std::size_t e = extent(c.g);
  return e > c.f.by ? e - c.f.by : 0;
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
constexpr Join<P, Q> operator|(P p, Q q) {
  return {std::move(p), std::move(q)};
}
/** @brief The meet, dual to the join.  @param p @param q the predicates.
 *  @return @c p and @c q. */
export template <detail_periodic::IsPeriodicPredicate P,
                 detail_periodic::IsPeriodicPredicate Q>
constexpr Meet<P, Q> operator&(P p, Q q) {
  return {std::move(p), std::move(q)};
}
/** @param j the join.  @return the lcm of its legs' periods. */
export template <HasPeriod P, HasPeriod Q>
constexpr std::size_t period(const Join<P, Q>& j) {
  return std::lcm(period(j.lhs), period(j.rhs));
}
/** @param m the meet.  @return the lcm of its legs' periods. */
export template <HasPeriod P, HasPeriod Q>
constexpr std::size_t period(const Meet<P, Q>& m) {
  return std::lcm(period(m.lhs), period(m.rhs));
}
/** @param j the join.  @return the lesser of its legs' extents. */
export template <HasPeriod P, HasPeriod Q>
constexpr std::size_t extent(const Join<P, Q>& j) {
  return std::min(extent(j.lhs), extent(j.rhs));
}
/** @param m the meet.  @return the lesser of its legs' extents. */
export template <HasPeriod P, HasPeriod Q>
constexpr std::size_t extent(const Meet<P, Q>& m) {
  return std::min(extent(m.lhs), extent(m.rhs));
}

namespace detail_periodic {
/** @brief The window's step: how many are true. */
struct CountStep {
  constexpr std::size_t operator()(std::size_t m, bool b) const {
    return m + (b ? 1 : 0);
  }
};
/** @brief The window's step: whether any is true. */
struct AnyStep {
  constexpr bool operator()(bool a, bool b) const { return a || b; }
};
/** @brief One step of a window at @c k: the step applied to @c f(k + j). */
template <typename F, typename Step>
struct Window {
  F f;
  std::size_t k;
  template <typename Acc>
  constexpr Acc operator()(Acc acc, std::size_t j) const {
    return Step{}(acc, f(k + j));
  }
};
}  // namespace detail_periodic
/** @brief A window over a periodic predicate, SQL's window function over
 *  @c ROWS @c BETWEEN @c CURRENT @c ROW @c AND @c w - 1 @c FOLLOWING:
 *  @f$k \mapsto@f$ the step folded over @f$f(k), \ldots, f(k + w - 1)@f$.  A
 *  finite combination of translations, so periodic with the same period.
 *  @tparam F the predicate.  @tparam Step the fold.  @tparam Acc its result. */
export template <detail_periodic::IsPeriodicPredicate F, typename Step,
                 typename Acc>
struct Rolling {
  using Domain = std::size_t;
  using Codomain = Acc;
  F f;
  std::size_t width{1};
  /** @param k a count.  @return the window's fold at @c k. */
  constexpr Acc operator()(std::size_t k) const {
    return std::ranges::fold_left(std::views::iota(std::size_t{0}, width),
                                  Acc{},
                                  detail_periodic::Window<F, Step>{f, k});
  }
};
/** @param r the window.  @return the predicate's period. */
export template <typename F, typename Step, typename Acc>
constexpr std::size_t period(const Rolling<F, Step, Acc>& r) {
  return period(r.f);
}
/** @param r the window.  @return the predicate's extent, less the window. */
export template <typename F, typename Step, typename Acc>
constexpr std::size_t extent(const Rolling<F, Step, Acc>& r) {
  const std::size_t e = extent(r.f);
  return e >= r.width ? e - (r.width - 1) : 0;
}
/** @brief How many of @c w consecutive values are true, SQL's
 *  @c COUNT @c OVER a window.  @param f the predicate.  @param w the width. */
export template <detail_periodic::IsPeriodicPredicate F>
constexpr Rolling<F, detail_periodic::CountStep, std::size_t> rolling_count(
    F f, std::size_t w) {
  return {std::move(f), w};
}
/** @brief Whether any of @c w consecutive values is true.  @param f the
 *  predicate.  @param w the width. */
export template <detail_periodic::IsPeriodicPredicate F>
constexpr Rolling<F, detail_periodic::AnyStep, bool> rolling_any(
    F f, std::size_t w) {
  return {std::move(f), w};
}

// --- The outer product of predicates along chains, and its counts ----------
namespace detail_periodic {
/** @brief The meet of a pair of truth values: the spider's merge on 𝔹. */
using Both = Merge<bool, std::logical_and<bool>>;
/** @brief A chain of the counts under an arrow. */
template <typename F>
using Along = unfold_view<
    std::size_t,
    Mapped<std::size_t, StepCoalgebra<std::size_t, Prior<std::size_t>>, F>>;
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
 *  followed by the merge, and the box's sides are the chains' lengths.
 *  @tparam T the entry.  @tparam K the number of legs. */
export template <typename T, std::size_t K>
struct Box {
  T entry;
  std::array<std::size_t, K> sides;
};
/** @param a @param b two chains under predicates.  @return their outer
 *  product. */
export template <IsArrow F, IsArrow G>
  requires std::same_as<Cod<F>, bool> && std::same_as<Cod<G>, bool>
constexpr auto operator*(const detail_periodic::Along<F>& a,
                         const detail_periodic::Along<G>& b) {
  using E = Compose<Tensor<F, G>, detail_periodic::Both>;
  return Box<E, 2>{E{Tensor<F, G>{a.coalgebra().f, b.coalgebra().f},
                     detail_periodic::Both{}},
                   {a.seed(), b.seed()}};
}
/** @param x an outer product.  @param b one more chain under a predicate.
 *  @return their outer product. */
export template <typename T, std::size_t K, IsArrow G>
  requires std::same_as<Cod<G>, bool>
constexpr auto operator*(const Box<T, K>& x,
                         const detail_periodic::Along<G>& b) {
  using E = Compose<Tensor<T, G>, detail_periodic::Both>;
  std::array<std::size_t, K + 1> sides{};
  std::ranges::copy(x.sides, sides.begin());
  sides[K] = b.seed();
  return Box<E, K + 1>{
      E{Tensor<T, G>{x.entry, b.coalgebra().f}, detail_periodic::Both{}},
      sides};
}

namespace detail_periodic {
/** @brief Two arrays, one after the other. */
template <std::size_t A, std::size_t B>
constexpr std::array<std::size_t, A + B> concat(
    const std::array<std::size_t, A>& a, const std::array<std::size_t, B>& b) {
  std::array<std::size_t, A + B> out{};
  std::ranges::copy(a, out.begin());
  std::ranges::copy(b, out.begin() + A);
  return out;
}
/** @brief Each leg's count, the legs read left to right from @c at. */
template <typename T, std::size_t K>
constexpr std::array<std::size_t, box_legs<T>::value> counts_of(
    const T& t, const std::array<std::size_t, K>& sides, std::size_t at) {
  if constexpr (box_node<T>::value) {
    using L = typename box_node<T>::left;
    return concat(counts_of(t.f.r, sides, at),
                  counts_of(t.f.s, sides, at + box_legs<L>::value));
  } else {
    return {count(sides[at] >> chain<std::size_t>() >> t)};
  }
}
/** @brief One residue's count on a leg, appended to its histogram. */
template <typename T>
struct Residue {
  T t;
  std::size_t n;
  std::size_t m;
  constexpr std::vector<std::size_t> operator()(std::vector<std::size_t> h,
                                                std::size_t r) const {
    h.push_back(count(n >> chain<std::size_t>() >>
                      Meet<T, Congruent>{t, congruent(m, r)}));
    return h;
  }
};
/** @brief Each leg's histogram over the residues modulo @c m, the legs read
 *  left to right from @c at. */
template <typename T, std::size_t K>
constexpr std::vector<std::vector<std::size_t>> residues_of(
    const T& t, const std::array<std::size_t, K>& sides, std::size_t at,
    std::size_t m) {
  if constexpr (box_node<T>::value) {
    using L = typename box_node<T>::left;
    auto hs = residues_of(t.f.r, sides, at, m);
    const auto rs = residues_of(t.f.s, sides, at + box_legs<L>::value, m);
    hs.insert(hs.end(), rs.begin(), rs.end());
    return hs;
  } else {
    return {std::ranges::fold_left(std::views::iota(std::size_t{0}, m),
                                   std::vector<std::size_t>{},
                                   Residue<T>{t, sides[at], m})};
  }
}
/** @brief One term of a cyclic convolution: the residues @c q div m and
 *  @c q mod m of two histograms add to their sum modulo @c m. */
struct CyclicTerm {
  std::vector<std::size_t> a;
  std::vector<std::size_t> b;
  std::size_t m;
  constexpr std::vector<std::size_t> operator()(std::vector<std::size_t> c,
                                                std::size_t q) const {
    const std::size_t i = q / m, j = q % m;
    c[(i + j) % m] += a[i] * b[j];
    return c;
  }
};
/** @brief The cyclic convolution of the histogram so far with one more leg's,
 *  in the counts' semiring, the ring of polynomials modulo @f$x^m - 1@f$. */
struct Convolve {
  std::size_t m;
  constexpr std::vector<std::size_t> operator()(
      const std::vector<std::size_t>& acc,
      const std::vector<std::size_t>& h) const {
    return std::ranges::fold_left(std::views::iota(std::size_t{0}, m * m),
                                  std::vector<std::size_t>(m, 0),
                                  CyclicTerm{acc, h, m});
  }
};
}  // namespace detail_periodic

/** @brief Each leg's count over its chain, the factors of the box's count.
 *  @param b the outer product.  @return the counts, leg by leg. */
export template <typename T, std::size_t K>
constexpr std::array<std::size_t, K> count_factors(const Box<T, K>& b) {
  return detail_periodic::counts_of(b.entry, b.sides, 0);
}
/** @brief The count over the box, SQL's @c COUNT(*) over a cross join: by
 *  distributivity the product of the legs' counts, contracting the rank-one
 *  tensor against the all-ones covector on each leg.  Exact beyond the word
 *  only as its factors (@c count_factors); this is its residue.
 *  @param b the outer product.  @return the count, modulo the word. */
export template <typename T, std::size_t K>
constexpr std::size_t count(const Box<T, K>& b) {
  return std::ranges::fold_left(count_factors(b), std::size_t{1},
                                std::multiplies<std::size_t>{});
}
/** @brief The count over the section @f$i_1 + \cdots + i_K \equiv r
 *  \pmod m@f$ of the box, SQL's @c GROUP @c BY a computed key: the cyclic
 *  convolution of the legs' histograms over the residues, @c m numbers a
 *  leg, in the counts' semiring.  @param b the outer product.  @param m the
 *  modulus, positive.  @param r the residue.  @return the count, modulo the
 *  word. */
export template <typename T, std::size_t K>
constexpr std::size_t count_section(const Box<T, K>& b, std::size_t m,
                                    std::size_t r) {
  if (m == 0) detail_periodic::refuse_zero_modulus();
  std::vector<std::size_t> unit(m, 0);
  unit[0] = 1;
  return std::ranges::fold_left(
      detail_periodic::residues_of(b.entry, b.sides, 0, m), unit,
      detail_periodic::Convolve{m})[r % m];
}

namespace detail_periodic_witness {
constexpr auto fib_even = fibonacci_mod(2) >> equals(std::size_t{0});
constexpr std::size_t n = std::size_t{1} << 60;
static_assert(
    period(is_even) == 2 && period(fibonacci_mod(2)) == 3 &&
        period(fibonacci_mod(10)) == 60 && period(fib_even) == 3 &&
        fibonacci_mod(1'000)(90) == 2'880'067'194'370'816'120uz % 1'000,
    "the residue classes and the Pisano periods, declared and inferred");
static_assert(count(n >> chain<std::size_t>() >> is_even) == n / 2 &&
                  count(n >> chain<std::size_t>() >> fib_even) == n / 3 + 1 &&
                  count(1'000uz >> chain<std::size_t>() >> fib_even) ==
                      count(1'000uz >> chain<std::size_t>() >>
                            opaque(fib_even)),
              "the law's count over 2^60, and the plain fold agreeing with it "
              "where both can run");
constexpr auto every = n >> chain<std::size_t>();
static_assert(period(shift(1) >> fib_even) == 3 &&
                  extent(shift(1) >> fib_even) ==
                      std::numeric_limits<std::size_t>::max() - 1 &&
                  period(is_even | fib_even) == 6 &&
                  period(is_even & fib_even) == 6 &&
                  period(rolling_any(fib_even, 3)) == 3,
              "periods through a translation, a join, a meet and a window");
static_assert(count(every >> (is_even | fib_even)) == n / 2 + n / 3 - n / 6 &&
                  count(every >> rolling_any(fib_even, 3)) == n &&
                  sum(every >> rolling_count(fib_even, 3)) == n,
              "every window of three Fibonacci numbers holds exactly one even "
              "one, found by the law over 2^60");
static_assert(count(1'000uz >> chain<std::size_t>() >> (is_even | fib_even)) ==
                      count(1'000uz >> chain<std::size_t>() >>
                            opaque(is_even | fib_even)) &&
                  sum(1'000uz >> chain<std::size_t>() >>
                      rolling_count(fib_even, 3)) ==
                      sum(1'000uz >> chain<std::size_t>() >>
                          opaque(rolling_count(fib_even, 3))),
              "the laws agree with the plain fold where both can run");
static_assert(count_factors((n >> chain<std::size_t>() >> is_even) *
                            (n >> chain<std::size_t>() >> fib_even)) ==
                  std::array<std::size_t, 2>{n / 2, n / 3 + 1},
              "the box's count factorises, leg by leg");
}  // namespace detail_periodic_witness

}  // namespace dedekind::category
