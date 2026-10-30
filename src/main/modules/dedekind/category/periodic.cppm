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
#include <concepts>
#include <cstddef>
#include <cstdlib>  // std::abort: a modulus of 0 is refused
#include <functional>
#include <ranges>
#include <type_traits>

export module dedekind.category:periodic;

import :morphism;  // IsArrow, Dom, Cod, Compose
import :chain;     // the counting chain, unfold_view, Mapped
import :nno;       // Prior: the default chain's step
import :discrete;  // equals: an arrow into the truth values

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

namespace detail_periodic {
/** @brief Refuses a modulus of 0, which has no residues: not a constant
 *  expression at compile time, the end of the program at run time. */
[[noreturn]] constexpr void refuse_zero_modulus() { std::abort(); }
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
/** @brief The even counts, the residue class of 0 modulo 2. */
export inline constexpr Congruent is_even{2, 0};

namespace detail_periodic {
/** @brief One step of fast doubling, @f$(F(j), F(j+1))@f$ modulo @c m to
 *  @f$(F(2j), F(2j+1))@f$ or @f$(F(2j+1), F(2j+2))@f$ by bit @c i of @c k:
 *  @f$F(2j) = F(j)(2F(j+1) - F(j))@f$, @f$F(2j+1) = F(j)^2 + F(j+1)^2@f$. */
struct Double {
  std::size_t k;
  std::size_t m;
  constexpr std::array<std::size_t, 2> operator()(std::array<std::size_t, 2> p,
                                                  std::size_t i) const {
    const std::size_t a = p[0], b = p[1];
    const std::size_t c = a * ((2 * b + m - a) % m) % m;
    const std::size_t d = (a * a + b * b) % m;
    return ((k >> i) & 1U) != 0 ? std::array<std::size_t, 2>{d, (c + d) % m}
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
    const auto bits =
        std::views::iota(std::size_t{0}, static_cast<std::size_t>(64)) |
        std::views::reverse;
    return std::ranges::fold_left(bits,
                                  std::array<std::size_t, 2>{0, 1 % modulus},
                                  detail_periodic::Double{k, modulus})[0];
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
  return (n / p) * detail_periodic::count_below(p, f) +
         detail_periodic::count_below(n % p, f);
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
}  // namespace detail_periodic_witness

}  // namespace dedekind::category
