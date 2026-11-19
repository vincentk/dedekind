/**
 * @file src/test/cpp/modules/dedekind/python/showcase_16_listing3_two_folds.cpp
 * @brief Showcase 16: the Jlt paper's two recursions, compiled.  The listings
 *        of Section 2, self-contained, and what the compiler leaves of each:
 *        the fold over a value at both binding times, and the folds over a
 *        term's type, which are finished while compiling.
 *
 * Every name is declared here or is the standard's: the arrows and the law of
 * Listing 1, the monoid of Listing 2, and @c concat, @c then and @c length of
 * Listing 3.  The functions below are @c extern @c "C" and never inlined, so
 * each is its own symbol in the IR the fixture script reads.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <algorithm>
#include <array>
#include <concepts>
#include <cstddef>
#include <functional>
#include <ranges>

namespace {
using std::size_t;

// Listing 1: arrows on size_t, and a law as a table.
struct Succ {
  constexpr size_t operator()(size_t x) const { return x + 1; }
};
struct Neg {
  constexpr size_t operator()(size_t x) const { return -x; }
};
template <typename F>
concept Arrow = requires(const F f, size_t x) {
  { f(x) } -> std::same_as<size_t>;
};
template <Arrow F, Arrow G>
struct Then {
  F f;
  G g;
  constexpr size_t operator()(size_t x) const { return g(f(x)); }
};
template <Arrow F, Arrow G>
constexpr Then<F, G> operator>>(F f, G g) {
  return Then<F, G>{f, g};
}
template <typename F>
constexpr bool injective = false;
template <>
constexpr bool injective<Succ> = true;
template <>
constexpr bool injective<Neg> = true;
template <Arrow F, Arrow G>
constexpr bool injective<Then<F, G>> = injective<F> && injective<G>;
template <Arrow F>
constexpr bool is_injective(F) {
  return injective<F>;
}

// Listing 2: a monoid as a carrier, an operation and its unit.
template <typename T, typename Op>
constexpr bool associative = false;
template <>
constexpr bool associative<size_t, std::plus<size_t>> = true;
template <>
constexpr bool associative<size_t, std::multiplies<size_t>> = true;
template <typename T, typename Op, T e>
concept IsMonoid = std::regular<T> && associative<T, Op> && requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
} && Op{}(e, e) == e;

// Listing 3: the two recursions.
template <typename T, typename Op, T e, std::ranges::sized_range R>
  requires IsMonoid<T, Op, e> && std::same_as<std::ranges::range_value_t<R>, T>
constexpr T concat(R r) {
  return std::ranges::fold_left(r, e, Op{});
}

// Listing 4: n copies by repeated squaring, and the 2x2 matrices over a
// semiring, their associativity inferred from the semiring of the entries.
template <typename T>
struct Power {
  T result, square;
  size_t left;
};
template <typename T, typename Op>
struct Squaring {
  constexpr Power<T> operator()(Power<T> s, size_t) const {
    return {s.left % 2 == 1 ? Op{}(s.result, s.square) : s.result,
            Op{}(s.square, s.square), s.left / 2};
  }
};
template <typename T, typename Op, T e, typename B>
  requires IsMonoid<T, Op, e>
constexpr T concat(std::ranges::repeat_view<T, B> r) {
  return std::ranges::size(r) == 0
             ? e
             : std::ranges::fold_left(
                   std::views::iota(0uz, 64uz),
                   Power<T>{e, *r.begin(), size_t(std::ranges::size(r))},
                   Squaring<T, Op>{})
                   .result;
}
template <typename T, typename Add, typename Times>
constexpr bool semiring = false;
template <>
constexpr bool semiring<size_t, std::plus<size_t>, std::multiplies<size_t>> =
    true;
template <typename T, typename Add, typename Times>
struct MatMul {
  constexpr std::array<T, 4> operator()(std::array<T, 4> a,
                                        std::array<T, 4> b) const {
    return {Add{}(Times{}(a[0], b[0]), Times{}(a[1], b[2])),
            Add{}(Times{}(a[0], b[1]), Times{}(a[1], b[3])),
            Add{}(Times{}(a[2], b[0]), Times{}(a[3], b[2])),
            Add{}(Times{}(a[2], b[1]), Times{}(a[3], b[3]))};
  }
};
template <typename T, typename Add, typename Times>
constexpr bool associative<std::array<T, 4>, MatMul<T, Add, Times>> =
    semiring<T, Add, Times>;
using M = std::array<size_t, 4>;
using Mul = MatMul<size_t, std::plus<size_t>, std::multiplies<size_t>>;
constexpr M fib{1, 1, 1, 0};
// The same value at every position, through a range that is not a
// repeat_view: the plain fold is chosen for it.
struct Fib {
  constexpr M operator()(size_t) const { return fib; }
};
template <Arrow F, Arrow G>
constexpr Then<F, G> then(F f, G g) {
  return Then<F, G>{f, g};
}
template <Arrow F, Arrow G, Arrow H>
constexpr auto then(Then<F, G> t, H h) {
  return then(t.f, then(t.g, h));
}
template <typename F>
constexpr size_t length = 1;
template <Arrow F, Arrow G>
constexpr size_t length<Then<F, G>> = length<F> + length<G>;

constexpr auto ns = std::views::reverse(std::views::iota(0uz, 10uz));
constexpr auto a = then(Succ{} >> Neg{}, Succ{});
static_assert(concat<size_t, std::plus<size_t>, 0uz>(ns) == 45 &&
              is_injective(a) && length<Then<Succ, Then<Neg, Succ>>> == 3);
}  // namespace

/** @return the fold over a value, everything known while compiling. */
extern "C" __attribute__((noinline)) std::size_t listing3_sum_known() {
  return concat<size_t, std::plus<size_t>, 0uz>(ns);
}
/** @return the same fold, the length known only at run time. */
extern "C" __attribute__((noinline)) std::size_t listing3_sum(std::size_t n) {
  return concat<size_t, std::plus<size_t>, 0uz>(
      std::views::reverse(std::views::iota(0uz, n)));
}
/** @return the same fold on the other monoid, the product of 1 to n - 1. */
extern "C" __attribute__((noinline)) std::size_t listing3_product(
    std::size_t n) {
  return concat<size_t, std::multiplies<size_t>, 1uz>(
      std::views::reverse(std::views::iota(1uz, n)));
}
/** @return the law of a term, a fold over its type. */
extern "C" __attribute__((noinline)) bool listing1_law() {
  return is_injective(a);
}
/** @return the length of a term, a fold over its type. */
extern "C" __attribute__((noinline)) std::size_t listing3_length() {
  return length<Then<Succ, Then<Neg, Succ>>>;
}
/** @return the term applied to a value known only at run time. */
extern "C" __attribute__((noinline)) std::size_t listing3_term(std::size_t x) {
  return a(x);
}
/** @return the n-th Fibonacci number: n copies of one matrix, the overload by
 *  squaring chosen because the matrices are inferred to be a monoid. */
extern "C" __attribute__((noinline)) std::size_t listing4_fib(std::size_t n) {
  return concat<M, Mul, M{1, 0, 0, 1}>(std::views::repeat(fib, n))[1];
}
/** @return the same number by the plain fold, one step for each copy. */
extern "C" __attribute__((noinline)) std::size_t listing4_fib_plain(
    std::size_t n) {
  return concat<M, Mul, M{1, 0, 0, 1}>(
      std::views::transform(std::views::iota(0uz, n), Fib{}))[1];
}
