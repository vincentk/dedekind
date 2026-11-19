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
#include <bit>
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
  T product, base;
  size_t exponent;
};
// One step of the power by squaring: product * base^exponent stays what it
// was while the exponent is halved.  The step names the calls it makes: its
// constraint is the monoid.
template <typename T, typename Op, T e>
struct Halving {
  constexpr Power<T> operator()(Power<T> p, size_t) const
    requires IsMonoid<T, Op, e>
  {
    return {p.exponent % 2 == 1 ? Op{}(p.product, p.base) : p.product,
            Op{}(p.base, p.base), p.exponent / 2};
  }
};
template <typename T, typename Op, T e>
  requires IsMonoid<T, Op, e>
constexpr T power(T x, size_t n) {
  return std::ranges::fold_left(
             std::views::iota(0uz, size_t(std::bit_width(n))),
             Power<T>{e, x, n}, Halving<T, Op, e>{})
      .product;
}
// A law of the standard's types, not of the program: a repeat_view holds one
// value at every position.  It is what selects the overload below, where
// associativity is what makes it correct.
template <typename R>
constexpr bool constant = false;
template <typename T>
constexpr bool constant<std::ranges::repeat_view<T, size_t>> = true;
template <typename R>
concept IsConstant = std::ranges::sized_range<R> && constant<R>;
template <typename T, typename Op, T e, IsConstant R>
  requires IsMonoid<T, Op, e> && std::same_as<std::ranges::range_value_t<R>, T>
constexpr T concat(R r) {
  return std::ranges::size(r) == 0
             ? e
             : power<T, Op, e>(*std::ranges::begin(r), std::ranges::size(r));
}
// The monoid pattern one level up.  Two more laws, each a trait with its
// entries, and a shape that names them beside the two monoids: a semiring.
template <typename T, typename Op>
constexpr bool commutative = false;
template <typename T, typename Times, typename Add>
constexpr bool distributes = false;
template <>
constexpr bool commutative<size_t, std::plus<size_t>> = true;
template <>
constexpr bool distributes<size_t, std::multiplies<size_t>, std::plus<size_t>> =
    true;
template <typename T, T zero, T one>
concept IsSemiring =
    IsMonoid<T, std::plus<T>, zero> && IsMonoid<T, std::multiplies<T>, one> &&
    commutative<T, std::plus<T>> &&
    distributes<T, std::multiplies<T>, std::plus<T>>;
// The 2x2 matrices over a semiring under the type's own + and *, as a type
// with a product of its own, so that std::multiplies names that product.  The
// product's body writes the operators, and the shape names std::plus<T> and
// std::multiplies<T>, which the standard defines as those operators.
template <typename T, T zero, T one>
struct Mat {
  T a, b, c, d;
  constexpr bool operator==(const Mat&) const = default;
  constexpr Mat operator*(Mat y) const
    requires IsSemiring<T, zero, one>
  {
    return {a * y.a + b * y.c, a * y.b + b * y.d, c * y.a + d * y.c,
            c * y.b + d * y.d};
  }
};
// The inferred law: the matrices' product is associative for a semiring T.
template <typename T, T zero, T one>
  requires IsSemiring<T, zero, one>
constexpr bool
    associative<Mat<T, zero, one>, std::multiplies<Mat<T, zero, one>>> = true;
using M = Mat<size_t, 0uz, 1uz>;
using Mul = std::multiplies<M>;
constexpr M identity{1, 0, 0, 1};
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
  return concat<M, Mul, identity>(std::views::repeat(fib, n)).b;
}
/** @return the same number by the plain fold, one step for each copy. */
extern "C" __attribute__((noinline)) std::size_t listing4_fib_plain(
    std::size_t n) {
  return concat<M, Mul, identity>(
             std::views::transform(std::views::iota(0uz, n), Fib{}))
      .b;
}
