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

// Listing 2: a monoid on size_t as an operation and its unit.
template <typename Op, size_t e>
constexpr bool monoid = false;
template <>
constexpr bool monoid<std::plus<size_t>, 0uz> = true;
template <>
constexpr bool monoid<std::multiplies<size_t>, 1uz> = true;
template <typename Op, size_t e>
concept IsMonoid = monoid<Op, e> && requires(size_t a, size_t b) {
  { Op{}(a, b) } -> std::same_as<size_t>;
};

// Listing 3: the two recursions.
template <typename Op, size_t e, std::ranges::sized_range R>
  requires IsMonoid<Op, e> &&
           std::same_as<std::ranges::range_value_t<R>, size_t>
constexpr size_t concat(R r) {
  return std::ranges::fold_left(r, e, Op{});
}
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
static_assert(concat<std::plus<size_t>, 0uz>(ns) == 45 && is_injective(a) &&
              length<Then<Succ, Then<Neg, Succ>>> == 3);
}  // namespace

/** @return the fold over a value, everything known while compiling. */
extern "C" __attribute__((noinline)) std::size_t listing3_sum_known() {
  return concat<std::plus<size_t>, 0uz>(ns);
}
/** @return the same fold, the length known only at run time. */
extern "C" __attribute__((noinline)) std::size_t listing3_sum(std::size_t n) {
  return concat<std::plus<size_t>, 0uz>(
      std::views::reverse(std::views::iota(0uz, n)));
}
/** @return the same fold on the other monoid, the product of 1 to n - 1. */
extern "C" __attribute__((noinline)) std::size_t listing3_product(
    std::size_t n) {
  return concat<std::multiplies<size_t>, 1uz>(
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
