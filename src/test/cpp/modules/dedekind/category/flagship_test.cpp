/**
 * @file flagship_test.cpp
 * @brief The flagship of the Jlt paper (#1104), in progress: periodic
 *        vectors built from the library's arrows, the laws inferred through
 *        the combinators, their outer product beyond 2^128 entries, and its
 *        counts, over the box, over a window and over a modular section, all
 *        at compile time; each law checked against the plain fold where both
 *        can run.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <algorithm>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <ranges>
#include <utility>

import dedekind.category;

namespace flagship {
using dedekind::category::chain;
using dedekind::category::count;
using dedekind::category::count_factors;
using dedekind::category::count_section;
using dedekind::category::equals;
using dedekind::category::fibonacci_mod;
using dedekind::category::is_even;
using dedekind::category::opaque;
using dedekind::category::period;
using dedekind::category::rolling_count;
using dedekind::category::shift;
using dedekind::category::sum;
using std::size_t;

// The library's arrows: the even counts, the even Fibonacci numbers, and the
// periods inferred through composition, the involution, a join and a window.
constexpr auto fib_even = fibonacci_mod(2) >> equals(size_t{0});
constexpr auto odd = !is_even;
constexpr auto near = shift(1) >> fib_even | shift(2) >> fib_even;
static_assert(period(is_even) == 2 && period(fib_even) == 3 &&
                  period(odd) == 2 && period(near) == 3,
              "declared at the generators, inferred through >>, ! and |");

// The brute force over a two-leg box at small sizes: every entry, evaluated
// through the box's own entry arrow, the Tensor of the legs and their meet.
template <typename E>
struct Row {
  E e;
  size_t i;
  size_t m;
  size_t r;
  constexpr size_t operator()(size_t c, size_t j) const {
    const bool in = m == 0 || (i + j) % m == r;
    return c + (in && e(std::pair{i, j}) ? 1 : 0);
  }
};
template <typename E>
struct Rows {
  E e;
  size_t nb;
  size_t m;
  size_t r;
  constexpr size_t operator()(size_t c, size_t i) const {
    return std::ranges::fold_left(std::views::iota(size_t{0}, nb), c,
                                  Row<E>{e, i, m, r});
  }
};
template <typename B>
constexpr size_t brute(const B& b, size_t m, size_t r) {
  return std::ranges::fold_left(
      std::views::iota(size_t{0}, b.sides[0]), size_t{0},
      Rows<decltype(b.entry)>{b.entry, b.sides[1], m, r});
}

// The laws, checked against the brute force where both can run: the box, its
// modular section, and the plain fold of a window.
constexpr auto small =
    (40uz >> chain<size_t>() >> near) * (30uz >> chain<size_t>() >> odd);
static_assert(count(small) == brute(small, 0, 0) &&
              count_section(small, 3, 0) == brute(small, 3, 0) &&
              count_section(small, 5, 2) == brute(small, 5, 2));
static_assert(sum(1'000uz >> chain<size_t>() >> rolling_count(near, 4)) ==
              sum(1'000uz >> chain<size_t>() >>
                  opaque(rolling_count(near, 4))));

// The exhibit: five vectors of 2^60 entries, 2^300 in the box.
constexpr size_t n = size_t{1} << 60;
constexpr auto evens = n >> chain<size_t>() >> is_even;
constexpr auto odds = n >> chain<size_t>() >> odd;
constexpr auto fibs = n >> chain<size_t>() >> fib_even;
constexpr auto box = evens * odds * fibs * evens * fibs;
static_assert(count_factors(box) ==
              std::array<size_t, 5>{n / 2, n / 2, n / 3 + 1, n / 2, n / 3 + 1});
static_assert(count_section(evens* odds, 2, 0) == 0 &&
                  count_section(evens * odds, 2, 1) == count(evens * odds),
              "an even and an odd index sum to an odd one: the section "
              "i + j ≡ 0 (mod 2) is empty, and i + j ≡ 1 (mod 2) is the box");
}  // namespace flagship

TEST_CASE("Flagship: a count over a vector of 2^60 entries, at run time",
          "[category][flagship]") {
  using std::size_t;
  size_t lo = 60;
  const size_t n = size_t{1} << lo;
  CHECK(flagship::count(n >> dedekind::category::chain<size_t>() >>
                        dedekind::category::is_even) == n / 2);
}
