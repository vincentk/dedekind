/**
 * @file periodic_test.cpp
 * @brief The laws of @c :periodic at compile time: the periods declared and
 *        inferred, the counts by the laws over 2^60, and the plain fold
 *        agreeing with them where both can run (#1104).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <limits>
#include <ranges>

import dedekind.category;

using namespace dedekind::category;

namespace periodic_witness {
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
static_assert(count(200uz >> chain<std::size_t>() >> (is_even | fib_even)) ==
                      count(200uz >> chain<std::size_t>() >>
                            opaque(is_even | fib_even)) &&
                  sum(200uz >> chain<std::size_t>() >>
                      rolling_count(fib_even, 3)) ==
                      sum(200uz >> chain<std::size_t>() >>
                          opaque(rolling_count(fib_even, 3))),
              "the laws agree with the plain fold where both can run");
static_assert(count_factors((n >> chain<std::size_t>() >> is_even) *
                            (n >> chain<std::size_t>() >> fib_even)) ==
                  std::array<std::size_t, 2>{n / 2, n / 3 + 1},
              "the box's count factorises, leg by leg");
// A chain restricted by value, [lo, hi): by the law, two counts whatever the
// bounds, checked against the plain fold where both can run.
static_assert(count(slice(256uz, 512uz) >> fib_even) ==
                  count(slice(256uz, 512uz) >> opaque(fib_even)) &&
              count(slice(n / 2, n) >> is_even) == n / 4 &&
              count(slice(5uz, 5uz) >> is_even) == 0 &&
              count(slice(7uz, 5uz) >> is_even) == 0);

// The complement of a join keeps its period, so it is counted by the law over
// 2^60; and no word narrower than int, whose products would promote to it.
static_assert(period(!(is_even | fib_even)) == 6 &&
              count(every >> !(is_even | fib_even)) ==
                  n - (n / 2 + n / 3 - n / 6) &&
              IsCountWord<unsigned> && !IsCountWord<unsigned short> &&
              !IsCountWord<bool>);

// Generic in the counts: the same laws on another word, its own type kept.
static_assert(
    count(1'000u >> chain<unsigned>() >> congruent<unsigned>(3, 1)) == 333u &&
    std::same_as<decltype(period(congruent<unsigned>(3, 1))), unsigned> &&
    period(fibonacci_mod<unsigned>(10)) == 60u);

// The widest modulus a word admits, 2^(w/2), agrees with a wider word; a
// default view of a slice is empty, as its plain fold.
static_assert(fibonacci_mod<unsigned>(65'536u)(50u) == 14'689u &&
              fibonacci_mod(65'536)(50) == 14'689 &&
              count(decltype(slice(0uz, 10uz) >> is_even){}) == 0);
// Copies are counted in the chain's own word, not only std::size_t: n
// copies along a chain of unsigned, or a repeat bounded by an int, are a
// power, grown from its prefixes.
static_assert(
    IsCopies<decltype(1'000u >> chain<unsigned>() >> constant<unsigned>(3u))> &&
    concat(1'000u >> chain<unsigned>() >> constant<unsigned>(3u)) == 3'000u &&
    std::same_as<decltype(copies_count(std::views::repeat(7uz, 10))),
                 unsigned> &&
    concat(std::views::repeat(7uz, 10)) == 70);
}  // namespace periodic_witness

TEST_CASE("Periodic: a count by the law at run time", "[category][periodic]") {
  std::size_t lo = 60;
  const std::size_t n = std::size_t{1} << lo;
  CHECK(count(n >> chain<std::size_t>() >> is_even) == n / 2);
}
