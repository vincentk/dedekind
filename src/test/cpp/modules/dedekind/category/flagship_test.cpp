/**
 * @file flagship_test.cpp
 * @brief The flagship of the Jlt paper (#1104), in progress: periodic
 *        vectors built from the library's arrows, the period inferred through
 *        the combinators, their outer product beyond 2^128 entries, and a
 *        count over a box section, all at compile time; the law checked
 *        against the plain fold where both can run.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <algorithm>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <tuple>

import dedekind.category;

namespace flagship {
using dedekind::category::chain;
using dedekind::category::count;
using dedekind::category::equals;
using dedekind::category::fibonacci_mod;
using dedekind::category::is_even;
using dedekind::category::Mapped;
using dedekind::category::opaque;
using dedekind::category::period;
using dedekind::category::Prior;
using dedekind::category::StepCoalgebra;
using dedekind::category::unfold_view;
using std::size_t;

// A vector in the numpy sense: n >> chain<size_t>() >> f, never stored.
template <typename F>
using Vector =
    unfold_view<size_t,
                Mapped<size_t, StepCoalgebra<size_t, Prior<size_t>>, F>>;

// Still the test's: the outer product of k vectors, T(i1, ..., ik) = v1(i1)
// and ... and vk(ik), and its count over the box, by distributivity the
// product of the factors' counts, kept exact as the factors.  To become the
// library's Tensor followed by the meet (#1104).
template <typename... F>
struct Outer {
  std::tuple<Vector<F>...> vs;
};
template <typename F, typename G>
constexpr Outer<F, G> operator*(const Vector<F>& a, const Vector<G>& b) {
  return {{a, b}};
}
template <typename... F, typename G>
constexpr Outer<F..., G> operator*(const Outer<F...>& a, const Vector<G>& b) {
  return {std::tuple_cat(a.vs, std::tuple<Vector<G>>{b})};
}
struct Counts {
  template <typename... V>
  constexpr std::array<size_t, sizeof...(V)> operator()(const V&... v) const {
    return {count(v)...};
  }
};
template <typename... F>
constexpr std::array<size_t, sizeof...(F)> count_factors(const Outer<F...>& o) {
  return std::apply(Counts{}, o.vs);
}

// The library's arrows: the even counts, the even Fibonacci numbers, and the
// period inferred through composition and the involution.
constexpr auto fib_even = fibonacci_mod(2) >> equals(size_t{0});
constexpr auto odd = !is_even;
static_assert(period(is_even) == 2 && period(fib_even) == 3 && period(odd) == 2,
              "declared at the generators, inferred through >> and !");

// The law, checked against the plain fold where both can run.
constexpr size_t small = 1'000;
static_assert(count(small >> chain<size_t>() >> fib_even) ==
                  count(small >> chain<size_t>() >> opaque(fib_even)) &&
              count(small >> chain<size_t>() >> odd) ==
                  count(small >> chain<size_t>() >> opaque(odd)));

// The exhibit: five vectors of 2^60 entries, 2^300 in the box.
constexpr size_t n = size_t{1} << 60;
constexpr auto evens = n >> chain<size_t>() >> is_even;
constexpr auto odds = n >> chain<size_t>() >> odd;
constexpr auto fibs = n >> chain<size_t>() >> fib_even;
constexpr auto box = evens * odds * fibs * evens * fibs;
static_assert(count_factors(box) ==
              std::array<size_t, 5>{n / 2, n / 2, n / 3 + 1, n / 2, n / 3 + 1});
}  // namespace flagship

TEST_CASE("Flagship: a count over a vector of 2^60 entries, at run time",
          "[category][flagship]") {
  using std::size_t;
  size_t lo = 60;
  const size_t n = size_t{1} << lo;
  CHECK(flagship::count(n >> dedekind::category::chain<size_t>() >>
                        dedekind::category::is_even) == n / 2);
}
