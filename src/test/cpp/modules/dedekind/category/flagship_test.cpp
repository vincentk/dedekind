/**
 * @file flagship_test.cpp
 * @brief The flagship of the Jlt paper (#1104), a first attempt: periodic
 *        vectors, laws inferred through the combinators that build them,
 *        their outer product beyond 2^128 entries, and a count over a box
 *        section, all at compile time.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <algorithm>
#include <array>
#include <bit>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <ranges>
#include <type_traits>

import dedekind.category;

namespace flagship {
using dedekind::category::chain;
using dedekind::category::Compose;
using dedekind::category::Mapped;
using dedekind::category::Prior;
using dedekind::category::StepCoalgebra;
using dedekind::category::unfold_view;
using std::size_t;
using dedekind::category::operator>>;
using dedekind::category::operator!;  // ADL does not reach it from here

// A vector in the numpy sense: n >> chain<size_t>() >> f, the values f(k) for
// k below n, never stored.
template <typename F>
using Vector =
    unfold_view<size_t,
                Mapped<size_t, StepCoalgebra<size_t, Prior<size_t>>, F>>;

// The law: an arrow on the counts is periodic with period p, f(k + p) = f(k).
// A declared certificate, 0 where none is declared.
template <typename F>
struct period : std::integral_constant<size_t, 0> {};
template <typename F>
inline constexpr size_t period_v = period<std::remove_cvref_t<F>>::value;

// Two generators.  Parity of the counts, period 2.
struct IsEven {
  using Domain = size_t;
  using Codomain = bool;
  constexpr bool operator()(size_t k) const { return k % 2 == 0; }
};
// Parity of the Fibonacci numbers, F(k) mod 2 by fast doubling over the bits
// of k; its period, the Pisano period 3, is a theorem, declared here.
struct FibParity {
  using Domain = size_t;
  using Codomain = bool;
  struct Double {  // (F(m), F(m+1)) mod 2 to (F(2m [+1]), ...) mod 2
    size_t k;
    constexpr std::array<bool, 2> operator()(std::array<bool, 2> p,
                                             size_t i) const {
      const bool a = p[0], b = p[1];
      const bool c = a;       // F(2m) = F(m)(2F(m+1) - F(m)), mod 2 F(m)^2
      const bool d = a != b;  // F(2m+1) = F(m)^2 + F(m+1)^2, mod 2 a + b
      return ((k >> i) & 1U) != 0 ? std::array<bool, 2>{d, c != d}
                                  : std::array<bool, 2>{c, d};
    }
  };
  constexpr bool operator()(size_t k) const {
    const size_t w = static_cast<size_t>(std::bit_width(k));
    const auto bits = std::views::iota(size_t{0}, w) | std::views::reverse;
    return !std::ranges::fold_left(bits, std::array<bool, 2>{false, true},
                                   Double{k})[0];
  }
};
template <>
struct period<IsEven> : std::integral_constant<size_t, 2> {};
template <>
struct period<FibParity> : std::integral_constant<size_t, 3> {};

// Inferred, not declared (E-Law-Ctx): an arrow after a periodic one is
// periodic with the same period, whatever the second arrow is.
template <typename F, typename G>
  requires(period_v<F> > 0)
struct period<Compose<F, G>> : period<F> {};

static_assert(FibParity{}(0) && !FibParity{}(1) && !FibParity{}(2) &&
              FibParity{}(3) && FibParity{}(6) && !FibParity{}(10));

// The count of a periodic vector: (n div p) periods and a remainder, by the
// semimodule law of the counts, n copies of one period's count are n times it.
struct Count {  // the Iverson bracket, 𝔹 to ℕ, a semiring homomorphism
  constexpr size_t operator()(size_t m, bool b) const { return m + b; }
};
template <typename F>
  requires(period_v<F> > 0)
constexpr size_t count(const Vector<F>& v) {
  const size_t n = v.seed(), p = period_v<F>;
  const F f = v.coalgebra().f;
  const auto first = [&](size_t m) {
    return std::ranges::fold_left(
        std::views::iota(size_t{0}, m) | std::views::transform(f), size_t{0},
        Count{});
  };
  return (n / p) * first(p) + first(n % p);
}

// The outer product of k vectors, T(i1, ..., ik) = v1(i1) and ... and vk(ik),
// and its count over the box of all indices: by distributivity, the product
// of the factors' counts, kept exact as the factors, and its residue.
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
template <typename... F>
constexpr std::array<size_t, sizeof...(F)> count_factors(const Outer<F...>& o) {
  return std::apply(
      [](const auto&... v) {
        return std::array<size_t, sizeof...(F)>{count(v)...};
      },
      o.vs);
}
template <typename... F>
constexpr size_t count_residue(const Outer<F...>& o) {
  const auto fs = count_factors(o);
  return std::ranges::fold_left(fs, size_t{1}, std::multiplies<size_t>{});
}

// The exhibit: five vectors of 2^60 entries, 2^300 in the box.
constexpr size_t n = size_t{1} << 60;
constexpr auto evens = n >> chain<size_t>() >> IsEven{};
constexpr auto odds = n >> chain<size_t>() >> !IsEven{};
constexpr auto fib_even = n >> chain<size_t>() >> FibParity{};
constexpr auto box = evens * odds * fib_even * evens * fib_even;

static_assert(period_v<decltype(!IsEven{})> == 2,
              "the period survives the involution: inferred, not declared");
static_assert(count(evens) == n / 2 && count(odds) == n / 2 &&
              count(fib_even) == n / 3 + 1);
static_assert(count_factors(box) ==
              std::array<size_t, 5>{n / 2, n / 2, n / 3 + 1, n / 2, n / 3 + 1});
static_assert(count_residue(box) ==
              (n / 2) * (n / 2) * (n / 3 + 1) * (n / 2) * (n / 3 + 1));
}  // namespace flagship

TEST_CASE("Flagship: a count over a box of 2^300 entries, at run time",
          "[category][flagship]") {
  using std::size_t;
  size_t lo = 60;
  const size_t n = size_t{1} << lo;
  const auto v = n >> dedekind::category::chain<size_t>() >> flagship::IsEven{};
  CHECK(flagship::count(v) == n / 2);
}
