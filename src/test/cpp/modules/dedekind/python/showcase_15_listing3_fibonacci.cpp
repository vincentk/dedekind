/**
 * @file src/test/cpp/modules/dedekind/python/showcase_15_listing3_fibonacci.cpp
 * @brief Showcase 15 — the Jlt paper's Listing 3 at run time: Fibonacci as a
 *        fold over a chain, the count known only when the program runs.
 *
 * Listing 3 folds the matrix action over @c n @c >> @c chain<size_t>(), a
 * chain of counts, from the vector (1, 0).  Here @c n is a run-time argument,
 * so the fold survives as the one loop of the function, and Appendix G reads
 * its IR: the loop header's φ-nodes are the fold's state, the two components
 * of the vector and the count.  The function is @c extern @c "C" and never
 * inlined, so it is its own symbol in the IR the fixture script reads, and
 * the loop is kept as the fold wrote it, neither unrolled nor vectorised.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <array>
#include <cstddef>

import dedekind.category;

using dedekind::category::chain;
using dedekind::category::constant;
using dedekind::category::fold;

namespace {
// Listing 3: matrices on vectors, row by row.
using V = std::array<std::size_t, 2>;
struct M {
  V top, bottom;
  friend constexpr V operator*(V v, M a) {  // v·A
    return {v[0] * a.top[0] + v[1] * a.bottom[0],
            v[0] * a.top[1] + v[1] * a.bottom[1]};
  }
};
constexpr auto act = [](V v, M a) { return v * a; };
constexpr M fib{{1, 1}, {1, 0}};

/** @return F(n), the fold of Listing 3 over a chain of n counts. */
constexpr std::size_t fibonacci(std::size_t n) {
  return fold(n >> chain<std::size_t>() >> constant<std::size_t>(fib), V{1, 0},
              act)[1];
}
static_assert(fibonacci(0) == 0 && fibonacci(10) == 55);
}  // namespace

/** @return the same fold, its count known only at run time: an emitted
 *  symbol, since a constexpr function is inline and would not be. */
extern "C" __attribute__((noinline)) std::size_t listing3_fibonacci(
    std::size_t n) {
  return fibonacci(n);
}
