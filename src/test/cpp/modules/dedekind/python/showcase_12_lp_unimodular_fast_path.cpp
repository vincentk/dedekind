/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_12_lp_unimodular_fast_path.cpp
 * @brief Showcase 12 — Compile-time LP via the bit-ops fast path on a
 *        signed-unimodular axis-aligned pack.
 *
 *   maximize x + y
 *   subject to  x  ≤ 1    (U1)
 *               y  ≤ 1    (U2)
 *              -x  ≤ 0    (U3:  x ≥ 0)
 *                -y ≤ 0   (U4:  y ≥ 0)
 *   ⇒ optimum (1, 1), objective value = 2.
 *
 * @section Description
 * Every halfspace of the pack has @c (a, b) ∈ {−1, 0, +1}² with one
 * coefficient zero, so @c maximize_with_values dispatches to the bit-ops
 * kernel rather than the Cramer kernel of @c showcase_09.  Called with
 * constant arguments it is evaluated at compile time, on values; the
 * witness functions below compile to @c ret @c i64 @c 1 at @c -O2.
 *
 * Companion: @c showcase_12b (runtime call site of the same fast-path
 * kernel), which exhibits the bit-ops IR without compile-time evaluation.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <array>
#include <cstdint>
#include <span>

import dedekind.linear_algebra;
import dedekind.optimization;

using dedekind::linear_algebra::Vec2V;
using dedekind::optimization::HalfspaceTriple;
using dedekind::optimization::maximize_with_values;

namespace {
constexpr std::array<HalfspaceTriple<long>, 4> kUnitSquare = {{
    {1L, 0L, 1L},   //  x ≤ 1
    {0L, 1L, 1L},   //  y ≤ 1
    {-1L, 0L, 0L},  //  x ≥ 0
    {0L, -1L, 0L},  //  y ≥ 0
}};
constexpr auto kOptimum = maximize_with_values<long>(
    std::span<const HalfspaceTriple<long>>(kUnitSquare), 1L, 1L);
}  // namespace

static_assert(kOptimum.predicate.feasible);
static_assert(kOptimum.predicate.point.x == 1L &&
              kOptimum.predicate.point.y == 1L);
static_assert(kOptimum(Vec2V<long>{1L, 1L}));

/** @brief Showcase 12a: the fast-path optimum's x-coordinate.  Expected IR:
 *  `ret i64 1`. */
extern "C" __attribute__((noinline)) int64_t witness_lp_fast_optimum_x() {
  return kOptimum.predicate.point.x;
}

/** @brief Showcase 12b: the fast-path optimum's y-coordinate.  Expected IR:
 *  `ret i64 1`. */
extern "C" __attribute__((noinline)) int64_t witness_lp_fast_optimum_y() {
  return kOptimum.predicate.point.y;
}
