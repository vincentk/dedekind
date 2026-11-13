/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_09_lp_vertex_typed_constant.cpp
 * @brief Showcase 9 — Compile-time LP: the optimum is a constant.
 *
 *   maximize 3x + 2y
 *   subject to  x +  y ≤ 4       (H1)   non-axis-aligned
 *               2x +  y ≤ 6       (H2)   non-axis-aligned
 *              -x       ≤ 0       (H3:  x ≥ 0)
 *                   -y  ≤ 0       (H4:  y ≥ 0)
 *   ⇒ optimum (2, 2), objective value = 10.
 *
 * The kernel enumerates the vertex candidates (all `C(4, 2) = 6` pairs of
 * binding constraints), solves each `2×2` active set by Cramer's rule,
 * filters by feasibility against the other constraints, and keeps the
 * objective-maximising feasible vertex.  The active set at the optimum is
 * `{H1, H2}` — both non-axis-aligned — so the LP isn't decided by
 * axis-alignment.  Called with constant arguments the kernel is evaluated
 * at compile time, on values: the optimum is a constant, checked below by
 * `static_assert`.
 *
 * Expected LLVM IR: `ret i64 2` for `witness_lp_optimum_x` and
 *                   `ret i64 2` for `witness_lp_optimum_y` — no LP solver,
 * no active-set iteration survives into the object file.
 *
 * The scalar is the word, exact on this instance: the active sets'
 * determinants are ±1, so Cramer's division is exact in `long`.
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
constexpr std::array<HalfspaceTriple<long>, 4> kPolytope = {{
    {1L, 1L, 4L},   //  x +  y ≤ 4
    {2L, 1L, 6L},   // 2x +  y ≤ 6
    {-1L, 0L, 0L},  //  x      ≥ 0
    {0L, -1L, 0L},  //       y ≥ 0
}};
constexpr auto kOptimum = maximize_with_values<long>(
    std::span<const HalfspaceTriple<long>>(kPolytope), 3L, 2L);
}  // namespace

static_assert(kOptimum.predicate.feasible);
static_assert(kOptimum.predicate.point.x == 2L &&
              kOptimum.predicate.point.y == 2L);
static_assert(kOptimum(Vec2V<long>{2L, 2L}));

/** @brief Showcase 9a: the optimum's x-coordinate.  Expected IR:
 *  `ret i64 2`. */
extern "C" __attribute__((noinline)) int64_t witness_lp_optimum_x() {
  return kOptimum.predicate.point.x;
}

/** @brief Showcase 9b: the optimum's y-coordinate.  Expected IR:
 *  `ret i64 2`. */
extern "C" __attribute__((noinline)) int64_t witness_lp_optimum_y() {
  return kOptimum.predicate.point.y;
}
