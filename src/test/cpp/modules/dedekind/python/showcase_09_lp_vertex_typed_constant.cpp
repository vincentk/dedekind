/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_09_lp_vertex_typed_constant.cpp
 * @brief Showcase 9 — Compile-time LP: the optimum is a typed constant.
 *
 *   maximize 3x + 2y
 *   subject to  x +  y ≤ 4       (H1)   non-axis-aligned
 *               2x +  y ≤ 6       (H2)   non-axis-aligned
 *              -x       ≤ 0       (H3:  x ≥ 0)
 *                   -y  ≤ 0       (H4:  y ≥ 0)
 *   ⇒ optimum = Vec2<Rat, 2, 2>, objective value = 10.
 *
 * The reduction `maximize<Rat, cx, cy, Hs...>()` enumerates vertex
 * candidates (all `C(4, 2) = 6` pairs of binding constraints), solves
 * each `2×2` active set via `Invertible2x2`'s closed-form Cramer inverse,
 * filters by feasibility against non-active constraints, and picks the
 * objective-maximising feasible vertex. The active set at the optimum is
 * `{H1, H2}` — both non-axis-aligned — giving an honest Gurobi-shaped LP
 * that isn't trivially decided by axis-alignment.
 *
 * Expected LLVM IR: `ret i64 2` for `witness_lp_optimum_x` and
 *                   `ret i64 2` for `witness_lp_optimum_y`.
 * The numerator of the optimum's x- and y-coordinate is folded to the
 * constant 2 by the compiler — no runtime LP solver, no simplex
 * iteration, no tableau manipulation. The vertex IS a type, and the
 * return value IS a literal in the emitted object file.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>
#include <cstdint>

import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.optimization;
import dedekind.sets;

using namespace dedekind::linear_algebra;
using namespace dedekind::numbers;
using namespace dedekind::optimization;
using dedekind::sets::SignedExtensionalCardinal;

// Arbitrary-precision signed rational: the canonical ℚ. Overflow-safe
// to 2^{64·N−1} in a single limb, eliminating the signed-long hazard.
using Rat = Rational<SignedExtensionalCardinal<>>;

// Constraints of the paper-facing LP instance.
using H1 = Halfspace2D<Rat, Rat{1}, Rat{1}, Rat{4}>;    //  x +  y ≤ 4
using H2 = Halfspace2D<Rat, Rat{2}, Rat{1}, Rat{6}>;    // 2x +  y ≤ 6
using H3 = Halfspace2D<Rat, Rat{-1}, Rat{0}, Rat{0}>;   //  x      ≥ 0
using H4 = Halfspace2D<Rat, Rat{0}, Rat{-1}, Rat{0}>;   //       y ≥ 0

// The optimum at the type level: `Vec2<Rat, Rat{2}, Rat{2}>`.
using Optimum = decltype(maximize<Rat, Rat{3}, Rat{2}, H1, H2, H3, H4>());

// Type-level witnesses: the optimum IS the vertex (2, 2).
static_assert(std::same_as<Optimum, Vec2<Rat, Rat{2}, Rat{2}>>);
static_assert(Optimum::first == Rat{2});
static_assert(Optimum::second == Rat{2});

/**
 * @brief Showcase 9a: the optimum's x-coordinate as a compile-time
 *        literal in the emitted IR.
 *
 * Expected IR: `ret i64 2` — the compiler folds the entire LP
 * reduction (6 candidate solves, 6 feasibility checks, 6 objective
 * comparisons) to the literal numerator of `Rat{2L}` at `-O2`.
 */
extern "C" __attribute__((noinline)) int64_t witness_lp_optimum_x() {
  return static_cast<int64_t>(Optimum::first.num());
}

/**
 * @brief Showcase 9b: the optimum's y-coordinate.
 *
 * Expected IR: `ret i64 2`.
 */
extern "C" __attribute__((noinline)) int64_t witness_lp_optimum_y() {
  return static_cast<int64_t>(Optimum::second.num());
}
