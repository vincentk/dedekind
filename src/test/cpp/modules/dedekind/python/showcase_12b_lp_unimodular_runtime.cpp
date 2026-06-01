/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_12b_lp_unimodular_runtime.cpp
 * @brief Showcase 12b — Runtime call site of the bit-ops fast-path kernel.
 *        Companion IR fixture to @c showcase_12 (compile-time NTTP form).
 *
 * @section Description
 * Showcase 12 (@c showcase_12_lp_unimodular_fast_path.cpp) folds the
 * fast-path reduction to a typed constant @c Vec2<Rat, 1, 1> via the
 * NTTP entry @c maximize<>(); the witness IR is a literal load.
 *
 * This fixture calls the @em same kernel through the runtime entry
 * point @c maximize_axis_aligned_with_values<int>(span, cx, cy) — the
 * coefficients arrive as function arguments, not known at translation
 * time.  At @c -O2 the emitted IR retains the fast-path algorithm
 * (per-axis bound bracketing + sign-driven vertex selection) without
 * folding away.  Read side-by-side with @c showcase_09b.ll (generic
 * Cramer runtime: @c MUL, @c DIV, full active-set enumeration) and
 * @c showcase_12.ll (compile-time fold), this fixture is the third
 * panel of the §5 triptych: bit-ops IR for an axis-aligned
 * signed-unimodular pack, with @c MUL and @c DIV absent on the
 * carrier.
 *
 * Carrier is @c int for IR readability — integer compares, integer
 * negation, integer select; no @c FMUL or @c FDIV obscuring the
 * structural claim.  The kernel is carrier-parametric over
 * @c detail::SuitableForAxisAlignedFastPath<T> , which names every
 * operation the kernel uses ( @c T{} , @c T{1} , ring operators,
 * ordering, signedness via @c (T{} @c − @c T{1}) @c < @c T{} ).  The
 * same runtime entry works for any carrier satisfying that concept.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <cstddef>
#include <span>

import dedekind.optimization;

using dedekind::optimization::HalfspaceTriple;
using dedekind::optimization::maximize_axis_aligned_with_values;

/**
 * @note IR microscope.  This fixture calls the explicit power-user
 * runtime entry @c maximize_axis_aligned_with_values rather than the
 * mechanically-dispatching default @c maximize_with_values .  The
 * default routes via a runtime scan + branch; its IR contains the
 * scan code and both kernels, which pollutes the bit-ops signature
 * this fixture exists to exhibit.  The explicit power-user entry
 * goes straight to the fast-path kernel — IR microscope on the
 * kernel in isolation.  Library default users call
 * @c maximize_with_values and let the type system pick.
 */

/**
 * @brief Optimum's x-coordinate from a runtime axis-aligned polytope.
 *
 * Expected IR shape at @c -O2: per-axis bound tracking loop (compares
 * + conditional updates) and a final sign-driven select between
 * @c x_lo and @c x_hi.  No @c MUL, no @c DIV on the carrier.
 */
extern "C" __attribute__((noinline)) int witness_lp_axis_aligned_x(
    const HalfspaceTriple<int>* hs, std::size_t n, int cx, int cy) {
  return maximize_axis_aligned_with_values<int>(
             std::span<const HalfspaceTriple<int>>(hs, n), cx, cy)
      .predicate()
      .point.x;
}

/** @brief Companion y-coordinate witness; structurally identical IR. */
extern "C" __attribute__((noinline)) int witness_lp_axis_aligned_y(
    const HalfspaceTriple<int>* hs, std::size_t n, int cx, int cy) {
  return maximize_axis_aligned_with_values<int>(
             std::span<const HalfspaceTriple<int>>(hs, n), cx, cy)
      .predicate()
      .point.y;
}

/**
 * @brief Feasibility flag from a runtime axis-aligned polytope.
 *
 * The runtime entry reports feasibility through a flag rather than
 * the NTTP path's @c static_assert; for the bit-ops fast path this
 * corresponds to "all four axis bounds present and @c lo @c ≤ @c hi".
 */
extern "C" __attribute__((noinline)) bool witness_lp_axis_aligned_feasible(
    const HalfspaceTriple<int>* hs, std::size_t n, int cx, int cy) {
  return maximize_axis_aligned_with_values<int>(
             std::span<const HalfspaceTriple<int>>(hs, n), cx, cy)
      .predicate()
      .feasible;
}
