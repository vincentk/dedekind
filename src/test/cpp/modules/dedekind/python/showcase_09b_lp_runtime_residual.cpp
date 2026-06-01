/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_09b_lp_runtime_residual.cpp
 * @brief Showcase 9b — Runtime call site of the same LP kernel as
 *        showcase_09 (compile-time NTTP form).  Companion IR fixture.
 *
 * @section Description
 * Showcase 9 (`showcase_09_lp_vertex_typed_constant.cpp`) calls
 * `detail::maximize_impl<Rat>` at compile time through the NTTP
 * packaging `maximize<Rat, cx, cy, Hs...>()`; the compiler folds the
 * full active-set enumeration to a typed constant `Vec2<Rat, 2, 2>` and
 * the emitted IR for `witness_lp_optimum_x` is `ret i64 2`.
 *
 * This fixture calls the *same* kernel through the power-user runtime
 * entry `maximize_cramer_with_values<double>(span, cx, cy)` with the
 * coefficients arriving as function arguments (not known at
 * translation time).  At `-O2` the emitted IR retains the algorithm's
 * residual structure: the C(n, 2) active-set enumeration, the 2×2
 * Cramer determinant + solve, the feasibility loop, and the argmax —
 * all visible, none folded away.  Read side-by-side with
 * `showcase_09_lp_vertex_typed_constant.ll` this exhibits the bridge
 * in IR form: one `constexpr` kernel, two evaluation modes, selection
 * by the constexpr-ness of the call site.
 *
 * The fixture uses the explicit `maximize_cramer_with_values` rather
 * than the default `maximize_with_values` because the default does
 * mechanical dispatch (runtime scan + branch); its IR contains both
 * kernels and the scan code, which pollutes the residual-Cramer
 * signature this fixture exists to exhibit.  Calling the explicit
 * power-user entry is the IR microscope on the Cramer kernel in
 * isolation; library default users call `maximize_with_values` and
 * let the type system pick.
 *
 * Carrier is `double` for IR readability; the kernel is carrier-
 * parametric (see `src/test/python/showcase_09_lp_runtime.py` for the
 * same call site instantiated at `T = Dual<Rational>` — primal AND
 * first-order sensitivity, ℚ-exact, the only LP Python binding).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <cstddef>
#include <span>

import dedekind.optimization;

using dedekind::optimization::HalfspaceTriple;
using dedekind::optimization::maximize_cramer_with_values;

/**
 * @brief Optimum's x-coordinate from a runtime polytope, solved by the
 *        generic Cramer kernel.
 *
 * Expected IR shape at -O2: residual loop nest over the C(n, 2)
 * candidate active sets, 2×2 Cramer solve per pair, feasibility check
 * against the full constraint set, scalar argmax accumulator.  Nothing
 * folded — the coefficients are runtime arguments.
 */
extern "C" __attribute__((noinline)) double witness_lp_runtime_x(
    const HalfspaceTriple<double>* hs, std::size_t n, double cx, double cy) {
  return maximize_cramer_with_values<double>(
             std::span<const HalfspaceTriple<double>>(hs, n), cx, cy)
      .predicate()
      .point.x;
}

/** @brief Companion y-coordinate witness; structurally identical IR. */
extern "C" __attribute__((noinline)) double witness_lp_runtime_y(
    const HalfspaceTriple<double>* hs, std::size_t n, double cx, double cy) {
  return maximize_cramer_with_values<double>(
             std::span<const HalfspaceTriple<double>>(hs, n), cx, cy)
      .predicate()
      .point.y;
}

/**
 * @brief Feasibility flag from a runtime polytope.
 *
 * The runtime entry point reports feasibility through a flag rather
 * than the NTTP path's `static_assert`; Python callers branch on it.
 */
extern "C" __attribute__((noinline)) bool witness_lp_runtime_feasible(
    const HalfspaceTriple<double>* hs, std::size_t n, double cx, double cy) {
  return maximize_cramer_with_values<double>(
             std::span<const HalfspaceTriple<double>>(hs, n), cx, cy)
      .predicate()
      .feasible;
}
