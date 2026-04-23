/**
 * @file dedekind/optimization/lp.cppm
 * @partition :lp
 * @brief Level 13a: Linear programming with the optimum as a typed constant.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Overview
 * Compile-time LP in 2D over `Rational<long>`. A problem instance is
 * named by NTTPs: an objective vector and a list of halfspace constraints.
 * The reduction `maximize(objective, polytope)` enumerates vertex
 * candidates (pairs of binding constraints, solved via `Invertible2x2`),
 * filters the feasible ones, picks the objective-maximising vertex, and
 * wraps the result in a `Singleton` type carrying the optimum as NTTPs.
 *
 * Paper-facing existential proof (two non-axis-aligned constraints
 * active at the optimum):
 *
 *     maximize 3x + 2y
 *     subject to  x +  y ≤ 4
 *                 2x +  y ≤ 6
 *                 x, y ≥ 0
 *
 *     → optimum = Vec2V<Rat>{2, 2}, objective value = 10.
 *
 * The active set is `{x + y = 4, 2x + y = 6}`, solved by
 * `Invertible2x2<Rat, 2, 1, 1, 1>` (det = 1), giving the ℚ-exact
 * intersection without rounding.
 *
 * @section Scope
 * 2D only at this first cut — the paper-facing narrowing from #364.
 * Constraint carriers follow the `:lp` partition's own 2D halfspace DSL,
 * not (yet) the `dedekind.order:halfspace` DSL from paper-1; the two
 * should converge in a follow-up. Full-Schur (#366 open) lets this
 * generalise beyond degenerate active sets; degenerate-only is fine for
 * the minimalistic Gurobi-style showcase.
 *
 * @note "A problem is half solved if properly stated."
 *       — John Dewey, *How We Think* (1910).
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.optimization:lp;

import dedekind.numbers;        // Rational<Z>
import dedekind.linear_algebra; // Invertible2x2, Vec2V, Matrix2x2V

namespace dedekind::optimization {

using dedekind::linear_algebra::Invertible2x2;
using dedekind::linear_algebra::Vec2V;

/**
 * @brief 2D halfspace `a·x + b·y ≤ c`, entries as NTTPs.
 *
 * The carrier is `T` (typically `Rational<long>` for paper-facing ℚ
 * arithmetic). A polytope is a conjunction of halfspaces; each `LP2D`
 * problem is parameterised by an objective `Vec2V`-shape and a pack of
 * `Halfspace2D` constraints.
 *
 * Convention: constraints are `≤`. For `≥` constraints, negate the
 * coefficients and the bound: `x ≥ 0` becomes `Halfspace2D<T, -1, 0, 0>`.
 */
export template <typename T, T a, T b, T c>
struct Halfspace2D {
  using scalar_type = T;
  static constexpr T coeff_x = a;
  static constexpr T coeff_y = b;
  static constexpr T bound = c;

  /**
   * @brief Test membership at a concrete point.
   *
   * Returns true iff `a·x + b·y ≤ c`. Used by the vertex-feasibility
   * filter during enumeration.
   */
  template <T x, T y>
  static constexpr bool contains() {
    return !(c < a * x + b * y);
  }
};

/**
 * @brief Placeholder for a not-yet-implemented `maximize`.
 *
 * Phase 1 will introduce `maximize(ObjectiveX, ObjectiveY, Constraints...)`
 * with the active-set enumeration, 2×2 solve via `Invertible2x2`, and
 * feasibility filter. For now this file carries the carrier types + the
 * problem-class framing; the reduction follows in the next commit on
 * this branch.
 */
// FIXME(paper-lp-pivot): implement `maximize<ObjectiveX, ObjectiveY,
// Constraints...>` returning the optimum as a typed `Singleton` of
// `Vec2V<T, …>`.

}  // namespace dedekind::optimization
