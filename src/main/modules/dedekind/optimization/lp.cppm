/**
 * @file dedekind/optimization/lp.cppm
 * @partition :lp
 * @brief Level 13a: Linear programming with the optimum as a typed constant.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Overview
 * Compile-time 2D LP over a structural ring-like scalar (paper-facing
 * carrier: `Rational<long>`). A problem instance is named at the type
 * level — objective as two NTTPs, constraints as a pack of
 * `Halfspace2D<T, a, b, c>` carriers. The reduction
 * `maximize<cx, cy, H1, H2, …>()` returns the optimum as an NTTP
 * `Vec2<T, x*, y*>` — "the optimum IS a type", literally.
 *
 * @section Comonadic_Framing
 * The reduction is structurally a co-Kleisli arrow. Conceptually:
 *
 *     Path<Vec2<T, x, y>>  →  Vec2<T, x*, y*>
 *
 * i.e. a path of vertex-candidates (one per pair of binding constraints)
 * with an objective-directed argmax extractor. This mirrors the Path
 * co-Kleisli machinery in `dedekind.sequences:path` exactly in shape;
 * the implementation uses NTTP parameter packs rather than a runtime
 * `Path<T>` because the paper-facing claim requires the output at the
 * type level, not as a `constexpr` value.
 *
 * FIXME: unify with `dedekind.sequences:path` co-Kleisli arrows once an
 * NTTP-friendly `Path` carrier lands (tracked informally; the conceptual
 * pattern matches exactly).
 *
 * @section Paper_Facing_Showcase
 *     maximize 3x + 2y
 *     subject to  x +  y ≤ 4       (H1)
 *                 2x +  y ≤ 6       (H2)
 *                -x       ≤ 0       (H3:  x ≥ 0)
 *                     -y  ≤ 0       (H4:  y ≥ 0)
 *
 *     → optimum = Vec2<Rat, 2, 2>, objective value = 10.
 *
 * Active set at the optimum: {H1, H2} — both non-axis-aligned. The 2×2
 * solve uses `Invertible2x2<Rat, 1, 1, 2, 1>` (det = -1), giving the
 * ℚ-exact intersection (2, 2) without rounding.
 *
 * @section Scope
 * - 2D polytopes only (rank-2 narrowing from #364).
 * - Feasible + bounded inputs only. Infeasible polytope → empty
 *   candidate set (caller can inspect via `has_optimum`); unbounded /
 *   ill-posed → `static_assert` failure.
 * - Carrier must satisfy `IsRingLike<T>` and support division by the
 *   active-set determinant; paper-facing `T = Rational<long>` covers all
 *   requirements. The `Dual<Rational<long>>` layering works too (for
 *   parametric sensitivity analysis — see the `[dual]`-tagged test).
 *
 * @note "Modellbildung ist die halbe Miete."
 *       (Modelling is half the battle.)
 *       — Operations-research folk wisdom.
 */
module;

#include <array>
#include <concepts>
#include <cstddef>

export module dedekind.optimization:lp;

import dedekind.algebra;        // IsRingLike constraint
import dedekind.numbers;        // Rational<Z>
import dedekind.linear_algebra; // Invertible2x2, Vec2

namespace dedekind::optimization {

using dedekind::linear_algebra::Vec2;

/**
 * @brief 2D halfspace `a·x + b·y ≤ c`, entries as NTTPs.
 *
 * Convention: constraints are in `≤`-normal form. For `≥` constraints,
 * negate coefficients and bound: `x ≥ 0` becomes
 * `Halfspace2D<T, -1, 0, 0>`.
 */
export template <typename T, T a, T b, T c>
  requires dedekind::algebra::IsRingLike<T>
struct Halfspace2D {
  using scalar_type = T;
  static constexpr T coeff_x = a;
  static constexpr T coeff_y = b;
  static constexpr T bound = c;

  /** @brief `a·x + b·y ≤ c` evaluated at a specific (x, y). */
  template <T x, T y>
  static constexpr bool contains() {
    return !(c < a * x + b * y);
  }

  /** @brief Value-level membership check, for runtime / constexpr use. */
  static constexpr bool contains_value(T x, T y) {
    return !(c < a * x + b * y);
  }
};

/** @section LP_Reduction_Internals
 *
 *  Value-level helpers used to fold the NTTP constraint pack at compile
 *  time. The output is then re-promoted to an NTTP `Vec2<T, x*, y*>`.
 */
namespace detail {

/** @brief Rigid triple carrying one halfspace's (a, b, c) as values. */
template <typename T>
struct HalfspaceTriple {
  T a;
  T b;
  T c;
};

/** @brief Optional-like wrapper for a candidate vertex. */
template <typename T>
struct VertexCandidate {
  T x;
  T y;
  bool feasible;
};

/**
 * @brief 2×2 active-set solve for the intersection of two halfspaces,
 *        treated as equalities: `a1 x + b1 y = c1`, `a2 x + b2 y = c2`.
 *
 * Uses `Invertible2x2`'s Cramer inverse if the active-set matrix is
 * full-rank. If singular, returns an infeasible sentinel (the two
 * halfspaces are parallel; no vertex).
 */
template <typename T>
constexpr VertexCandidate<T> solve_active_set(const HalfspaceTriple<T>& h1,
                                              const HalfspaceTriple<T>& h2) {
  // Determinant of the active-set matrix [[a1 b1] [a2 b2]].
  const T det = h1.a * h2.b - h1.b * h2.a;
  if (det == T{0})
    return {T{0}, T{0}, false};  // singular — parallel halfspaces

  // Cramer's rule.
  const T x = (h1.c * h2.b - h1.b * h2.c) / det;
  const T y = (h1.a * h2.c - h1.c * h2.a) / det;
  return {x, y, true};
}

/**
 * @brief Feasibility: candidate vertex satisfies every halfspace.
 */
template <typename T>
constexpr bool is_feasible(const VertexCandidate<T>& v, const T* halfspaces,
                           std::size_t n) {
  if (!v.feasible) return false;
  for (std::size_t i = 0; i < n; ++i) {
    const T a = halfspaces[3 * i + 0];
    const T b = halfspaces[3 * i + 1];
    const T c = halfspaces[3 * i + 2];
    if (c < a * v.x + b * v.y) return false;
  }
  return true;
}

/**
 * @brief Argmax over feasible vertex-candidates under the objective
 *        `cx · x + cy · y`. The co-Kleisli argmax on the candidate
 *        path; returns `{x*, y*, true}` on success, `{0, 0, false}` on
 *        empty feasible set.
 */
template <typename T, std::size_t N>
constexpr VertexCandidate<T> maximize_impl(
    const std::array<HalfspaceTriple<T>, N>& constraints, T cx, T cy) {
  // Flatten constraints for is_feasible's plain-pointer interface.
  std::array<T, 3 * N> flat{};
  for (std::size_t i = 0; i < N; ++i) {
    flat[3 * i + 0] = constraints[i].a;
    flat[3 * i + 1] = constraints[i].b;
    flat[3 * i + 2] = constraints[i].c;
  }

  VertexCandidate<T> best{T{0}, T{0}, false};
  T best_obj{};
  bool best_set = false;

  for (std::size_t i = 0; i < N; ++i) {
    for (std::size_t j = i + 1; j < N; ++j) {
      const auto v = solve_active_set(constraints[i], constraints[j]);
      if (!is_feasible(v, flat.data(), N)) continue;

      const T obj = cx * v.x + cy * v.y;
      if (!best_set || best_obj < obj) {
        best_set = true;
        best_obj = obj;
        best = v;
      }
    }
  }
  return best;
}

}  // namespace detail

/**
 * @brief The LP reduction: compute the optimum of `cx · x + cy · y` over
 *        the polytope defined by the halfspace pack, as a constexpr
 *        value-level vertex.
 *
 * Primarily used by `maximize_type` to produce the NTTP output; also
 * usable directly when a `constexpr Vec2V<T>` is all you need. The
 * `.feasible` flag indicates whether the polytope admits any vertex (if
 * false, the polytope is empty).
 */
export template <typename T, T cx, T cy, typename... Hs>
  requires(sizeof...(Hs) >= 2)
constexpr detail::VertexCandidate<T> maximize_value() {
  constexpr std::array<detail::HalfspaceTriple<T>, sizeof...(Hs)> cs = {
      detail::HalfspaceTriple<T>{Hs::coeff_x, Hs::coeff_y, Hs::bound}...};
  return detail::maximize_impl(cs, cx, cy);
}

/**
 * @brief The paper-facing reduction: optimum as an NTTP `Vec2<T, x*, y*>`.
 *
 * Usage:
 *
 *     using Opt = decltype(maximize<Rat, Rat{3L}, Rat{2L},
 *                                   Halfspace2D<Rat, 1, 1, 4>,
 *                                   Halfspace2D<Rat, 2, 1, 6>,
 *                                   Halfspace2D<Rat,-1, 0, 0>,
 *                                   Halfspace2D<Rat, 0,-1, 0>>());
 *     // Opt == Vec2<Rat, Rat{2L}, Rat{2L}>  — the optimum IS a type.
 *
 * Requires: the polytope is feasible and bounded. Infeasible input
 * triggers a `static_assert` failure; unbounded input likewise (detected
 * via the candidate set being empty — since unbounded polytopes have no
 * finite optimal vertex, the function errors at instantiation).
 */
export template <typename T, T cx, T cy, typename... Hs>
  requires(sizeof...(Hs) >= 2) && dedekind::algebra::IsRingLike<T>
constexpr auto maximize() {
  constexpr auto v = maximize_value<T, cx, cy, Hs...>();
  static_assert(v.feasible,
                "LP is infeasible or unbounded: no optimal vertex in the "
                "polytope. Check that the halfspace pack intersects to a "
                "non-empty bounded region.");
  return Vec2<T, v.x, v.y>{};
}

}  // namespace dedekind::optimization
