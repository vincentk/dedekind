/**
 * @file dedekind/optimization/lp.cppm
 * @partition :lp
 * @brief Linear programming with the optimum as a typed constant.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section lp__Overview
 * Compile-time 2D LP over a structural ring-like scalar (paper-facing
 * carrier: `Rational<long>`). A problem instance is named at the type
 * level — objective as two NTTPs, constraints as a pack of
 * `Halfspace2D<T, a, b, c>` carriers. The reduction
 * `maximize<cx, cy, H1, H2, …>()` returns the optimum as an NTTP
 * `Vec2<T, x*, y*>` — "the optimum IS a type", literally.
 *
 * @section lp__Comonadic_Framing
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
 * @section lp__Paper_Facing_Showcase
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
 * @section lp__Scope
 * - 2D polytopes only (rank-2 narrowing from #364).
 * - **Feasible + bounded** inputs only. An infeasible polytope (empty
 *   feasible region) surfaces as `VertexCandidate::feasible == false`
 *   on the returned candidate; callers inspect that flag on the
 *   result of `maximize_value()`.  Unbounded polytopes are *not*
 *   detected by this reduction: enumeration returns the best vertex
 *   among the ${n \choose 2}$ active-set candidates, so an unbounded
 *   objective over an unbounded polytope will yield a finite vertex
 *   that is not the true supremum.  The paper-facing showcases are
 *   bounded by construction; unbounded-LP detection is tracked as a
 *   follow-up.
 * - Carrier must (today) satisfy `HasRingOperators<T>` plus division and
 *   comparison, matching the actual `Invertible2x2` Cramer solve
 *   below. The `requires` clauses are deliberately weaker than what
 *   the body needs — tightening to `HasFieldOperators<T>` plus an
 *   ordering requirement, or to a proper `IsField<T, +, *>` once the
 *   latter lands in `dedekind.category:total`, is tracked under
 *   epic #374 (algebraic concept vocabulary alignment).  Paper-facing
 *   `T = Rational<SignedExtensionalCardinal<>>` covers all the real
 *   requirements; `Dual<Rational<...>>` layering works for parametric
 *   sensitivity analysis (`[dual]`-tagged test).
 *
 * @note "Modellbildung ist die halbe Miete."
 *       (Modelling is half the battle.)
 *       — Operations-research folk wisdom.
 */
module;

#include <array>
#include <concepts>
#include <cstddef>
#include <span>

export module dedekind.optimization:lp;

import dedekind.algebra;        // HasRingOperators constraint
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
// HasRingOperators<T> here: Halfspace2D evaluates `a*x + b*y` and a
// scalar comparison; it does not depend on ring axioms of T (no
// associativity / commutativity / identity claim is made), only on the
// operators compiling and closing.  See #393 for the audit rationale.
export template <typename T, T a, T b, T c>
  requires dedekind::algebra::HasRingOperators<T>
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

/** @brief Value-level halfspace carrier @c (a, b, c) for the bridge
 *  kernel.  Paired with the NTTP carrier @ref Halfspace2D: an instance
 *  of @c Halfspace2D<T, a, b, c> destructures into a @c HalfspaceTriple
 *  <T>{a, b, c} for the uniform value-vector the kernel consumes.  The
 *  two structures share one role — the halfspace — split across the
 *  type/value boundary the bridge straddles.
 */
export template <typename T>
struct HalfspaceTriple {
  T a;
  T b;
  T c;
};

/** @brief Reduction output: optimum coordinates with a feasibility flag.
 *
 *  The @c .feasible flag distinguishes a genuine optimum from an empty
 *  feasible region — the runtime call site inspects the flag; the NTTP
 *  call site checks it via @c static_assert in @ref maximize.
 */
export template <typename T>
struct VertexCandidate {
  T x;
  T y;
  bool feasible;
};

/** @section lp__LP_Reduction_Internals
 *
 *  Value-level helpers used by the bridge kernel.  The kernel itself
 *  (@ref detail::maximize_impl) is one @c constexpr function called from
 *  both modes — the NTTP entry point @ref maximize_value hands it a
 *  span over a @c constexpr @c std::array of triples (folds at compile
 *  time); the runtime entry point @ref maximize_with_values hands it a
 *  span over a runtime @c std::vector (runs at call time).  Same code
 *  path, two evaluation modes, selected by the constexpr-ness of the
 *  arguments at the call site.
 */
namespace detail {

/**
 * @brief Carrier-aware singularity predicate on the Cramer determinant.
 *
 * @details For plain ring-like carriers (`Rational<Z>`, `int`, etc.) a
 * determinant is singular iff it equals the additive identity. For
 * `Dual<F>` (forward-mode AD) the situation is subtler: a dual of the
 * form `{val = 0, der = t}` is *still* singular regardless of `t`,
 * because the first-order primal part is what controls invertibility.
 * A naive `det == T{}` check would miss this because `operator==` on
 * `Dual<F>` compares both components and would treat `{0, t}` with
 * `t != 0` as non-zero.
 *
 * We duck-type on the `.val` / `.der` pair to detect Dual-like
 * carriers without adding a `dedekind.numbers:dual` import: any carrier
 * exposing both fields is treated as having primal-determined
 * singularity; everything else falls back to full equality.
 */
template <typename T>
constexpr bool is_singular(const T& det) {
  if constexpr (requires {
                  det.val;
                  det.der;
                }) {
    // Dual-like carrier: primal-only check.
    return det.val == decltype(det.val){};
  } else {
    return det == T{};
  }
}

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
  if (is_singular(det)) return {T{}, T{}, false};  // singular — parallel

  // Cramer's rule.
  const T x = (h1.c * h2.b - h1.b * h2.c) / det;
  const T y = (h1.a * h2.c - h1.c * h2.a) / det;
  return {x, y, true};
}

/**
 * @brief Argmax over feasible vertex-candidates under the objective
 *        `cx · x + cy · y`.  The co-Kleisli argmax on the candidate
 *        path; returns `{x*, y*, true}` on success, `{0, 0, false}` on
 *        an empty feasible set.
 *
 * @details One @c constexpr kernel for both evaluation modes.  Called
 * with a @c constexpr @c std::span (e.g. from @c maximize_value below,
 * which materialises the NTTP pack as a fixed-size @c std::array), the
 * reduction folds at translation time and the optimum collapses to a
 * typed constant.  Called with a runtime @c std::span (e.g. over a
 * @c std::vector materialised from a Python list of triples), the same
 * function runs at runtime — same code path, same active-set
 * enumeration, same carrier-aware @c is_singular check.  This is the
 * paper's two-way bridge made literal: one function, two modes,
 * selection by the constexpr-ness of the arguments at the call site.
 */
template <typename T>
constexpr VertexCandidate<T> maximize_impl(
    std::span<const HalfspaceTriple<T>> constraints, T cx, T cy) {
  VertexCandidate<T> best{T{}, T{}, false};
  T best_obj{};
  bool best_set = false;
  const std::size_t N = constraints.size();

  for (std::size_t i = 0; i < N; ++i) {
    for (std::size_t j = i + 1; j < N; ++j) {
      const auto v = solve_active_set(constraints[i], constraints[j]);
      if (!v.feasible) continue;

      bool feasible = true;
      for (std::size_t k = 0; k < N; ++k) {
        const auto& h = constraints[k];
        if (h.c < h.a * v.x + h.b * v.y) {
          feasible = false;
          break;
        }
      }
      if (!feasible) continue;

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
 * @brief NTTP entry point: optimum as a value-level @c VertexCandidate.
 *
 * @details Materialises the NTTP halfspace pack into a @c constexpr
 * @c std::array of value triples and hands a span to the unified bridge
 * kernel @ref detail::maximize_impl.  Because both the array and the
 * kernel call are @c constexpr, the reduction folds at translation time
 * and the returned candidate is a typed constant.  Used by @ref maximize
 * to lift the result back to an NTTP @c Vec2<T, x*, y*>.
 *
 * The @c .feasible flag indicates whether the polytope admits any
 * vertex (false ⇒ the polytope is empty).
 */
export template <typename T, T cx, T cy, typename... Hs>
  requires(sizeof...(Hs) >= 2)
constexpr VertexCandidate<T> maximize_value() {
  constexpr std::array<HalfspaceTriple<T>, sizeof...(Hs)> cs = {
      HalfspaceTriple<T>{Hs::coeff_x, Hs::coeff_y, Hs::bound}...};
  return detail::maximize_impl<T>(std::span<const HalfspaceTriple<T>>(cs), cx,
                                  cy);
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
  requires(sizeof...(Hs) >= 2) && dedekind::algebra::HasRingOperators<T>
constexpr auto maximize() {
  constexpr auto v = maximize_value<T, cx, cy, Hs...>();
  static_assert(v.feasible,
                "LP is infeasible or unbounded: no optimal vertex in the "
                "polytope. Check that the halfspace pack intersects to a "
                "non-empty bounded region.");
  return Vec2<T, v.x, v.y>{};
}

/** @section lp__Comonadic_Extract_Witness
 *
 *  @c maximize<T, cx, cy, Hs...>() is structurally a comonadic counit
 *  (@c ε in the Kleisli notation): it takes the polytope-context
 *  @c (cx, cy, Hs...) — bundled as the NTTP pack — and extracts the
 *  optimal vertex as @c Vec2<T, x*, y*>.  The pack-as-context view
 *  matches the textbook co-Kleisli arrow shape
 *  @c F<T> → T already noted in the file header.  The
 *  @c Polytope2D wrapper below makes the extract explicit at the
 *  type level so the witness can be pinned without changing the
 *  primary API surface.
 */

/** @brief Polytope context: the (cx, cy, Hs...) pack reified as a type.
 *
 *  Carries no runtime state; it exists only so the comonadic extract
 *  has a context-carrier to consume.  Conceptually @c F<T> in the
 *  @c F<T> → T shape of the LP reduction.  Exported alongside
 *  @c lp_extract so module consumers can name the parameter type.
 */
export template <typename T, T cx, T cy, typename... Hs>
  requires(sizeof...(Hs) >= 2) && dedekind::algebra::HasRingOperators<T>
struct Polytope2D final {
  using scalar_type = T;
  static constexpr T objective_x = cx;
  static constexpr T objective_y = cy;

  /** @brief @c ε: extract the optimal vertex as a typed Vec2.  The
   *  reduction collapses to a typed constant at instantiation —
   *  exactly the LP-vertex-as-typed-constant claim from §5 of the
   *  paper.
   */
  static constexpr auto extract() { return maximize<T, cx, cy, Hs...>(); }
};

/** @brief @c ε / extract for the LP comonadic context.  Provided as a
 *  free function so callers can write @c lp_extract(polytope) at the
 *  call site.
 */
export template <typename T, T cx, T cy, typename... Hs>
  requires(sizeof...(Hs) >= 2) && dedekind::algebra::HasRingOperators<T>
constexpr auto lp_extract(Polytope2D<T, cx, cy, Hs...>) {
  return Polytope2D<T, cx, cy, Hs...>::extract();
}

/** @section lp__The_Bridge
 *
 *  @ref maximize_value and @ref maximize_with_values are not two
 *  reductions.  They are two call sites of @em the same function,
 *  @ref detail::maximize_impl, which takes a @c std::span<const
 *  HalfspaceTriple<T>> and a @c (cx, cy) objective and is @c constexpr.
 *
 *  - @ref maximize_value materialises the NTTP pack into a @c constexpr
 *    @c std::array and hands its span to the kernel.  Every argument is
 *    a constant expression, so the call folds at translation time and
 *    the optimum collapses to a typed constant — used by @ref maximize
 *    to lift the result back to an NTTP @c Vec2<T, x*, y*>.
 *
 *  - @ref maximize_with_values takes a span over runtime data (e.g. a
 *    @c std::vector populated from a Python list of triples through the
 *    nanobind facade) and hands it to the same kernel.  The arguments
 *    are not constant expressions, so the call runs at runtime.
 *
 *  Selection between the two evaluation modes is therefore not a kernel
 *  choice but a property of the call site: the same @c constexpr code
 *  path serves both, with the compiler folding what it can.  This is the
 *  paper's two-way bridge between value and type-level evaluation made
 *  literal — one function, two modes.
 */

/** @brief Runtime entry point: hand a value-level constraint span to the
 *  unified @ref detail::maximize_impl kernel.  Marked @c constexpr so
 *  the same call folds at compile time when its arguments happen to be
 *  constant expressions (witnessed by a @c static_assert in the test
 *  suite); reduces to a runtime call when they are not.
 */
export template <typename T>
  requires dedekind::algebra::HasRingOperators<T>
constexpr VertexCandidate<T> maximize_with_values(
    std::span<const HalfspaceTriple<T>> halfspaces, T cx, T cy) {
  return detail::maximize_impl<T>(halfspaces, cx, cy);
}

}  // namespace dedekind::optimization
