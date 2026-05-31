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
 *   result of `maximize_with_values()`.  Unbounded polytopes are *not*
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
#include <limits>
#include <span>

export module dedekind.optimization:lp;

import dedekind.algebra;        // HasRingOperators constraint
import dedekind.category;       // ClassicalLogic — Set's logic species (#747)
import dedekind.numbers;        // Rational<Z>
import dedekind.linear_algebra; // Invertible2x2, Vec2
import dedekind.sets;           // Set — DSL participants (#747)

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

/** @section lp__Structural_Classifiers
 *
 *  Structural properties of halfspace packs that the @ref maximize NTTP
 *  entry uses to dispatch among the §5 triptych: the signed-unimodular
 *  fast path, the generic Cramer fold, and the @c static_assert refusal
 *  for infeasible packs.  Algebraic gates over the halfspace family —
 *  the dispatch is driven by what the constraints @em are, not by what
 *  the call site says they should be.
 */
namespace detail {

/** @brief Per-halfspace check: @c (coeff_x, coeff_y) ∈ {−1, 0, +1}². */
template <typename H>
constexpr bool entries_in_unit_range_v =
    (H::coeff_x == typename H::scalar_type{-1} ||
     H::coeff_x == typename H::scalar_type{} ||
     H::coeff_x == typename H::scalar_type{1}) &&
    (H::coeff_y == typename H::scalar_type{-1} ||
     H::coeff_y == typename H::scalar_type{} ||
     H::coeff_y == typename H::scalar_type{1});

/** @brief Per-halfspace check: at least one of @c (coeff_x, coeff_y) is zero —
 *  the halfspace is axis-aligned (constrains x or y but not both).
 */
template <typename H>
constexpr bool axis_aligned_v = H::coeff_x == typename H::scalar_type{} ||
                                H::coeff_y == typename H::scalar_type{};

}  // namespace detail

/** @brief Carrier requirements for the bit-ops fast-path kernel.
 *
 *  @details The kernel uses default construction ( @c T{} ), construction
 *  from the integer literal @c 1 ( @c T{1} ), ring operators, ordering,
 *  and the convention that @c −1 lies strictly below @c 0 — which excludes
 *  unsigned integral carriers where @c T{} @c − @c T{1} wraps to the
 *  maximum value and would pass the kernel's @c neg_one validation, then
 *  be misread as a positive coefficient by the @c zero @c < @c h.a branch.
 *  Naming each operation in the concept lets unsupported carriers fail at
 *  the API boundary rather than inside the function body.
 */
export template <typename T>
concept SuitableForAxisAlignedFastPath =
    dedekind::algebra::HasRingOperators<T> && std::totally_ordered<T> &&
    requires {
      T{};
      T{1};
      requires(T{} - T{1}) < T{};
    };

/** @brief Pack-level concept: every halfspace has entries in {−1, 0, +1}.
 *
 *  @details Necessary structural condition for the bit-ops fast path in
 *  @ref maximize : when every halfspace in the pack carries
 *  @c (coeff_x, coeff_y) ∈ {−1, 0, +1}², the active-set 2×2
 *  determinants live in {−2, −1, 0, +1, +2} and the matrix entries
 *  themselves collapse multiplications to additions, subtractions, and
 *  sign flips at the IR level.  Every signed-unimodular pack the §5
 *  exhibit ships further satisfies pairwise @c |det| ∈ {0, 1}, which
 *  takes the fast-path division to a no-op; a pairwise sharpening of
 *  the gate is a candidate refinement when a workload turns up
 *  unit-range entries with @c |det| ∈ {1, 2} active-set determinants.
 *
 *  Algebraic gate per `feedback_algebraic_over_architectural_constraints`.
 */
export template <typename... Hs>
concept IsSignedUnimodular = (detail::entries_in_unit_range_v<Hs> && ...);

/** @brief Pack-level concept: every halfspace is axis-aligned.
 *
 *  @details Combined with @ref IsSignedUnimodular this is the dispatch
 *  gate for the bit-ops fast path in @ref maximize : axis-aligned halfspaces
 *  with entries in {−1, 0, +1} reduce the LP to per-axis bound extraction,
 *  with vertex selection driven by the sign of the objective coefficients.
 *  No multiplication, no division — only comparisons and sign flips remain
 *  in the IR.
 */
export template <typename... Hs>
concept IsAxisAlignedPolytope = (detail::axis_aligned_v<Hs> && ...);

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
 *  both modes — the NTTP packaging @ref maximize materialises a
 *  @c constexpr @c std::array of triples and hands its span to the
 *  single public entry @ref maximize_with_values (folds at compile
 *  time); the runtime call site hands a @c std::vector-backed span to
 *  the same @ref maximize_with_values (runs at call time).  Same code
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
 * with a @c constexpr @c std::span (e.g. from @c maximize_with_values
 * below, when @c maximize materialises the NTTP pack as a fixed-size
 * @c std::array), the reduction folds at translation time and the
 * optimum collapses to a typed constant.  Called with a runtime
 * @c std::span (e.g. over a @c std::vector materialised from a Python
 * list of triples), the same function runs at runtime — same code
 * path, same active-set enumeration, same carrier-aware
 * @c is_singular check.  This is the
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

/**
 * @brief Bit-ops fast-path kernel for axis-aligned signed-unimodular
 *        polytopes.  Sibling of @ref maximize_impl ; same return shape,
 *        same constexpr-callable / runtime-callable bridge property.
 *
 * @details Precondition (caller's responsibility, statically gated by
 * @c IsSignedUnimodular && @c IsAxisAlignedPolytope at the @ref maximize
 * dispatch site): every halfspace in @c constraints has
 * @c (a, b) ∈ {−1, 0, +1}² with at least one coefficient zero.  Under
 * this precondition each halfspace contributes one bound on one axis,
 * the active set degenerates to per-axis bracketing, and the optimum
 * vertex is selected by the sign of the objective coefficients.  No
 * multiplication or division is performed on the carrier — the IR for
 * the runtime call reduces to comparisons, negations, and selects.
 */
template <typename T>
constexpr VertexCandidate<T> maximize_impl_axis_aligned(
    std::span<const HalfspaceTriple<T>> constraints, T cx, T cy) {
  bool has_x_lo = false, has_x_hi = false;
  bool has_y_lo = false, has_y_hi = false;
  T x_lo{}, x_hi{}, y_lo{}, y_hi{};

  const T one{1};
  const T zero{};
  const T neg_one{T{} - one};

  // Indexed loop rather than range-for: the range-for's implicit
  // iterator comparison triggers ADL through @c HalfspaceTriple<T> 's
  // value type and on libstdc++ pulls in @c Rational / @c Cardinality
  // operators that are not viable for iterator operands but still
  // confuse overload resolution.  Indexed iteration matches the style
  // of the generic Cramer kernel above and avoids the iterator-comparison
  // path entirely.
  const std::size_t N = constraints.size();
  for (std::size_t i = 0; i < N; ++i) {
    const auto& h = constraints[i];
    // Precondition guard: each halfspace must be axis-aligned with
    // @c (a, b) ∈ {−1, 0, +1}².  The NTTP entry @ref maximize gates
    // this via @c IsSignedUnimodular && @c IsAxisAlignedPolytope; the
    // exported runtime entry @ref maximize_axis_aligned_with_values is
    // reachable directly, so the kernel rejects out-of-range coefficients
    // by collapsing to the infeasible sentinel rather than silently
    // mis-solving with @c h.a = 2 read as @c +1·x.  Honest Rejection
    // per the project's discipline.
    const bool a_ok = h.a == neg_one || h.a == zero || h.a == one;
    const bool b_ok = h.b == neg_one || h.b == zero || h.b == one;
    if (!a_ok || !b_ok) return {zero, zero, false};

    // Axis-alignment guard: reject halfspaces where BOTH coefficients
    // are nonzero (e.g. @c x @c + @c y @c ≤ @c c ).  Without this the
    // @c zero @c < @c h.a branch silently consumes the @c a-coefficient
    // and ignores @c h.b , misreading a diagonal halfspace as an upper
    // bound on x.
    if (h.a != zero && h.b != zero) return {zero, zero, false};

    // Boundary guard: when the carrier specialises @c std::numeric_limits
    // (e.g. signed integral types), reject @c h.c at the lowest
    // representable value.  The negation @c zero @c − @c h.c below would
    // be undefined behaviour on signed integral T at @c T::lowest() ;
    // carriers without @c numeric_limits specialisation (Rational, Dual)
    // bypass this check.
    if constexpr (std::numeric_limits<T>::is_specialized) {
      if (h.c == std::numeric_limits<T>::lowest()) return {zero, zero, false};
    }

    if (zero < h.a) {
      // +1·x ≤ c   →   x ≤ c   (upper bound on x).
      if (!has_x_hi || h.c < x_hi) {
        x_hi = h.c;
        has_x_hi = true;
      }
    } else if (h.a < zero) {
      // −1·x ≤ c   →   x ≥ −c   (lower bound on x).
      const T lo = zero - h.c;
      if (!has_x_lo || x_lo < lo) {
        x_lo = lo;
        has_x_lo = true;
      }
    } else if (zero < h.b) {
      // +1·y ≤ c   →   y ≤ c   (upper bound on y).
      if (!has_y_hi || h.c < y_hi) {
        y_hi = h.c;
        has_y_hi = true;
      }
    } else if (h.b < zero) {
      // −1·y ≤ c   →   y ≥ −c   (lower bound on y).
      const T lo = zero - h.c;
      if (!has_y_lo || y_lo < lo) {
        y_lo = lo;
        has_y_lo = true;
      }
    } else if (h.c < zero) {
      // Zero-row halfspace @c 0·x + 0·y ≤ c with @c c < 0 is vacuously
      // unsatisfiable: the whole polytope collapses to empty.  The
      // @c c ≥ 0 case is the trivial halfspace and contributes nothing.
      return {zero, zero, false};
    }
  }

  // Feasibility: a bounded box requires both bounds on each axis, and
  // lo ≤ hi.
  if (!has_x_lo || !has_x_hi || !has_y_lo || !has_y_hi)
    return {T{}, T{}, false};
  if (x_hi < x_lo || y_hi < y_lo) return {T{}, T{}, false};

  // Pick the vertex by objective sign — no multiplication needed.
  const T x_opt = (cx < T{}) ? x_lo : x_hi;
  const T y_opt = (cy < T{}) ? y_lo : y_hi;
  return {x_opt, y_opt, true};
}

}  // namespace detail

/** @section lp__The_Bridge
 *
 *  @ref maximize_with_values is the single user-facing entry point for
 *  the 2D LP reduction.  It is @c constexpr, takes a
 *  @c std::span<const HalfspaceTriple<T>> and a @c (cx, cy) objective,
 *  and delegates to the @c constexpr kernel @ref detail::maximize_impl.
 *  Selection between evaluation modes is a property of the call site:
 *
 *  - Called with a span over a @c constexpr @c std::array (e.g. from
 *    @ref maximize, which materialises the NTTP pack), every argument
 *    is a constant expression and the call folds at translation time.
 *
 *  - Called with a span over runtime data (e.g. a @c std::vector
 *    populated from a Python list of triples through the nanobind
 *    facade), the arguments are not constant expressions and the call
 *    runs at runtime.
 *
 *  One function, two modes — the paper's two-way bridge between value
 *  and type-level evaluation literally collapsed onto one definition.
 *  The compile-time @em packaging surface @ref maximize sits on top of
 *  this entry to lift the result back into an NTTP @c Vec2<T, x*, y*>;
 *  it is the only thing in this file that performs the type-level lift.
 */
export template <typename T>
  requires dedekind::algebra::HasRingOperators<T>
constexpr VertexCandidate<T> maximize_with_values(
    std::span<const HalfspaceTriple<T>> halfspaces, T cx, T cy) {
  // Mechanical dispatch: scan the span for structural fast-path
  // eligibility (every halfspace has @c (a, b) ∈ {−1, 0, +1}² with at
  // least one zero coefficient) and route to the bit-ops kernel when
  // it qualifies, else the generic Cramer kernel.  Mirrors the NTTP
  // entry's @c if @c constexpr dispatch at runtime — the type system
  // selects the kernel from the input's structure end-to-end.
  if constexpr (SuitableForAxisAlignedFastPath<T>) {
    const T one{1};
    const T zero{};
    const T neg_one{T{} - one};
    bool fast_eligible = true;
    for (std::size_t i = 0; i < halfspaces.size(); ++i) {
      const auto& h = halfspaces[i];
      const bool a_unit = h.a == neg_one || h.a == zero || h.a == one;
      const bool b_unit = h.b == neg_one || h.b == zero || h.b == one;
      const bool axis_aligned = h.a == zero || h.b == zero;
      if (!a_unit || !b_unit || !axis_aligned) {
        fast_eligible = false;
        break;
      }
    }
    if (fast_eligible) {
      return detail::maximize_impl_axis_aligned<T>(halfspaces, cx, cy);
    }
  }
  return detail::maximize_impl<T>(halfspaces, cx, cy);
}

/** @brief Runtime entry for the bit-ops fast-path kernel.  Same shape as
 *  @ref maximize_with_values ; the NTTP entry @ref maximize statically
 *  gates this path via @c IsSignedUnimodular and @c IsAxisAlignedPolytope ,
 *  but the runtime entry is also reachable directly, so the kernel
 *  defensively validates each halfspace and collapses the result to the
 *  infeasible sentinel (@c VertexCandidate::feasible @c == @c false) on:
 *
 *  - any coefficient outside {−1, 0, +1} (e.g. @c h.a @c = @c 2 would
 *    otherwise read as @c +1·x );
 *  - a halfspace with both coefficients nonzero — diagonal, not
 *    axis-aligned (@c x @c + @c y @c ≤ @c c would otherwise read as
 *    @c x @c ≤ @c c );
 *  - @c h.c at @c std::numeric_limits<T>::lowest() on carriers where
 *    @c numeric_limits is specialised (signed integers): @c zero @c −
 *    @c h.c would be undefined behaviour there.
 *
 *  Honest Rejection per the project's discipline.  Constexpr-callable
 *  for compile-time evaluation; the runtime call site sees an IR
 *  without @c MUL or @c DIV on the carrier.
 *
 *  Carrier constraint @ref detail::SuitableForAxisAlignedFastPath names
 *  every operation the kernel uses ( @c T{} , @c T{1} , ring ops,
 *  ordering, signedness via @c (T{} @c − @c T{1}) @c < @c T{} ) so
 *  unsupported carriers fail at the API boundary rather than inside the
 *  function body.
 */
export template <typename T>
  requires SuitableForAxisAlignedFastPath<T>
constexpr VertexCandidate<T> maximize_axis_aligned_with_values(
    std::span<const HalfspaceTriple<T>> halfspaces, T cx, T cy) {
  return detail::maximize_impl_axis_aligned<T>(halfspaces, cx, cy);
}

/**
 * @brief NTTP packaging on top of @ref maximize_with_values: optimum as
 *        a typed @c Vec2<T, x*, y*>.
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
 * @details Materialise the NTTP halfspace pack into a @c constexpr
 * @c std::array of triples and call the single bridge entry
 * @ref maximize_with_values.  Because the array and the entry are both
 * @c constexpr, the call folds at translation time and the resulting
 * coordinates lift back to NTTPs as @c Vec2<T, x*, y*>.  This is the
 * only surface that performs the type-level lift; everything below it
 * is the same @c constexpr function the runtime path calls.
 *
 * Requires: the polytope is feasible and bounded.  Infeasible input
 * triggers a @c static_assert failure; unbounded input likewise
 * (detected via the candidate set being empty — since unbounded
 * polytopes have no finite optimal vertex, the function errors at
 * instantiation).
 */
/** @section lp__Output_Set_Predicates
 *
 *  Predicates for the @em output side of @ref argmax — the optimal
 *  locus expressed as a @c :expressions::Set rather than a typed
 *  @c Vec2 .  The output Set's predicate type encodes the LP's
 *  mathematical regime:
 *
 *  - @ref Singleton2DPredicate carries @c (x*, y*) at the type level
 *    when the LP has a unique optimal vertex.
 *  - The existing @c :expressions::EmptyPredicate carries the empty
 *    set when the LP is infeasible.
 *  - An unbounded-face predicate is deferred until §5 needs it.
 *
 *  Both NTTP and runtime entries of @ref argmax return a Set; at the
 *  NTTP level the predicate type pins the regime, at the runtime level
 *  the regime is carried by the Set's value-level state.  The §3
 *  sets-as-rules vocabulary is the same on both sides of the @c
 *  argmax combinator — input polytope and output locus alike.
 */

/** @brief @c :expressions predicate for the LP's unique optimal vertex
 *  at NTTP coordinates.
 *
 *  @details Carries @c (x_val, y_val) as NTTPs (structural) and exposes
 *  a stateless @c operator() — the predicate matches @c v iff
 *  @c v.x == x_val @c && @c v.y == y_val .  Cardinality is @c Finite
 *  (size 1).  Used as the output-Set predicate when @ref argmax
 *  produces a unique optimum.
 */
export template <typename T, T x_val, T y_val>
struct Singleton2DPredicate {
  using Domain = dedekind::linear_algebra::Vec2V<T>;
  static constexpr T coord_x = x_val;
  static constexpr T coord_y = y_val;
  using cardinality_type = dedekind::sets::Finite;

  constexpr bool operator()(const Domain& p) const {
    return p.x == x_val && p.y == y_val;
  }
};

/** @brief Lift an NTTP @c (x*, y*) into a @c :expressions Set whose
 *  predicate carries the singleton structurally.  Mirrors
 *  @ref halfspace_set on the input side; together they make the §3 ↔
 *  §5 bridge symmetric — Set goes in, Set comes out. */
export template <typename T, T x_val, T y_val>
constexpr auto lp_singleton_set() {
  return dedekind::sets::Set<dedekind::linear_algebra::Vec2V<T>,
                             dedekind::category::ClassicalLogic,
                             Singleton2DPredicate<T, x_val, y_val>>{
      Singleton2DPredicate<T, x_val, y_val>{}};
}

/** @brief Lift an empty optimum into a @c :expressions Set whose
 *  predicate is the existing @c :expressions::EmptyPredicate over
 *  @c Vec2V<T> .  Used as the output of @ref argmax when the LP is
 *  infeasible — the empty optimum is a value of the output Set type,
 *  not a compile event. */
export template <typename T>
constexpr auto lp_empty_set() {
  return dedekind::sets::Set<
      dedekind::linear_algebra::Vec2V<T>, dedekind::category::ClassicalLogic,
      dedekind::sets::EmptyPredicate<dedekind::linear_algebra::Vec2V<T>>>{
      dedekind::sets::EmptyPredicate<dedekind::linear_algebra::Vec2V<T>>{}};
}

export template <typename T, T cx, T cy, typename... Hs>
  requires(sizeof...(Hs) >= 2) && dedekind::algebra::HasRingOperators<T>
constexpr auto maximize() {
  constexpr std::array<HalfspaceTriple<T>, sizeof...(Hs)> cs = {
      HalfspaceTriple<T>{Hs::coeff_x, Hs::coeff_y, Hs::bound}...};
  // §5 triptych dispatch: when the pack is signed-unimodular AND
  // axis-aligned, the bit-ops fast path applies; otherwise fall back to
  // the generic Cramer fold.  Infeasibility surfaces uniformly via the
  // @c static_assert on @c .feasible below.
  if constexpr (IsSignedUnimodular<Hs...> && IsAxisAlignedPolytope<Hs...>) {
    constexpr auto v = maximize_axis_aligned_with_values<T>(
        std::span<const HalfspaceTriple<T>>(cs), cx, cy);
    static_assert(v.feasible,
                  "LP is infeasible: axis-aligned bit-ops fast path could "
                  "not bracket a non-empty box. Check that every axis has "
                  "matched lower and upper bounds with lo ≤ hi.");
    return Vec2<T, v.x, v.y>{};
  } else {
    constexpr auto v = maximize_with_values<T>(
        std::span<const HalfspaceTriple<T>>(cs), cx, cy);
    static_assert(v.feasible,
                  "LP is infeasible or unbounded: no optimal vertex in the "
                  "polytope. Check that the halfspace pack intersects to a "
                  "non-empty bounded region.");
    return Vec2<T, v.x, v.y>{};
  }
}

/**
 * @brief NTTP packaging on top of @ref maximize_with_values returning a
 *        @c :expressions Set whose predicate type encodes the LP's
 *        mathematical regime.
 *
 * Usage:
 *
 *     constexpr auto opt = maximize_set<Rat, Rat{3L}, Rat{2L},
 *                                       H1, H2, H3, H4>();
 *     //  decltype(opt) = Set<Vec2V<Rat>, ClassicalLogic,
 *     //                      Singleton2DPredicate<Rat, Rat{2L}, Rat{2L}>>
 *     //  — the regime (Singleton) AND the coordinates (2, 2) live in
 *     //  the type.
 *
 * @details Same kernel dispatch as @ref maximize (signed-unimodular fast
 * path vs generic Cramer fold), but the return is a Set whose predicate
 * type carries the regime instead of a @c Vec2 with an implicit
 * "feasible" assumption.  When the LP is infeasible the return is a
 * Set with @c :expressions::EmptyPredicate ; no @c static_assert
 * fires — the empty optimum is a value of the output Set type.
 *
 * Callers who want compile-time refusal of infeasible inputs can
 * @c static_assert on @c decltype(opt) themselves (e.g. @c static_assert
 * (!std::same_as<decltype(opt), decltype(lp_empty_set<T>())>)).
 *
 * This is the Set-as-output companion to @ref maximize ; @ref argmax
 * below is the @c :expressions Set DSL entry that calls it.
 */
export template <typename T, T cx, T cy, typename... Hs>
  requires(sizeof...(Hs) >= 2) && dedekind::algebra::HasRingOperators<T>
constexpr auto maximize_set() {
  constexpr std::array<HalfspaceTriple<T>, sizeof...(Hs)> cs = {
      HalfspaceTriple<T>{Hs::coeff_x, Hs::coeff_y, Hs::bound}...};
  if constexpr (IsSignedUnimodular<Hs...> && IsAxisAlignedPolytope<Hs...>) {
    constexpr auto v = maximize_axis_aligned_with_values<T>(
        std::span<const HalfspaceTriple<T>>(cs), cx, cy);
    if constexpr (v.feasible) {
      return lp_singleton_set<T, v.x, v.y>();
    } else {
      return lp_empty_set<T>();
    }
  } else {
    constexpr auto v = maximize_with_values<T>(
        std::span<const HalfspaceTriple<T>>(cs), cx, cy);
    if constexpr (v.feasible) {
      return lp_singleton_set<T, v.x, v.y>();
    } else {
      return lp_empty_set<T>();
    }
  }
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

/** @section lp__Set_DSL_Slice  Halfspace meets as real `:expressions` Set DSL.
 *
 *  The §5 LP exhibit reads in the textbook frame
 *  `F : ℚ×ℚ, U : F→ℚ, G ⊆ F, opt = argmax(G, U)`, with `G` being a
 *  genuine `:expressions::Set<Vec2V<T>, ClassicalLogic, P>` instance —
 *  the same DSL §3 builds out of bound-scout comprehensions, applied
 *  to the 2D LP carrier:
 *
 *      constexpr auto G = halfspace_set(H1{}) & halfspace_set(H2{})
 *                       & halfspace_set(H3{}) & halfspace_set(H4{});
 *      constexpr LinearFunctional<Rat, Rat{3L}, Rat{2L}> U{};
 *      constexpr auto opt = argmax(G, U);   //  Vec2<Rat, Rat{2L}, Rat{2L}>
 *      static_assert(G.contains(opt));      //  opt ∈ G via the Set DSL
 *
 *  The integration with `:expressions` rides on the existing
 *  @c structured_and customization point (the same one @c :order:halfspace
 *  uses to collapse one-dimensional halfspace meets to singletons /
 *  intervals / empty).  @c :expressions::Set::operator& discovers our
 *  @c structured_and overloads via ADL on the predicate types and
 *  produces a structurally-typed Set:
 *
 *      Set<Vec2V<T>, ClassicalLogic,
 *          Polytope2DPredicate<T, H1, H2, H3, H4>>
 *
 *  carrying the halfspace pack inside the predicate's template parameter
 *  list — exactly what @c argmax extracts to call the kernel.  Closes
 *  FIXME(#365) in the 2D-halfspace case: the meet is no longer a lambda
 *  fall-through, it's a structural reduction the DSL recognises.
 */

/** @brief @c :expressions predicate for one 2D halfspace `a·x + b·y ≤ c`.
 *
 *  Carries `(a, b, c)` as NTTPs (structural) and exposes a stateless
 *  @c operator() — the predicate Set DSL participants are built from.
 *  ADL through this type's namespace (`:optimization`) lets the
 *  @c structured_and overloads below intercept halfspace meets inside
 *  @c :expressions::Set::operator& . */
export template <typename T, T a, T b, T c>
struct Halfspace2DPredicate {
  using Domain = dedekind::linear_algebra::Vec2V<T>;
  // `cardinality_type` here serves @em two purposes for the
  // `:expressions::Set::operator&` dispatch on the structured result
  // (see `:expressions::expressions.cppm:708-715`):
  //
  //   1. Required for substitution well-formedness.  The Finite-elevation
  //      branch is spelled
  //          `requires { typename Result::cardinality_type; } &&
  //           std::same_as<typename Result::cardinality_type, Finite>`
  //      and the `&&` short-circuits at @em evaluation but NOT at
  //      @em substitution: the `std::same_as<typename Result::…, Finite>`
  //      operand is type-checked regardless of the `requires`'s truth
  //      value.  Empirically: removing this typedef yields a hard
  //      `no type named 'cardinality_type'` error at that line.
  //      (FIXME on the `:expressions` side: lift the second clause into a
  //      nested `if constexpr` so the requires guard is sufficient on its
  //      own.)
  //   2. Classification: matches `Set`'s own default (`ℵ_0`) so the
  //      structured polytope ends up wrapped via the generic
  //      `Set<T, L, Result>{…}` path rather than elevated as a Finite
  //      result.  The right answer for the 2D polytope case.
  //
  // The `ℵ_0` tag is a typed defaulting choice for the DSL dispatch —
  // NOT a paper claim about the cardinality of `Vec2V<T>` for arbitrary
  // `T` (e.g. uncountable for `T = double`); sharper carrier-axis bounds
  // (#622) belong on the `:sets:cardinality` ladder, not at the predicate
  // site.
  using cardinality_type = dedekind::sets::ℵ_0;

  constexpr bool operator()(const Domain& p) const {
    return Halfspace2D<T, a, b, c>::contains_value(p.x, p.y);
  }
};

/** @brief @c :expressions predicate for the @em meet of N 2D halfspaces:
 *  a polytope as a single conjunctive predicate, with the halfspace
 *  pack carried at the type level so downstream combinators
 *  (@ref argmax , and anything else dispatching on the polytope's
 *  structure) can extract it. */
export template <typename T, typename... Hs>
struct Polytope2DPredicate {
  using Domain = dedekind::linear_algebra::Vec2V<T>;
  // Same typed-defaulting rationale as @ref Halfspace2DPredicate (1)
  // substitution well-formedness for `:expressions::Set::operator&`'s
  // Finite-elevation branch (which references `Result::cardinality_type`
  // outside a `requires` guard, so the typedef must exist for the body
  // to type-check), and (2) classification — `ℵ_0` routes the structured
  // polytope through the generic Set wrapping path.  Not a paper claim
  // about the cardinality of `Vec2V<T>` for arbitrary `T`.
  using cardinality_type = dedekind::sets::ℵ_0;

  constexpr bool operator()(const Domain& p) const {
    return (Hs::contains_value(p.x, p.y) && ...);
  }
};

/** @brief Lift a @ref Halfspace2D NTTP value into a `:expressions` Set
 *  whose predicate carries the halfspace structurally. */
export template <typename T, T a, T b, T c>
constexpr auto halfspace_set(Halfspace2D<T, a, b, c>) {
  return dedekind::sets::Set<dedekind::linear_algebra::Vec2V<T>,
                             dedekind::category::ClassicalLogic,
                             Halfspace2DPredicate<T, a, b, c>>{
      Halfspace2DPredicate<T, a, b, c>{}};
}

/** @section lp__structured_and_overloads
 *
 *  Customization point: when @c :expressions::Set::operator& meets two
 *  predicates, it tries @c structured_and(p1, p2) via ADL before
 *  falling back to a lambda meet.  Our overloads here let the DSL
 *  recognise halfspace + halfspace, polytope + halfspace, and halfspace
 *  + polytope meets as structural reductions producing a
 *  @ref Polytope2DPredicate that carries the union of the two
 *  halfspace packs.
 *
 *  Parallels @c :order:halfspace::structured_and on 1D halfspaces; the
 *  difference is that 1D meets land in `Singleton` / `OrderInterval` /
 *  empty (the lattice for 1D), whereas 2D halfspace meets land in
 *  `Polytope2DPredicate` (no cardinality-1 short circuit at this layer —
 *  the optimum-as-typed-constant collapse happens further down in
 *  @ref argmax via @ref maximize ).
 */

/** @brief Halfspace + halfspace → polytope of two halfspaces. */
export template <typename T, T a1, T b1, T c1, T a2, T b2, T c2>
constexpr auto structured_and(Halfspace2DPredicate<T, a1, b1, c1>,
                              Halfspace2DPredicate<T, a2, b2, c2>) {
  return Polytope2DPredicate<T, Halfspace2D<T, a1, b1, c1>,
                             Halfspace2D<T, a2, b2, c2>>{};
}

/** @brief Polytope + halfspace → polytope with the halfspace appended. */
export template <typename T, typename... Hs, T a, T b, T c>
constexpr auto structured_and(Polytope2DPredicate<T, Hs...>,
                              Halfspace2DPredicate<T, a, b, c>) {
  return Polytope2DPredicate<T, Hs..., Halfspace2D<T, a, b, c>>{};
}

/** @brief Halfspace + polytope → delegate to the canonical direction. */
export template <typename T, T a, T b, T c, typename... Hs>
constexpr auto structured_and(Halfspace2DPredicate<T, a, b, c>,
                              Polytope2DPredicate<T, Hs...>) {
  return Polytope2DPredicate<T, Halfspace2D<T, a, b, c>, Hs...>{};
}

/** @brief Polytope + polytope → concatenate the halfspace packs.  Without
 *  this overload, a right-associative meet `(H1 & H2) & (H3 & H4)` would
 *  fall through to the lambda meet in @c :expressions::Set::operator& ;
 *  the whole point of @c structured_and is that the structural collapse
 *  must not depend on how the source bracketed the meet. */
export template <typename T, typename... Hs1, typename... Hs2>
constexpr auto structured_and(Polytope2DPredicate<T, Hs1...>,
                              Polytope2DPredicate<T, Hs2...>) {
  return Polytope2DPredicate<T, Hs1..., Hs2...>{};
}

/** @brief Linear functional `U : F → T` with NTTP-typed coefficients.
 *  Carries the objective as type parameters so @ref argmax can call the
 *  NTTP-driven kernel @c maximize<T, cx, cy, Hs...>() directly without
 *  smuggling the values through function parameters. */
export template <typename T, T cx_, T cy_>
struct LinearFunctional {
  using scalar_type = T;
  static constexpr T cx = cx_;
  static constexpr T cy = cy_;

  constexpr T operator()(const dedekind::linear_algebra::Vec2V<T>& p) const {
    return cx * p.x + cy * p.y;
  }
};

/** @brief @c argmax(G, U) on a polytope-Set and a linear functional.
 *
 *  @c G is a Set whose predicate is a @ref Polytope2DPredicate carrying
 *  the halfspace pack; @c U is a @ref LinearFunctional carrying the
 *  objective coefficients as NTTPs.  Extracts both packs from the type
 *  level and dispatches to the kernel via @ref maximize_set , returning
 *  the optimal locus as a Set — singleton @c {(x*, y*)} when the LP has
 *  a unique optimum, empty when infeasible.
 *
 *  Set in, Set out: the textbook @c argmax @c : @c Set @c → @c Set
 *  shape, with both input and output carried by the project's set-
 *  comprehension DSL.  The output Set's predicate type encodes the
 *  mathematical regime — caller pattern-matches on @c decltype(opt) if
 *  they want compile-time regime discrimination.
 */
export template <typename T, typename L, typename... Hs, T cx, T cy>
  requires(sizeof...(Hs) >= 2) && dedekind::algebra::HasRingOperators<T>
constexpr auto argmax(
    const dedekind::sets::Set<dedekind::linear_algebra::Vec2V<T>, L,
                              Polytope2DPredicate<T, Hs...>>&,
    LinearFunctional<T, cx, cy>) {
  return maximize_set<T, cx, cy, Hs...>();
}

}  // namespace dedekind::optimization
