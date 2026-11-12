/**
 * @file dedekind/optimization/lp.cppm
 * @partition :lp
 * @brief Linear programming in 2D: one constexpr kernel, its optimum a Set.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section lp__Overview
 * A 2D LP over an ordered ring-like scalar: halfspaces `a·x + b·y ≤ c` as
 * values (@ref HalfspaceTriple), an objective `(cx, cy)`, and one
 * `constexpr` kernel that enumerates the active sets, solves each by
 * Cramer's rule, and keeps the feasible vertex with the greatest objective.
 * The optimum is a Set: the singleton of the vertex, or empty where the LP
 * is infeasible (@ref LPSolutionPredicate).  Called with constant
 * arguments the kernel is evaluated at compile time, on values; called at
 * run time it runs.
 *
 * @section lp__Paper_Facing_Showcase
 *     maximize 3x + 2y
 *     subject to  x +  y ≤ 4       (H1)
 *                 2x +  y ≤ 6       (H2)
 *                -x       ≤ 0       (H3:  x ≥ 0)
 *                     -y  ≤ 0       (H4:  y ≥ 0)
 *
 *     → optimum (2, 2), objective value = 10.
 *
 * Active set at the optimum: {H1, H2}, both non-axis-aligned, det = −1.
 *
 * @section lp__Scope
 * - 2D polytopes only (rank-2 narrowing from #364).
 * - **Feasible + bounded** inputs only. An infeasible polytope surfaces as
 *   the empty optimum.  Unbounded polytopes are *not* detected:
 *   enumeration returns the best vertex among the ${n \choose 2}$
 *   active-set candidates, so an unbounded objective yields a finite
 *   vertex that is not the true supremum.  Unbounded-LP detection is
 *   tracked as a follow-up.
 * - Over ℚ (@c RatExpr) every vertex is exact, each store canonical; over a
 *   word, Cramer's division is exact where the determinants are ±1.
 *   `Dual<RatExpr>` layers parametric sensitivity analysis.
 *
 * @note "Modellbildung ist die halbe Miete."
 *       (Modelling is half the battle.)
 *       — Operations-research folk wisdom.
 */
module;

#include <concepts>
#include <cstddef>
#include <limits>
#include <span>

export module dedekind.optimization:lp;

import dedekind.algebra;        // HasRingOperators constraint
import dedekind.category;       // Boole — Set's logic species (#747)
import dedekind.numbers;        // RatExpr<>
import dedekind.linear_algebra; // Vec2V: the optimum's point
import dedekind.sets;           // Set — DSL participants (#747)

namespace dedekind::optimization {

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

/** @brief A halfspace @c a·x + b·y ≤ c, as the value the kernel consumes.
 */
export template <typename T>
struct HalfspaceTriple {
  T a;
  T b;
  T c;
};

/** @brief Reduction output: optimum coordinates with a feasibility flag,
 *  which distinguishes a genuine optimum from an empty feasible region.
 */
export template <typename T>
struct VertexCandidate {
  T x;
  T y;
  bool feasible;
};

/** @section lp__LP_Reduction_Internals
 *
 *  The kernel (@ref detail::maximize_impl) is one @c constexpr function:
 *  handed a span over a @c constexpr @c std::array it is evaluated at
 *  compile time, over a @c std::vector at run time.  Same code path, two
 *  evaluation modes, selected by the constexpr-ness of the arguments.
 */
namespace detail {

/**
 * @brief 2×2 active-set solve for the intersection of two halfspaces,
 *        treated as equalities: `a1 x + b1 y = c1`, `a2 x + b2 y = c2`.
 *
 * Cramer's rule, dividing by the determinant of the active-set matrix.  The
 * determinant is a divisor only where the carrier certifies it (@c :total's
 * @c divisor: ℚ's non-zero, 𝔻's units, the word itself), so the vertex is a
 * value exactly there; a singular matrix (the two halfspaces are parallel; no
 * vertex) returns the infeasible sentinel.  The check is the one place the
 * refusal lives; the division is total on what passes it.
 */
template <typename T>
constexpr VertexCandidate<T> solve_active_set(const HalfspaceTriple<T>& h1,
                                              const HalfspaceTriple<T>& h2) {
  using dedekind::sets::canonical;
  const T det = canonical(h1.a * h2.b - h1.b * h2.a);
  const auto d = dedekind::category::divisor(det);
  if (!d) return {T{}, T{}, false};  // singular: parallel
  // Each store is canonical: the identity on a word, the normal form on a
  // term, so a vertex of ℚ stays within its term's bound.
  const T x = canonical((h1.c * h2.b - h1.b * h2.c) / *d);
  const T y = canonical((h1.a * h2.c - h1.c * h2.a) / *d);
  return {x, y, true};
}

/**
 * @brief Argmax over feasible vertex-candidates under the objective
 *        `cx · x + cy · y`.  The co-Kleisli argmax on the candidate
 *        path; returns `{x*, y*, true}` on success, `{0, 0, false}` on
 *        an empty feasible set.
 *
 * @details One @c constexpr kernel for both evaluation modes.  Called
 * with a @c constexpr @c std::span, the reduction is evaluated at
 * translation time and the optimum is a constant.  Called with a runtime
 * @c std::span (e.g. over a @c std::vector materialised from a Python
 * list of triples), the same function runs at runtime — same code
 * path, same active-set enumeration, same carrier-aware
 * divisor check.  This is the
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

      using dedekind::sets::canonical;  // a term's, or a hidden friend's
      const T obj = canonical(cx * v.x + cy * v.y);
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
 * @details Precondition (checked by the dispatch in
 * @ref maximize_with_values , and defended by the kernel itself): every
 * halfspace in @c constraints has
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
  // value type and on libstdc++ pulls in @c RatExpr / @c IntExpr
  // operators that are not viable for iterator operands but still
  // confuse overload resolution.  Indexed iteration matches the style
  // of the generic Cramer kernel above and avoids the iterator-comparison
  // path entirely.
  const std::size_t N = constraints.size();
  for (std::size_t i = 0; i < N; ++i) {
    const auto& h = constraints[i];
    // Precondition guard: each halfspace must be axis-aligned with
    // @c (a, b) ∈ {−1, 0, +1}².  The exported entry
    // @ref maximize_axis_aligned_with_values is
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
    // carriers without @c numeric_limits specialisation (RatExpr, Dual)
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

/** @brief @c :expressions predicate carrying the LP's runtime optimal
 *  locus as @c (point, feasible) value-level state.
 *
 *  @details Used as the predicate of the runtime entries' output Set:
 *  membership is @c feasible @c && @c v @c == @c point , so the Set
 *  is a singleton @c {point} when the LP is feasible and empty when
 *  it isn't.  Callers may use @c Set::contains for membership queries
 *  or inspect the predicate's @c point / @c feasible directly when
 *  scalar extraction is needed (e.g. interop with Python).
 */
export template <typename T>
struct LPSolutionPredicate {
  using Domain = dedekind::linear_algebra::Vec2V<T>;
  using cardinality_type = dedekind::sets::Finite;

  Domain point;
  bool feasible;

  constexpr bool operator()(const Domain& v) const {
    return feasible && v.x == point.x && v.y == point.y;
  }
};

/** @brief Lift a runtime optimal locus into a @c :expressions Set
 *  whose predicate carries @c (point, feasible) at the value level.
 *  The Set is a singleton @c {point} when feasible, empty when
 *  not, with the same Set type either way. */
export template <typename T>
constexpr auto lp_runtime_solution_set(dedekind::linear_algebra::Vec2V<T> point,
                                       bool feasible) {
  return dedekind::sets::Comprehension<
      dedekind::sets::𝔸<dedekind::linear_algebra::Vec2V<T>,
                        dedekind::category::Boole>,
      LPSolutionPredicate<T>>{LPSolutionPredicate<T>{point, feasible}};
}

/** @section lp__F_Algebra_Witness
 *
 *  Inline type-witness: the output Set of the reduction is an F-algebra
 *  (Mac~Lane ch.~V, Pierce~1991 §5; the project's @c category::IsFAlgebra
 *  concept).  This pins the catamorphism @em shape mechanically — the
 *  output Set carries a structure map @c F<X> @c → @c X for the
 *  carrier @c X @c = @c Set<Vec2V<T>, Boole,
 *  LPSolutionPredicate<T>> .  The witness uses the identity endofunctor
 *  ( @c F(X) @c = @c X ); the catamorphism interpretation — that
 *  the reduction is the unique morphism into this algebra from the initial
 *  halfspace-list F-algebra carrying the polytope's pack — additionally
 *  requires F to be the halfspace-list functor and the opt-in
 *  @c is_initial_f_algebra_v registration that captures the universal
 *  property's uniqueness clause (engineer's honesty obligation per
 *  @c category:f_algebra ; deferred here as the halfspace-list endofunctor
 *  needs its own @c Σ_cat / @c Shape / @c φ scaffolding).
 *
 *  See also @c [optimization:lp][f-algebra][catamorphism] in @c lp_test.cpp
 *  for the test-side static_assert and the parallel @c IsSet witness.
 */
namespace detail {

template <typename T>
using LPSolutionSet = dedekind::sets::Comprehension<
    dedekind::sets::𝔸<dedekind::linear_algebra::Vec2V<T>,
                      dedekind::category::Boole>,
    LPSolutionPredicate<T>>;

template <typename T>
using LPSolutionIdF = dedekind::category::identity_functor<
    dedekind::category::DiscreteCategory<LPSolutionSet<T>>>;

/** @brief Shape-level @c F-algebra structure map on the output Set:
 *  an endomap representing the "active-set step" at the type level.
 *  Concrete LP folds close over halfspaces internally; the
 *  shape-level witness here is halfspace-agnostic, which is what
 *  F-algebra structure abstracts. */
template <typename T>
constexpr auto lp_step_arrow =
    dedekind::category::arrow([](const LPSolutionSet<T>& s) { return s; });

}  // namespace detail

// Inline type witness: the output Set is an F-algebra for the
// identity endofunctor — the catamorphism shape on the strict-form
// side of the reduction.  Carrier is @c RatExpr (matches the
// paper-facing T = ℚ); the witness lifts uniformly to any carrier
// satisfying the same shape.
static_assert(dedekind::category::IsFAlgebra<
              detail::LPSolutionSet<dedekind::sets::RatExpr<>>,
              std::decay_t<
                  decltype(detail::lp_step_arrow<dedekind::sets::RatExpr<>>)>,
              detail::LPSolutionIdF<dedekind::sets::RatExpr<>>>);

/** @section lp__The_Bridge
 *
 *  @ref maximize_with_values is the single user-facing entry point for
 *  the 2D LP reduction.  It is @c constexpr, takes a
 *  @c std::span<const HalfspaceTriple<T>> and a @c (cx, cy) objective,
 *  and delegates to the @c constexpr kernel @ref detail::maximize_impl.
 *  Selection between evaluation modes is a property of the call site:
 *
 *  - Called with a span over a @c constexpr @c std::array, every
 *    argument is a constant expression and the call is evaluated at
 *    translation time.
 *
 *  - Called with a span over runtime data (e.g. a @c std::vector
 *    populated from a Python list of triples through the nanobind
 *    facade), the arguments are not constant expressions and the call
 *    runs at runtime.
 *
 *  One function, two modes: compile-time and run-time evaluation on one
 *  definition.
 */
export template <typename T>
  requires dedekind::algebra::HasRingOperators<T>
constexpr auto maximize_with_values(
    std::span<const HalfspaceTriple<T>> halfspaces, T cx, T cy) {
  // Mechanical dispatch: scan the span for structural fast-path
  // eligibility (every halfspace has @c (a, b) ∈ {−1, 0, +1}² with at
  // least one zero coefficient) and route to the bit-ops kernel when
  // it qualifies, else the generic Cramer kernel: the kernel is selected
  // from the input's structure.
  VertexCandidate<T> v{T{}, T{}, false};
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
      v = detail::maximize_impl_axis_aligned<T>(halfspaces, cx, cy);
    } else {
      v = detail::maximize_impl<T>(halfspaces, cx, cy);
    }
  } else {
    v = detail::maximize_impl<T>(halfspaces, cx, cy);
  }
  return lp_runtime_solution_set<T>(
      dedekind::linear_algebra::Vec2V<T>{v.x, v.y}, v.feasible);
}

/** @brief Runtime entry for the bit-ops fast-path kernel.  Same shape as
 *  @ref maximize_with_values , which dispatches here only on an eligible
 *  pack; this entry is also reachable directly, so the kernel
 *  defensively validates each halfspace and collapses the result to the
 *  empty Set (the output Set's @c LPSolutionPredicate carries
 *  @c feasible @c == @c false at the value level) on:
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
 *  Carrier constraint @ref SuitableForAxisAlignedFastPath names every
 *  operation the kernel uses ( @c T{} , @c T{1} , ring ops,
 *  ordering, signedness via @c (T{} @c − @c T{1}) @c < @c T{} ) so
 *  unsupported carriers fail at the API boundary rather than inside the
 *  function body.
 */
export template <typename T>
  requires SuitableForAxisAlignedFastPath<T>
constexpr auto maximize_axis_aligned_with_values(
    std::span<const HalfspaceTriple<T>> halfspaces, T cx, T cy) {
  const auto v = detail::maximize_impl_axis_aligned<T>(halfspaces, cx, cy);
  return lp_runtime_solution_set<T>(
      dedekind::linear_algebra::Vec2V<T>{v.x, v.y}, v.feasible);
}

/** @brief Power-user runtime entry that forces the generic Cramer
 *  kernel and bypasses the mechanical dispatch in @ref
 *  maximize_with_values .  Same shape and requires as @ref
 *  maximize_with_values ; sibling of @ref maximize_axis_aligned_with_values
 *  on the Cramer side.
 *
 *  Intended for callers who already know they want Cramer (e.g. IR
 *  fixtures isolating the kernel's residual @c fdiv @c + @c fmuladd
 *  signature, or microbenchmarks).  Library default users should call
 *  @ref maximize_with_values and let the type system pick the kernel
 *  from the input's structure.
 */
export template <typename T>
  requires dedekind::algebra::HasRingOperators<T>
constexpr auto maximize_cramer_with_values(
    std::span<const HalfspaceTriple<T>> halfspaces, T cx, T cy) {
  const auto v = detail::maximize_impl<T>(halfspaces, cx, cy);
  return lp_runtime_solution_set<T>(
      dedekind::linear_algebra::Vec2V<T>{v.x, v.y}, v.feasible);
}

}  // namespace dedekind::optimization
