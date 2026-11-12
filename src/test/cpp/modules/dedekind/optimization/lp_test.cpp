/** @file dedekind/optimization/lp_test.cpp
 *
 * Unit coverage for the `:lp` partition of `dedekind.optimization`.
 * The paper-facing instance is the §5 polytope, its optimum (2, 2)
 * evaluated at compile time on values (`constexpr`, `STATIC_CHECK`), with a
 * non-axis-aligned active set `{H1, H2}` solved by Cramer's rule.
 */

#include <array>
#include <catch2/catch_test_macros.hpp>
#include <limits>
#include <span>
#include <vector>

import dedekind.analysis; // Dual<F> (relocated from :numbers at PR #513)
import dedekind.category; // Boole — Set's logic species (#747)
import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.optimization;
import dedekind.sets;

using namespace dedekind::analysis;  // Dual<F>
using namespace dedekind::linear_algebra;
using namespace dedekind::numbers;
using namespace dedekind::optimization;
using dedekind::sets::RatExpr;
namespace {
// Most instances are over the word, exact on them: the active sets'
// determinants are ±1 (the §5 polytope's {H1, H2} has det −1), so Cramer's
// division is exact in `long`.  ℚ is tested at the end of this file.
using Rat = long;

// The paper-facing LP instance (§5 candidate centrepiece):
//   maximize 3x + 2y
//   s.t.   x +  y ≤ 4      (H1)  — non-axis-aligned
//          2x +  y ≤ 6      (H2)  — non-axis-aligned
//         -x      ≤ 0       (H3:  x ≥ 0)
//              -y ≤ 0       (H4:  y ≥ 0)
//   Active set at the optimum: {H1, H2}; optimum at (2, 2), obj = 10.
}  // namespace

TEST_CASE(
    "optimization:lp — output Set is an F-algebra (catamorphism shape) (#749)",
    "[optimization][lp][set-out][f-algebra][catamorphism]") {
  // The output of the reduction, a Set, is structurally
  // an F-algebra: the output Set carries a structure map @c F<X> @c → @c X
  // for the carrier @c X = @c Set<Vec2V<T>, Boole,
  // LPSolutionPredicate<T>> .  The catamorphism (fold) interpretation
  // is then the canonical morphism into this algebra from the initial
  // halfspace-list F-algebra carrying the polytope's pack.
  //
  // We witness the F-algebra @em shape mechanically here; the
  // universal-property layer (initiality of the halfspace pack as an
  // F-algebra for @c F(X) @c = @c 1 @c + @c HalfspaceTriple<T> @c × @c X )
  // is the engineer's honesty obligation per the project's @c
  // category:f_algebra discipline — opt-in @c is_initial_f_algebra_v
  // registration is the documented escape hatch, deferred here as the
  // halfspace-list endofunctor's full @c IsEndofunctor instantiation
  // (Σ_cat, Shape, φ) is its own scaffolding task; see the source
  // commentary @c lp__F_Algebra_Witness in @c lp.cppm .
  using OutputSet = dedekind::sets::Comprehension<
      dedekind::sets::𝔸<Vec2V<Rat>, dedekind::category::Boole>,
      LPSolutionPredicate<Rat>>;
  using LPSolutionCat = dedekind::category::DiscreteCategory<OutputSet>;
  using LPSolutionIdF = dedekind::category::identity_functor<LPSolutionCat>;

  // The "active-set step" Arrow at the F-algebra shape level: an
  // endomap on the output Set.  Concrete LP folds compose chains of
  // these (each step closes over one halfspace internally); the
  // shape-level witness here is independent of any particular
  // halfspace, which is exactly what F-algebra structure abstracts.
  constexpr auto step =
      dedekind::category::arrow([](const OutputSet& s) { return s; });
  using StepArrow = std::decay_t<decltype(step)>;

  STATIC_CHECK(
      dedekind::category::IsFAlgebra<OutputSet, StepArrow, LPSolutionIdF>);
  // The initial-F-algebra opt-in is deliberately false here; the
  // catamorphism reading is documented prose, not a mechanical
  // universal-property witness.
  STATIC_CHECK_FALSE(
      dedekind::category::is_initial_f_algebra_v<LPSolutionIdF, OutputSet,
                                                 StepArrow>);
}

TEST_CASE("optimization:lp — the output satisfies IsSet (#749)",
          "[optimization][lp][set-out][is-set]") {
  // The optimum is a Set: the LPSolutionPredicate carrying (point,
  // feasible) at the value level, what maximize_with_values returns.
  using F = Vec2V<Rat>;
  using L = dedekind::category::Boole;
  using RuntimeOut = dedekind::sets::Comprehension<dedekind::sets::𝔸<F, L>,
                                                   LPSolutionPredicate<Rat>>;
  STATIC_CHECK(dedekind::category::IsSet<RuntimeOut>);
  STATIC_CHECK(dedekind::category::IsSet<dedekind::sets::Ø<F, L>>);
}

TEST_CASE("optimization:lp — bit-ops fast-path triptych dispatch (#749)",
          "[optimization][lp][triptych][fast-path]") {
  // Unit-square pack: signed-unimodular AND axis-aligned, the fast path.
  constexpr std::array<HalfspaceTriple<Rat>, 4> kUnitSquare = {{
      {Rat{1L}, Rat{0L}, Rat{1L}},
      {Rat{0L}, Rat{1L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
  }};
  constexpr auto v_pp = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kUnitSquare), Rat{1L}, Rat{1L});
  STATIC_CHECK(v_pp(Vec2V<Rat>{Rat{1L}, Rat{1L}}));
  STATIC_CHECK_FALSE(v_pp(Vec2V<Rat>{Rat{0L}, Rat{0L}}));

  // Negative-x objective: the fast path picks x_lo via sign selection.
  constexpr auto v_np = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kUnitSquare), Rat{-1L}, Rat{1L});
  STATIC_CHECK(v_np(Vec2V<Rat>{Rat{0L}, Rat{1L}}));
  STATIC_CHECK_FALSE(v_np(Vec2V<Rat>{Rat{1L}, Rat{1L}}));
}

TEST_CASE(
    "optimization:lp — triptych refusal: infeasible axis-aligned pack (#749)",
    "[optimization][lp][triptych][refusal]") {
  // Empty feasible region: x ≤ 0 ∧ x ≥ 1 ∧ 0 ≤ y ≤ 1.  The entry returns
  // @c feasible == false: the refusal is a value of the output Set.  A
  // negative- compile exhibit (@c .fail.cpp ) for the @c static_assert path is
  // deferred to a follow-up (needs CMake @c try_compile infra).
  constexpr std::array<HalfspaceTriple<Rat>, 4> kEmptyXInterval = {{
      {Rat{1L}, Rat{0L}, Rat{0L}},    //  x ≤ 0
      {Rat{-1L}, Rat{0L}, Rat{-1L}},  // -x ≤ -1, i.e. x ≥ 1
      {Rat{0L}, Rat{1L}, Rat{1L}},    //  y ≤ 1
      {Rat{0L}, Rat{-1L}, Rat{0L}},   // -y ≤ 0, i.e. y ≥ 0
  }};
  constexpr auto v = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kEmptyXInterval), Rat{1L}, Rat{1L});
  // Infeasible LP collapses to the empty Set — no point is in it.
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{1L}, Rat{1L}}));
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{0L}, Rat{0L}}));
}

TEST_CASE(
    "optimization:lp — fast-path defensive validation on runtime entry (#749)",
    "[optimization][lp][triptych][refusal]") {
  // Two precondition violations the runtime entry must collapse to the
  // infeasible sentinel rather than silently mis-solve: the entry is
  // reachable directly and the kernel defends Honest Rejection.

  // Zero-row halfspace with negative bound: @c 0·x + 0·y ≤ -1 is
  // vacuously unsatisfiable, the whole polytope collapses to empty.
  constexpr std::array<HalfspaceTriple<Rat>, 5> kZeroRow = {{
      {Rat{1L}, Rat{0L}, Rat{1L}},
      {Rat{0L}, Rat{1L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
      {Rat{0L}, Rat{0L}, Rat{-1L}},  // 0·x + 0·y ≤ -1 — infeasible row
  }};
  constexpr auto v_zero = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kZeroRow), Rat{1L}, Rat{1L});
  STATIC_CHECK_FALSE(v_zero(Vec2V<Rat>{Rat{1L}, Rat{1L}}));
  STATIC_CHECK_FALSE(v_zero(Vec2V<Rat>{Rat{0L}, Rat{0L}}));

  // Out-of-range coefficient: @c 2·x ≤ 4 is axis-aligned but not
  // signed-unimodular.  The kernel rejects rather than treating it as
  // @c +1·x ≤ 4 (which would silently set @c x_hi = 4 instead of 2).
  constexpr std::array<HalfspaceTriple<Rat>, 4> kOutOfRange = {{
      {Rat{2L}, Rat{0L}, Rat{4L}},  // coeff_x = 2 ∉ {−1, 0, +1}
      {Rat{0L}, Rat{1L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
  }};
  constexpr auto v_out = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kOutOfRange), Rat{1L}, Rat{1L});
  STATIC_CHECK_FALSE(v_out(Vec2V<Rat>{Rat{1L}, Rat{1L}}));

  // Diagonal halfspace: unit-range entries but BOTH nonzero.  The
  // kernel rejects rather than reading @c (1, 1, 2) as @c x ≤ 2 .
  constexpr std::array<HalfspaceTriple<Rat>, 5> kDiagonal = {{
      {Rat{1L}, Rat{0L}, Rat{1L}},
      {Rat{0L}, Rat{1L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
      {Rat{1L}, Rat{1L}, Rat{2L}},  // diagonal: not axis-aligned
  }};
  constexpr auto v_diag = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kDiagonal), Rat{1L}, Rat{1L});
  STATIC_CHECK_FALSE(v_diag(Vec2V<Rat>{Rat{1L}, Rat{1L}}));
}

TEST_CASE("optimization:lp — fast-path concept rejects unsigned carrier (#749)",
          "[optimization][lp][triptych][concept]") {
  // The fast-path requires clause uses `(T{} - T{1}) < T{}` to exclude
  // unsigned integral carriers, where `0u - 1u == UINT_MAX` would pass
  // the kernel's @c neg_one validation and be misread as a positive
  // coefficient.  The constraint failure surfaces at the API boundary,
  // not inside the kernel.  Test the concept directly rather than via
  // a SFINAE call site — clang under -Werror promotes the "no matching
  // function" diagnostic to a hard error before the @c requires
  // expression evaluates to false.
  STATIC_CHECK_FALSE(SuitableForAxisAlignedFastPath<unsigned int>);
  STATIC_CHECK(SuitableForAxisAlignedFastPath<int>);
  STATIC_CHECK(SuitableForAxisAlignedFastPath<Rat>);
}

TEST_CASE("optimization:lp — fast-path INT_MIN boundary guard (#749)",
          "[optimization][lp][triptych][refusal]") {
  // For signed integer carriers, `zero - h.c` is undefined behaviour
  // when `h.c == INT_MIN`.  The kernel uses `std::numeric_limits<T>`
  // to detect this and collapse to the infeasible sentinel; carriers
  // without `numeric_limits` specialisation (RatExpr, Dual) bypass
  // the check.
  constexpr int kIntMin = std::numeric_limits<int>::min();
  constexpr std::array<HalfspaceTriple<int>, 4> kIntMinPack = {{
      {1, 0, 1},
      {0, 1, 1},
      {-1, 0, kIntMin},  // -x ≤ INT_MIN: zero - INT_MIN is UB
      {0, -1, 0},
  }};
  constexpr auto v = maximize_axis_aligned_with_values<int>(
      std::span<const HalfspaceTriple<int>>(kIntMinPack), 1, 1);
  STATIC_CHECK_FALSE(v(Vec2V<int>{1, 1}));
}

// The §5 polytope as a constexpr array: shared by the compile-time tests
// below and the bridge witness, the same call as the runtime path, with
// constexpr inputs.
constexpr std::array<HalfspaceTriple<Rat>, 4> kPolytope = {{
    {Rat{1L}, Rat{1L}, Rat{4L}},   // H1:  x +  y ≤ 4
    {Rat{2L}, Rat{1L}, Rat{6L}},   // H2:  2x + y ≤ 6
    {Rat{-1L}, Rat{0L}, Rat{0L}},  // H3:  x      ≥ 0
    {Rat{0L}, Rat{-1L}, Rat{0L}},  // H4:       y ≥ 0
}};

TEST_CASE(
    "optimization:lp — the §5 polytope's optimum is (2, 2), "
    "evaluated at compile time",
    "[optimization][lp][centrepiece]") {
  constexpr auto v = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kPolytope), Rat{3L}, Rat{2L});
  STATIC_CHECK(v(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{0L}, Rat{0L}}));
}

TEST_CASE("optimization:lp — axis-aligned corner is pruned away",
          "[optimization][lp][centrepiece]") {
  // Sanity check: with the objective direction (3, 2), the candidate
  // (0, 4) has obj = 0 + 8 = 8; (3, 0) has obj = 9 + 0 = 9; (2, 2) wins
  // at obj = 10. The reduction correctly picks the non-axis-aligned
  // intersection — not (0, 4), not (3, 0), not (0, 0).
  constexpr auto v = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kPolytope), Rat{3L}, Rat{2L});
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{0L}, Rat{4L}}));
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{3L}, Rat{0L}}));
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{0L}, Rat{0L}}));
}

TEST_CASE(
    "optimization:lp — maximize_with_values mechanically dispatches (#749)",
    "[optimization][lp][set-out][mechanical-dispatch]") {
  // The single public runtime entry @c maximize_with_values now scans
  // its input for fast-path eligibility and routes to the bit-ops
  // kernel when every halfspace is signed-unimodular axis-aligned;
  // otherwise falls through to the generic Cramer kernel.  The wart of
  // having two public runtime entries the caller had to pick from is
  // gone — the type system selects the kernel from the input's
  // structure end-to-end.

  // Unit-square pack → fast-path kernel routes; same answer (1, 1).
  constexpr std::array<HalfspaceTriple<Rat>, 4> kUnitSquare = {{
      {Rat{1L}, Rat{0L}, Rat{1L}},
      {Rat{0L}, Rat{1L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
  }};
  constexpr auto via_auto = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kUnitSquare), Rat{1L}, Rat{1L});
  STATIC_CHECK(via_auto(Vec2V<Rat>{Rat{1L}, Rat{1L}}));

  // §5 polytope (H2's coeff_x = 2): not fast-eligible.  Cramer routes
  // and computes (2, 2) via the active-set enumeration.
  constexpr auto via_generic = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kPolytope), Rat{3L}, Rat{2L});
  STATIC_CHECK(via_generic(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
}

TEST_CASE("optimization:lp — infeasible polytope reports no optimum",
          "[optimization][lp][infeasible]") {
  // Intersect x ≤ 1 with x ≥ 3 (i.e. -x ≤ -3): infeasible — no (x, y) is
  // in both halfspaces.  The bridge entry, called with constexpr inputs,
  // reports `!feasible` rather than returning a bogus vertex.
  constexpr std::array<HalfspaceTriple<Rat>, 3> infeasible = {{
      {Rat{1L}, Rat{0L}, Rat{1L}},    //  x ≤ 1
      {Rat{-1L}, Rat{0L}, Rat{-3L}},  // -x ≤ -3 (i.e. x ≥ 3)
      {Rat{0L}, Rat{1L}, Rat{5L}},    //  y ≤ 5
  }};
  constexpr auto v = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(infeasible), Rat{1L}, Rat{1L});
  // Infeasible LP collapses to the empty Set.
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{1L}, Rat{1L}}));
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{3L}, Rat{1L}}));
}

/**
 * Parametric LP over `Dual<ℚ>`: compile-time sensitivity analysis.
 *
 * Framing (after Elliott, *The Simple Essence of Automatic
 * Differentiation*, ICFP 2018): forward-mode AD is the product `(f, f')`
 * with chain-rule composition, not the "dual number" object per se.
 * `Dual<F>` is one realization of that product; `ftc::derivative_at` is
 * another (numerical, central difference); `Polynomial::derive(slot)` is
 * a third (exact formal, on the polynomial's sparse support). Here we use
 * `Dual<Rat>` because the product is then available at compile time.
 *
 * The LP reduction is generic over any `HasRingOperators` carrier with a
 * total order on the primal part; `Dual<Rat>` provides both (primal is
 * `Rat`, ordered; tangent rides along through the chain rule). Running
 * the same reduction over `Dual<Rat>` instead of plain `Rat` gives us
 * the optimum AND its sensitivity to a perturbed parameter, as one
 * `Vec2V<Dual<Rat>>` — no separate derivative pass.
 *
 * Showcase: perturb H1's bound by ε. The active set {H1', H2} gives
 *   x_opt = 2 - ε,   y_opt = 2 + 2ε
 * so the tangent of x_opt is -1 and the tangent of y_opt is +2 for the
 * perturbation parameter — the chain rule has already run, compile-time,
 * during the Cramer solve.
 */
TEST_CASE(
    "optimization:lp — parametric LP over Dual<ℚ>: "
    "optimum + sensitivity as one typed constant",
    "[optimization][lp][dual][sensitivity]") {
  using D = Dual<Rat>;

  // Polytope with H1's bound perturbed by ε.  Same shape as the §5
  // exhibit; instantiated at T = Dual<Rat> the kernel returns primal
  // AND first-order sensitivity from one call.
  constexpr std::array<HalfspaceTriple<D>, 4> dual_polytope = {{
      {D{Rat{1L}}, D{Rat{1L}}, D{Rat{4L}, Rat{1L}}},  // H1':  x +  y ≤ 4 + ε
      {D{Rat{2L}}, D{Rat{1L}}, D{Rat{6L}}},           // H2:  2x +  y ≤ 6
      {D{Rat{-1L}}, D{Rat{0L}}, D{Rat{0L}}},          // H3:  x      ≥ 0
      {D{Rat{0L}}, D{Rat{-1L}}, D{Rat{0L}}},          // H4:       y ≥ 0
  }};
  constexpr auto v = maximize_with_values<D>(
      std::span<const HalfspaceTriple<D>>(dual_polytope), D{Rat{3L}},
      D{Rat{2L}});

  // Expected optimum as a Vec2V<D>: x = D{2, -1} (primal 2, tangent -1)
  // and y = D{2, +2} (primal 2, tangent +2).  Set membership encodes
  // primal AND sensitivity correctness in one check — the LP's optimum
  // is the dual point AND the chain rule has already produced the
  // tangent components inside the Cramer solve.
  constexpr auto expected = Vec2V<D>{D{Rat{2L}, Rat{-1L}}, D{Rat{2L}, Rat{2L}}};
  STATIC_CHECK(v(expected));
  // The primal-only point (2, 2) — wrong tangents — is NOT in the Set:
  // dual equality demands both components match.
  STATIC_CHECK_FALSE(v(Vec2V<D>{D{Rat{2L}}, D{Rat{2L}}}));
}

/**
 * @brief Runtime-coefficient entry point: the paper's bridge in the
 *        opposite direction.  Same polytope as the compile-time case,
 *        but the constraints arrive at run time — the
 *        shape Python callers see through the nanobind facade.
 *
 * The two modes go through the same active-set kernel, so agreement on
 * the locked polytope is the first parity claim; a second polytope
 * guards against the runtime path being accidentally constant-folded by
 * coincidence.
 */
/**
 * @brief Bridge witness: @ref maximize_with_values is one @c constexpr
 *        function that serves both evaluation modes.
 *
 * Called with @c constexpr arguments, the same call folds at translation
 * time — witnessed here by a @c STATIC_CHECK on the result.  Called with
 * runtime arguments, the same call runs at runtime (the subsequent
 * @c TEST_CASE blocks exercise that mode).  Two modes, one function;
 * selection is a property of the call site, not the API surface.  This
 * is the bridge the paper's §2 prose names.
 */
TEST_CASE(
    "optimization:lp — bridge witness: maximize_with_values folds when "
    "its arguments are constexpr (#743)",
    "[optimization][lp][bridge][centrepiece]") {
  constexpr std::array<HalfspaceTriple<Rat>, 4> cs = {{
      {Rat{1L}, Rat{1L}, Rat{4L}},
      {Rat{2L}, Rat{1L}, Rat{6L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
  }};
  // The runtime entry point, called with constexpr inputs, must produce
  // a constant expression: this is the "same function, two modes" claim
  // checked mechanically rather than by prose.
  constexpr auto v = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(cs), Rat{3L}, Rat{2L});
  STATIC_CHECK(v(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
  STATIC_CHECK_FALSE(v(Vec2V<Rat>{Rat{0L}, Rat{0L}}));
}

TEST_CASE(
    "optimization:lp — runtime-coefficient entry point: "
    "same polytope, value-level inputs, identical optimum (#743)",
    "[optimization][lp][runtime][centrepiece]") {
  // Same instance as the centrepiece above, but values not types.
  std::vector<HalfspaceTriple<Rat>> halfspaces{
      {Rat{1L}, Rat{1L}, Rat{4L}},   //  x +  y ≤ 4   (H1)
      {Rat{2L}, Rat{1L}, Rat{6L}},   // 2x +  y ≤ 6   (H2)
      {Rat{-1L}, Rat{0L}, Rat{0L}},  //  x      ≥ 0   (H3)
      {Rat{0L}, Rat{-1L}, Rat{0L}},  //       y ≥ 0   (H4)
  };
  const auto result = maximize_with_values<Rat>(halfspaces, Rat{3L}, Rat{2L});

  CHECK(result(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
  CHECK_FALSE(result(Vec2V<Rat>{Rat{0L}, Rat{0L}}));
}

TEST_CASE(
    "optimization:lp — runtime-coefficient entry point: "
    "second polytope guards against coincidental constant-fold (#743)",
    "[optimization][lp][runtime]") {
  // A different polytope with a different optimum: max x + y over the
  // triangle {x ≥ 0, y ≥ 0, x + 2y ≤ 6, 2x + y ≤ 6}.  The optimum is
  // at the active set {x + 2y = 6, 2x + y = 6}, giving x = y = 2 and
  // objective 4 — distinct from the centrepiece's (2, 2, 10) only in
  // the objective value, so we also check the objective.
  std::vector<HalfspaceTriple<Rat>> halfspaces{
      {Rat{1L}, Rat{2L}, Rat{6L}},
      {Rat{2L}, Rat{1L}, Rat{6L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
  };
  const auto result = maximize_with_values<Rat>(halfspaces, Rat{1L}, Rat{1L});

  CHECK(result(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
  // Sanity: optimum is NOT one of the easy corners.
  CHECK_FALSE(result(Vec2V<Rat>{Rat{0L}, Rat{0L}}));

  // A third instance to disambiguate the optimum location too: shrink
  // the bound on the first halfspace so the vertex moves.
  std::vector<HalfspaceTriple<Rat>> shrunk{
      {Rat{1L}, Rat{2L}, Rat{3L}},
      {Rat{2L}, Rat{1L}, Rat{6L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
  };
  const auto shrunk_result =
      maximize_with_values<Rat>(shrunk, Rat{1L}, Rat{1L});
  // Active set {x + 2y = 3, 2x + y = 6}: solve gives x = 3, y = 0.
  CHECK(shrunk_result(Vec2V<Rat>{Rat{3L}, Rat{0L}}));
  CHECK_FALSE(shrunk_result(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
}

TEST_CASE(
    "optimization:lp — runtime entry point reports infeasibility "
    "without throwing (#743)",
    "[optimization][lp][runtime][infeasible]") {
  // x ≤ 1 ∧ x ≥ 3: empty feasible region.  The runtime path must report
  // @c !feasible — Python callers need to inspect the flag at run time.
  std::vector<HalfspaceTriple<Rat>> halfspaces{
      {Rat{1L}, Rat{0L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{-3L}},
      {Rat{0L}, Rat{1L}, Rat{5L}},
  };
  const auto result = maximize_with_values<Rat>(halfspaces, Rat{1L}, Rat{1L});
  CHECK_FALSE(result(Vec2V<Rat>{Rat{1L}, Rat{1L}}));
  CHECK_FALSE(result(Vec2V<Rat>{Rat{3L}, Rat{1L}}));
}

TEST_CASE(
    "optimization:lp — the value path over ℚ divides by a certified "
    "determinant",
    "[optimization][lp][rational][divisor]") {
  // maximize x + y  s.t.  x + 2y ≤ 4,  3x + y ≤ 6,  x ≥ 0,  y ≥ 0.  The
  // binding pair has det = 1·1 − 2·3 = −5, so the vertex (8/5, 6/5) is not
  // integral: over a word Cramer's division would truncate it, over ℚ it is
  // exact.  Each active set's determinant enters ℚ* through the one check
  // (:total's divisor), and a parallel pair is refused there.
  using Q = RatExpr<>;
  const std::array<HalfspaceTriple<Q>, 4> polytope = {{
      {Q{1}, Q{2}, Q{4}},
      {Q{3}, Q{1}, Q{6}},
      {Q{-1}, Q{0}, Q{0}},
      {Q{0}, Q{-1}, Q{0}},
  }};
  const auto v = maximize_with_values<Q>(
      std::span<const HalfspaceTriple<Q>>(polytope), Q{1}, Q{1});
  CHECK(v(Vec2V<Q>{Q{8, 5}, Q{6, 5}}));
  CHECK_FALSE(v(Vec2V<Q>{Q{2}, Q{0}}));
  CHECK_FALSE(v(Vec2V<Q>{Q{0}, Q{2}}));
  CHECK_FALSE(v(Vec2V<Q>{Q{1}, Q{1}}));
}
