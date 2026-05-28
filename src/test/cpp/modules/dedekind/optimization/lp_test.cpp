/** @file dedekind/optimization/lp_test.cpp
 *
 * Unit coverage for the `:lp` partition of `dedekind.optimization`.
 * The paper-facing existential proof is `maximize<3, 2, H1, H2, H3, H4>()`
 * reducing to `Vec2<Rat, 2, 2>` at compile time, with a non-axis-aligned
 * active set `{H1, H2}` solved via `Invertible2x2`.
 */

#include <array>
#include <catch2/catch_test_macros.hpp>
#include <span>
#include <vector>

import dedekind.analysis; // Dual<F> (relocated from :numbers at PR #513)
import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.optimization;
import dedekind.sets;

using namespace dedekind::analysis;  // Dual<F>
using namespace dedekind::linear_algebra;
using namespace dedekind::numbers;
using namespace dedekind::optimization;
using dedekind::sets::SignedExtensionalCardinal;

namespace {
// Arbitrary-precision signed rational — the canonical ℚ for these tests.
using Rat = Rational<SignedExtensionalCardinal<>>;

// The paper-facing LP instance (§5 candidate centrepiece):
//   maximize 3x + 2y
//   s.t.   x +  y ≤ 4      (H1)  — non-axis-aligned
//          2x +  y ≤ 6      (H2)  — non-axis-aligned
//         -x      ≤ 0       (H3:  x ≥ 0)
//              -y ≤ 0       (H4:  y ≥ 0)
//   Active set at the optimum: {H1, H2}; optimum at (2, 2), obj = 10.
using H1 = Halfspace2D<Rat, Rat{1L}, Rat{1L}, Rat{4L}>;
using H2 = Halfspace2D<Rat, Rat{2L}, Rat{1L}, Rat{6L}>;
using H3 = Halfspace2D<Rat, Rat{-1L}, Rat{0L}, Rat{0L}>;
using H4 = Halfspace2D<Rat, Rat{0L}, Rat{-1L}, Rat{0L}>;

}  // namespace

TEST_CASE("optimization:lp — Halfspace2D membership at the type level",
          "[optimization][lp][halfspace]") {
  // x + y ≤ 4
  STATIC_CHECK(H1::template contains<Rat{2L}, Rat{2L}>());        // boundary
  STATIC_CHECK(H1::template contains<Rat{1L}, Rat{1L}>());        // interior
  STATIC_CHECK_FALSE(H1::template contains<Rat{3L}, Rat{3L}>());  // exterior
}

TEST_CASE("optimization:lp — Polytope2D + lp_extract comonadic counit (#388)",
          "[optimization][lp][comonad][counit]") {
  // The polytope context (cx, cy, Hs...) reified as a type, with the
  // co-Kleisli counit lp_extract :: Polytope2D(T) → Vec2(T) delegating
  // to maximize<...>().  Pins both the constructibility of the
  // Polytope2D wrapper and the equivalence between extract() member,
  // free-function lp_extract, and the underlying maximize() — three
  // surfaces that must agree.
  using Poly = Polytope2D<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>;

  STATIC_CHECK(std::same_as<typename Poly::scalar_type, Rat>);
  STATIC_CHECK(Poly::objective_x == Rat{3L});
  STATIC_CHECK(Poly::objective_y == Rat{2L});

  constexpr Poly polytope{};
  constexpr auto via_extract_member = polytope.extract();
  constexpr auto via_lp_extract = lp_extract(polytope);
  constexpr auto via_maximize =
      maximize<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>();

  // Assert on the *expression* types (the actual API return types)
  // rather than the constexpr-local *variable* types --- the latter
  // pick up a top-level const that isn't part of the API surface.
  STATIC_CHECK(
      std::same_as<decltype(polytope.extract()), Vec2<Rat, Rat{2L}, Rat{2L}>>);
  STATIC_CHECK(std::same_as<decltype(lp_extract(polytope)),
                            Vec2<Rat, Rat{2L}, Rat{2L}>>);
  STATIC_CHECK(
      std::same_as<decltype(maximize<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>()),
                   Vec2<Rat, Rat{2L}, Rat{2L}>>);

  CHECK(via_extract_member == via_maximize);
  CHECK(via_lp_extract == via_maximize);
  CHECK(via_extract_member == via_lp_extract);
}

// The §5 polytope as a constexpr value-level array: shared between the
// NTTP-folding tests below and the bridge witness, so every test makes
// the single bridge entry visible — same call as the runtime path, just
// with constexpr inputs.
constexpr std::array<HalfspaceTriple<Rat>, 4> kPolytope = {{
    {Rat{1L}, Rat{1L}, Rat{4L}},   // H1:  x +  y ≤ 4
    {Rat{2L}, Rat{1L}, Rat{6L}},   // H2:  2x + y ≤ 6
    {Rat{-1L}, Rat{0L}, Rat{0L}},  // H3:  x      ≥ 0
    {Rat{0L}, Rat{-1L}, Rat{0L}},  // H4:       y ≥ 0
}};

TEST_CASE(
    "optimization:lp — paper-facing existential proof: "
    "maximize(3x + 2y, polytope) = Vec2<Rat, 2, 2> at compile time",
    "[optimization][lp][centrepiece]") {
  // The reduction returns an NTTP `Vec2<Rat, 2, 2>` — the optimum IS a type.
  using Optimum = decltype(maximize<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>());
  STATIC_CHECK(std::same_as<Optimum, Vec2<Rat, Rat{2L}, Rat{2L}>>);

  // Equivalent value-level view — both `first` and `second` are NTTPs.
  constexpr Optimum opt{};
  STATIC_CHECK(opt.first == Rat{2L});
  STATIC_CHECK(opt.second == Rat{2L});

  // The single bridge entry, called with constexpr inputs, folds to the
  // same constant and carries the objective value too.  Same function
  // template as the runtime path — see the [bridge] witness below.
  constexpr auto v = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kPolytope), Rat{3L}, Rat{2L});
  STATIC_CHECK(v.feasible);
  STATIC_CHECK(v.x == Rat{2L});
  STATIC_CHECK(v.y == Rat{2L});
  // Objective: 3·2 + 2·2 = 10.
  constexpr Rat obj = Rat{3L} * v.x + Rat{2L} * v.y;
  STATIC_CHECK(obj == Rat{10L});
}

TEST_CASE("optimization:lp — axis-aligned corner is pruned away",
          "[optimization][lp][centrepiece]") {
  // Sanity check: with the objective direction (3, 2), the candidate
  // (0, 4) has obj = 0 + 8 = 8; (3, 0) has obj = 9 + 0 = 9; (2, 2) wins
  // at obj = 10. The reduction correctly picks the non-axis-aligned
  // intersection, not the corner.
  constexpr auto v = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kPolytope), Rat{3L}, Rat{2L});
  STATIC_CHECK(!(v.x == Rat{0L} && v.y == Rat{4L}));  // not (0, 4)
  STATIC_CHECK(!(v.x == Rat{3L} && v.y == Rat{0L}));  // not (3, 0)
  STATIC_CHECK(!(v.x == Rat{0L} && v.y == Rat{0L}));  // not (0, 0)
}

/**
 * @brief Structural bridge: the §5 LP in the textbook frame
 *        ` F : ℚ×ℚ, U : F → ℚ, G ⊆ F, opt = argmax(G, U) `,
 *        with G as a structurally-typed meet of halfspaces.
 *
 *   F = ℚ × ℚ            -- ambient field, operationally `Vec2V<Rat>`.
 *   U : F → ℚ            -- `LinearFunctional` carrying (3, 2) at the
 *                           type level.
 *   G ⊆ F                -- `halfspace_set(H1{}) & halfspace_set(H2{})
 *                           & halfspace_set(H3{}) & halfspace_set(H4{})`.
 *                           G's *type* IS
 *                           `Polytope2DSet<Rat, H1, H2, H3, H4>`,
 *                           carrying the halfspace pack at the type
 *                           level.  The `&` operator here is the
 *                           structural meet defined in `:optimization` —
 *                           shape-compatible with the `:expressions`
 *                           Set DSL surface, NOT actually a Set DSL
 *                           participant (see lp__Structural_Slice in
 *                           lp.cppm and #747).
 *   opt = argmax(G, U)   -- combinator that extracts G's pack from the
 *                           type, U's coefficients from its NTTPs, and
 *                           dispatches to the kernel.  Returns the
 *                           typed constant `Vec2<Rat, Rat{2L}, Rat{2L}>`.
 *
 * The bridge this exhibit supports is the *structural* one: G's *type*
 * IS the polytope's combinatorial data, so `argmax` can dispatch on it
 * without re-extracting halfspaces from a closure body.  The stronger
 * §3 ↔ §5 claim — that the @c :expressions Set DSL infrastructure
 * (Lawvere comprehension over @c Set<T, L, P>) does the work — requires
 * lifting these carriers into actual @c Set<...> instances with a
 * structural @c Halfspace2DPredicate ; tracked in #747.
 */
TEST_CASE(
    "optimization:lp — structural bridge: F = ℚ×ℚ, U : F → ℚ, "
    "G = H1 & H2 & H3 & H4, opt = argmax(G, U) = Vec2<Rat, 2, 2> (#743)",
    "[optimization][lp][structural][bridge][centrepiece]") {
  // F = ℚ × ℚ.  Operationally `Vec2V<Rat>`.
  using F = dedekind::linear_algebra::Vec2V<Rat>;
  static_assert(F::dimension == 2);
  static_assert(std::same_as<typename F::scalar_type, Rat>);

  // U : F → ℚ.  Linear functional carrying the §5 objective (3, 2) at
  // the type level so `argmax` can route its coefficients into the
  // NTTP-driven kernel without smuggling them through function params.
  constexpr LinearFunctional<Rat, Rat{3L}, Rat{2L}> U{};

  // G ⊆ F.  Structurally-typed meet of four halfspaces: `decltype(G)`
  // IS `Polytope2DSet<Rat, H1, H2, H3, H4>`, carrying the pack at the
  // type level.  The `&` here is the one in `:optimization`, not
  // `:expressions::Set::operator&` — see lp__Structural_Slice for the
  // honest read on what is and is not Set DSL participation.
  constexpr auto G = halfspace_set(H1{}) & halfspace_set(H2{}) &
                     halfspace_set(H3{}) & halfspace_set(H4{});
  static_assert(
      std::same_as<decltype(G), const Polytope2DSet<Rat, H1, H2, H3, H4>>);

  // argmax(G, U).  The combinator extracts the halfspace pack from G's
  // type, the (cx, cy) from U's NTTPs, dispatches to `maximize`, and
  // returns the optimum as the typed constant.
  constexpr auto opt = argmax(G, U);
  static_assert(std::same_as<decltype(opt), const Vec2<Rat, Rat{2L}, Rat{2L}>>);

  // The witnesses: the kernel's answer lies in G (pointwise `.contains`
  // on the structurally-typed carrier), and U evaluates to the textbook
  // value 10 on it.
  constexpr F opt_v{opt.first, opt.second};
  STATIC_CHECK(G.contains(opt_v));
  STATIC_CHECK(U(opt_v) == Rat{10L});

  // Negative witnesses on the structurally-typed carrier.
  STATIC_CHECK_FALSE(G.contains(F{Rat{3L}, Rat{3L}}));   // x + y = 6 > 4
  STATIC_CHECK_FALSE(G.contains(F{Rat{-1L}, Rat{0L}}));  // x < 0
}

TEST_CASE("optimization:lp — infeasible polytope reports no optimum",
          "[optimization][lp][infeasible]") {
  // Intersect x ≤ 1 with x ≥ 3 (i.e. -x ≤ -3): infeasible — no (x, y) is
  // in both halfspaces.  The bridge entry, called with constexpr inputs,
  // reports `!feasible` rather than returning a bogus vertex.  Note: the
  // NTTP packaging `maximize<...>()` would fire a static_assert at
  // instantiation; here we observe the flag directly.
  constexpr std::array<HalfspaceTriple<Rat>, 3> infeasible = {{
      {Rat{1L}, Rat{0L}, Rat{1L}},    //  x ≤ 1
      {Rat{-1L}, Rat{0L}, Rat{-3L}},  // -x ≤ -3 (i.e. x ≥ 3)
      {Rat{0L}, Rat{1L}, Rat{5L}},    //  y ≤ 5
  }};
  constexpr auto v = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(infeasible), Rat{1L}, Rat{1L});
  STATIC_CHECK_FALSE(v.feasible);
}

/**
 * Parametric LP over `Dual<ℚ>`: compile-time sensitivity analysis.
 *
 * Framing (after Elliott, *The Simple Essence of Automatic
 * Differentiation*, ICFP 2018): forward-mode AD is the product `(f, f')`
 * with chain-rule composition, not the "dual number" object per se.
 * `Dual<F>` is one realization of that product; `ftc::derivative_at` is
 * another (numerical, central difference); `RigPolynomial::derive()` is
 * a third (exact formal, polynomial coefficient vectors). Here we use
 * `Dual<Rat>` because we need the product available at compile time
 * inside an NTTP context — `Dual<F>` became structural in the same PR
 * that adds this test.
 *
 * The LP reduction is generic over any `HasRingOperators` carrier with a
 * total order on the primal part; `Dual<Rat>` provides both (primal is
 * `Rat`, ordered; tangent rides along through the chain rule). Running
 * the same reduction over `Dual<Rat>` instead of plain `Rat` gives us
 * the optimum AND its sensitivity to a perturbed parameter, packaged as
 * the NTTP `Vec2<Dual<Rat>, x*, y*>` — no separate derivative pass.
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
  STATIC_CHECK(v.feasible);

  // x* = 2 - ε  →  primal 2, tangent -1.
  STATIC_CHECK(v.x.val == Rat{2L});
  STATIC_CHECK(v.x.der == Rat{-1L});

  // y* = 2 + 2ε → primal 2, tangent +2.
  STATIC_CHECK(v.y.val == Rat{2L});
  STATIC_CHECK(v.y.der == Rat{2L});
}

/**
 * @brief Runtime-coefficient entry point: the paper's bridge in the
 *        opposite direction.  Same polytope as the NTTP centrepiece,
 *        but the constraints arrive as values rather than types — the
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
  STATIC_CHECK(v.feasible);
  STATIC_CHECK(v.x == Rat{2L});
  STATIC_CHECK(v.y == Rat{2L});

  // Parity with the NTTP packaging surface on the same polytope: the
  // bridge entry called with constexpr inputs and the NTTP-lifted
  // `Vec2<T, x*, y*>` must agree, because the latter is the former
  // wrapped in a type-level packaging step.
  using NttpLifted =
      decltype(maximize<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>());
  STATIC_CHECK(std::same_as<NttpLifted, Vec2<Rat, Rat{2L}, Rat{2L}>>);
  constexpr NttpLifted lifted{};
  STATIC_CHECK(v.x == lifted.first);
  STATIC_CHECK(v.y == lifted.second);
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

  CHECK(result.feasible);
  CHECK(result.x == Rat{2L});
  CHECK(result.y == Rat{2L});

  // Parity with the NTTP packaging surface: the same polytope reduced
  // through both surfaces must yield identical coordinates.
  using NttpLifted =
      decltype(maximize<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>());
  constexpr NttpLifted lifted{};
  CHECK(result.x == lifted.first);
  CHECK(result.y == lifted.second);
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

  CHECK(result.feasible);
  CHECK(result.x == Rat{2L});
  CHECK(result.y == Rat{2L});
  CHECK(Rat{1L} * result.x + Rat{1L} * result.y == Rat{4L});

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
  CHECK(shrunk_result.feasible);
  // Active set {x + 2y = 3, 2x + y = 6}: solve gives x = 3, y = 0.
  CHECK(shrunk_result.x == Rat{3L});
  CHECK(shrunk_result.y == Rat{0L});
}

TEST_CASE(
    "optimization:lp — runtime entry point reports infeasibility "
    "without throwing (#743)",
    "[optimization][lp][runtime][infeasible]") {
  // x ≤ 1 ∧ x ≥ 3: empty feasible region.  The runtime path must report
  // @c !feasible rather than the NTTP path's static_assert failure —
  // Python callers need to inspect the flag at run time.
  std::vector<HalfspaceTriple<Rat>> halfspaces{
      {Rat{1L}, Rat{0L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{-3L}},
      {Rat{0L}, Rat{1L}, Rat{5L}},
  };
  const auto result = maximize_with_values<Rat>(halfspaces, Rat{1L}, Rat{1L});
  CHECK_FALSE(result.feasible);
}
