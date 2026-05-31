/** @file dedekind/optimization/lp_test.cpp
 *
 * Unit coverage for the `:lp` partition of `dedekind.optimization`.
 * The paper-facing existential proof is `maximize<3, 2, H1, H2, H3, H4>()`
 * reducing to `Vec2<Rat, 2, 2>` at compile time, with a non-axis-aligned
 * active set `{H1, H2}` solved via `Invertible2x2`.
 */

#include <array>
#include <catch2/catch_test_macros.hpp>
#include <limits>
#include <span>
#include <vector>

import dedekind.analysis; // Dual<F> (relocated from :numbers at PR #513)
import dedekind.category; // ClassicalLogic — Set's logic species (#747)
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

TEST_CASE(
    "optimization:lp — output-side Set predicates (#749 Set-out alignment)",
    "[optimization][lp][set-out]") {
  // Building blocks for the Set-as-output design: the LP's optimal
  // locus is a Set whose predicate type encodes the regime.

  // Singleton2DPredicate: NTTP-coordinated singleton predicate.
  using Sing = Singleton2DPredicate<Rat, Rat{2L}, Rat{2L}>;
  STATIC_CHECK(Sing::coord_x == Rat{2L});
  STATIC_CHECK(Sing::coord_y == Rat{2L});
  constexpr Sing pred{};
  STATIC_CHECK(pred(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
  STATIC_CHECK_FALSE(pred(Vec2V<Rat>{Rat{2L}, Rat{3L}}));
  STATIC_CHECK_FALSE(pred(Vec2V<Rat>{Rat{0L}, Rat{0L}}));

  // lp_singleton_set: lifts NTTP (x*, y*) into a Set whose predicate
  // carries the singleton structurally.  The output Set's predicate
  // type pins the regime at the type level.
  constexpr auto sing_set = lp_singleton_set<Rat, Rat{2L}, Rat{2L}>();
  CHECK(sing_set(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
  CHECK_FALSE(sing_set(Vec2V<Rat>{Rat{3L}, Rat{3L}}));

  // lp_empty_set: lifts the empty optimum (infeasible LP) into a Set
  // whose predicate is the existing :expressions::EmptyPredicate.
  constexpr auto empty = lp_empty_set<Rat>();
  CHECK_FALSE(empty(Vec2V<Rat>{Rat{0L}, Rat{0L}}));
  CHECK_FALSE(empty(Vec2V<Rat>{Rat{2L}, Rat{2L}}));
}

TEST_CASE("optimization:lp — HasUnitRangeEntries structural classifier (#749)",
          "[optimization][lp][triptych][signed-unimodular]") {
  // The unit-square pack: maximize x + y s.t. 0 ≤ x ≤ 1, 0 ≤ y ≤ 1.
  // All entries in {−1, 0, +1}: classifier holds.
  using U1 = Halfspace2D<Rat, Rat{1L}, Rat{0L}, Rat{1L}>;   //  x ≤ 1
  using U2 = Halfspace2D<Rat, Rat{0L}, Rat{1L}, Rat{1L}>;   //  y ≤ 1
  using U3 = Halfspace2D<Rat, Rat{-1L}, Rat{0L}, Rat{0L}>;  // -x ≤ 0
  using U4 = Halfspace2D<Rat, Rat{0L}, Rat{-1L}, Rat{0L}>;  // -y ≤ 0
  STATIC_CHECK(HasUnitRangeEntries<U1, U2, U3, U4>);

  // The §5 polytope: H2 has coeff_x = 2, so the classifier fails.
  STATIC_CHECK_FALSE(HasUnitRangeEntries<H1, H2, H3, H4>);

  // Dropping the offending halfspace recovers the classifier.
  STATIC_CHECK(HasUnitRangeEntries<H1, H3, H4>);
}

TEST_CASE(
    "optimization:lp — IsAxisAlignedPolytope structural classifier (#749)",
    "[optimization][lp][triptych][axis-aligned]") {
  // Unit-square pack: every halfspace constrains exactly one axis.
  using U1 = Halfspace2D<Rat, Rat{1L}, Rat{0L}, Rat{1L}>;
  using U2 = Halfspace2D<Rat, Rat{0L}, Rat{1L}, Rat{1L}>;
  using U3 = Halfspace2D<Rat, Rat{-1L}, Rat{0L}, Rat{0L}>;
  using U4 = Halfspace2D<Rat, Rat{0L}, Rat{-1L}, Rat{0L}>;
  STATIC_CHECK(IsAxisAlignedPolytope<U1, U2, U3, U4>);

  // The §5 polytope: H1 = (1, 1, 4) is not axis-aligned (both coeffs nonzero).
  STATIC_CHECK_FALSE(IsAxisAlignedPolytope<H1, H2, H3, H4>);
}

TEST_CASE("optimization:lp — bit-ops fast-path triptych dispatch (#749)",
          "[optimization][lp][triptych][fast-path]") {
  // Unit-square pack: signed-unimodular AND axis-aligned, so the NTTP
  // entry @ref maximize dispatches to the bit-ops fast path.
  using U1 = Halfspace2D<Rat, Rat{1L}, Rat{0L}, Rat{1L}>;
  using U2 = Halfspace2D<Rat, Rat{0L}, Rat{1L}, Rat{1L}>;
  using U3 = Halfspace2D<Rat, Rat{-1L}, Rat{0L}, Rat{0L}>;
  using U4 = Halfspace2D<Rat, Rat{0L}, Rat{-1L}, Rat{0L}>;

  // Compile-time fold: maximize x + y over the unit square → (1, 1).
  using Opt = decltype(maximize<Rat, Rat{1L}, Rat{1L}, U1, U2, U3, U4>());
  STATIC_CHECK(std::same_as<Opt, Vec2<Rat, Rat{1L}, Rat{1L}>>);

  // Runtime entry (bridge witness for the fast-path kernel): same answer.
  constexpr std::array<HalfspaceTriple<Rat>, 4> kUnitSquare = {{
      {Rat{1L}, Rat{0L}, Rat{1L}},
      {Rat{0L}, Rat{1L}, Rat{1L}},
      {Rat{-1L}, Rat{0L}, Rat{0L}},
      {Rat{0L}, Rat{-1L}, Rat{0L}},
  }};
  constexpr auto v_pp = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kUnitSquare), Rat{1L}, Rat{1L});
  STATIC_CHECK(v_pp.feasible);
  STATIC_CHECK(v_pp.x == Rat{1L});
  STATIC_CHECK(v_pp.y == Rat{1L});

  // Negative-x objective: the fast path picks x_lo via sign selection.
  constexpr auto v_np = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kUnitSquare), Rat{-1L}, Rat{1L});
  STATIC_CHECK(v_np.feasible);
  STATIC_CHECK(v_np.x == Rat{0L});
  STATIC_CHECK(v_np.y == Rat{1L});
}

TEST_CASE(
    "optimization:lp — triptych: one NTTP entry routes three regimes (#749)",
    "[optimization][lp][triptych][routing]") {
  // The §5 abstract claim: one parametric template @c maximize<T,cx,cy,Hs...>
  // routes to three qualitatively distinct compile-time outcomes across
  // families of inputs.  This test pins the routing mechanically.
  //
  //   regime  | pack                       | dispatch branch
  //   --------+----------------------------+----------------
  //   fast    | unit square (U1..U4)       | bit-ops fast path
  //   generic | §5 polytope (H1..H4)       | Cramer fold
  //   refusal | empty x-interval           | static_assert at @ref maximize
  //                                          (the .fail.cpp form is deferred;
  //                                          the runtime collapse is the
  //                                          [refusal]-tagged TEST_CASE below)
  using U1 = Halfspace2D<Rat, Rat{1L}, Rat{0L}, Rat{1L}>;
  using U2 = Halfspace2D<Rat, Rat{0L}, Rat{1L}, Rat{1L}>;
  using U3 = Halfspace2D<Rat, Rat{-1L}, Rat{0L}, Rat{0L}>;
  using U4 = Halfspace2D<Rat, Rat{0L}, Rat{-1L}, Rat{0L}>;

  // Fast-path regime: NTTP entry dispatches via @c if @c constexpr to the
  // axis-aligned bit-ops kernel.  Same @c maximize template, same call
  // site shape; the only thing that changes is the halfspace pack.
  using FastOpt = decltype(maximize<Rat, Rat{1L}, Rat{1L}, U1, U2, U3, U4>());
  STATIC_CHECK(std::same_as<FastOpt, Vec2<Rat, Rat{1L}, Rat{1L}>>);

  // Generic regime: same NTTP entry, different pack — Cramer fold.
  using GenericOpt =
      decltype(maximize<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>());
  STATIC_CHECK(std::same_as<GenericOpt, Vec2<Rat, Rat{2L}, Rat{2L}>>);

  // Routing witnesses: the dispatch gate evaluates differently for the
  // two packs, which is what causes the two regimes to fire.
  STATIC_CHECK(HasUnitRangeEntries<U1, U2, U3, U4> &&
               IsAxisAlignedPolytope<U1, U2, U3, U4>);
  STATIC_CHECK_FALSE(HasUnitRangeEntries<H1, H2, H3, H4> &&
                     IsAxisAlignedPolytope<H1, H2, H3, H4>);
}

TEST_CASE(
    "optimization:lp — triptych refusal: infeasible axis-aligned pack (#749)",
    "[optimization][lp][triptych][refusal]") {
  // Empty feasible region: x ≤ 0 ∧ x ≥ 1 ∧ 0 ≤ y ≤ 1.  The NTTP entry
  // @ref maximize on this pack would fire @c static_assert on the fast
  // path; the runtime entry returns @c feasible == false, which is the
  // same refusal signal collapsed to a runtime predicate.  A negative-
  // compile exhibit (@c .fail.cpp ) for the @c static_assert path is
  // deferred to a follow-up (needs CMake @c try_compile infra).
  constexpr std::array<HalfspaceTriple<Rat>, 4> kEmptyXInterval = {{
      {Rat{1L}, Rat{0L}, Rat{0L}},    //  x ≤ 0
      {Rat{-1L}, Rat{0L}, Rat{-1L}},  // -x ≤ -1, i.e. x ≥ 1
      {Rat{0L}, Rat{1L}, Rat{1L}},    //  y ≤ 1
      {Rat{0L}, Rat{-1L}, Rat{0L}},   // -y ≤ 0, i.e. y ≥ 0
  }};
  constexpr auto v = maximize_axis_aligned_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kEmptyXInterval), Rat{1L}, Rat{1L});
  STATIC_CHECK_FALSE(v.feasible);
}

TEST_CASE(
    "optimization:lp — fast-path defensive validation on runtime entry (#749)",
    "[optimization][lp][triptych][refusal]") {
  // Two precondition violations the runtime entry must collapse to the
  // infeasible sentinel rather than silently mis-solve (the NTTP entry
  // gates these statically via @c HasUnitRangeEntries and
  // @c IsAxisAlignedPolytope ; the runtime entry is reachable directly
  // and the kernel defends Honest Rejection).

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
  STATIC_CHECK_FALSE(v_zero.feasible);

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
  STATIC_CHECK_FALSE(v_out.feasible);

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
  STATIC_CHECK_FALSE(v_diag.feasible);
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
  // without `numeric_limits` specialisation (Rational, Dual) bypass
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
  STATIC_CHECK_FALSE(v.feasible);
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
  STATIC_CHECK(via_auto.feasible);
  STATIC_CHECK(via_auto.x == Rat{1L});
  STATIC_CHECK(via_auto.y == Rat{1L});

  // §5 polytope (H2's coeff_x = 2): not fast-eligible.  Cramer routes
  // and computes (2, 2) via the active-set enumeration.
  constexpr auto via_generic = maximize_with_values<Rat>(
      std::span<const HalfspaceTriple<Rat>>(kPolytope), Rat{3L}, Rat{2L});
  STATIC_CHECK(via_generic.feasible);
  STATIC_CHECK(via_generic.x == Rat{2L});
  STATIC_CHECK(via_generic.y == Rat{2L});
}

/**
 * @brief Set DSL bridge (§3 ↔ §5): the §5 LP in the textbook frame
 *        ` F : ℚ×ℚ, U : F → ℚ, G ⊆ F, opt = argmax(G, U) `,
 *        with G a real `:expressions::Set` whose predicate carries the
 *        halfspace pack at the type level (#747).
 *
 *   F = ℚ × ℚ            -- ambient field, operationally `Vec2V<Rat>`.
 *   U : F → ℚ            -- `LinearFunctional` carrying (3, 2) at the
 *                           type level.
 *   G ⊆ F                -- the meet `halfspace_set(H1{}) & ... &
 *                           halfspace_set(H4{})`, where `halfspace_set`
 *                           lifts each NTTP halfspace into a Set
 *                           `Set<Vec2V<Rat>, ClassicalLogic,
 *                            Halfspace2DPredicate<Rat, ...>>` and `&`
 *                           routes through `:expressions::Set::operator&` →
 *                           `structured_and` (ADL on our overloads in
 *                           `:optimization`) → a Set whose predicate IS
 *                           `Polytope2DPredicate<Rat, H1, H2, H3, H4>`.
 *                           Closes FIXME(#365) for the 2D-halfspace case:
 *                           the meet is a structural reduction, not a
 *                           lambda fall-through.
 *   opt = argmax(G, U)   -- combinator that extracts the halfspace pack
 *                           from G's `Polytope2DPredicate` and U's
 *                           NTTPs, dispatches to `maximize`, returns the
 *                           typed constant `Vec2<Rat, Rat{2L}, Rat{2L}>`.
 *
 * Every step in the textbook frame is realised through a real Set DSL
 * object: G is a `:expressions::Set` instance, the meet goes through
 * `Set::operator&`, the predicate carries the structure.  The §3
 * comprehension View does the work for §5; the kernel itself is
 * untouched.
 */
TEST_CASE(
    "optimization:lp — Set DSL bridge: G = H1 & H2 & H3 & H4 as "
    "Set<Vec2V<Rat>, ClassicalLogic, Polytope2DPredicate<...>>, "
    "opt = argmax(G, U) = Set with Singleton2DPredicate<Rat, 2, 2> (#747)",
    "[optimization][lp][dsl][bridge][centrepiece]") {
  using F = dedekind::linear_algebra::Vec2V<Rat>;
  static_assert(F::dimension == 2);
  static_assert(std::same_as<typename F::scalar_type, Rat>);

  // U : F → ℚ.
  constexpr LinearFunctional<Rat, Rat{3L}, Rat{2L}> U{};

  // G ⊆ F.  The meet of four halfspace-Sets goes through
  // `:expressions::Set::operator&`, which discovers our
  // `structured_and` overloads via ADL on `Halfspace2DPredicate` /
  // `Polytope2DPredicate` and reduces structurally.
  constexpr auto G = halfspace_set(H1{}) & halfspace_set(H2{}) &
                     halfspace_set(H3{}) & halfspace_set(H4{});

  // `decltype(G)` IS a real `:expressions::Set` instance — the §3 DSL —
  // whose predicate carries the halfspace pack at the type level.
  using ExpectedG =
      dedekind::sets::Set<F, dedekind::category::ClassicalLogic,
                          Polytope2DPredicate<Rat, H1, H2, H3, H4>>;
  static_assert(std::same_as<decltype(G), const ExpectedG>);

  // argmax(G, U).  Set in, Set out.  The output Set's predicate type
  // pins the regime (Singleton) AND the coordinates ((2, 2)) at the
  // type level — input polytope and output locus carried in the same
  // DSL vocabulary.
  constexpr auto opt = argmax(G, U);
  using ExpectedOpt =
      dedekind::sets::Set<F, dedekind::category::ClassicalLogic,
                          Singleton2DPredicate<Rat, Rat{2L}, Rat{2L}>>;
  static_assert(std::same_as<decltype(opt), const ExpectedOpt>);

  // The witnesses: opt ∈ G via the Set DSL's `contains`; opt itself
  // singles out (2, 2); U(2, 2) is the textbook value 10.  Everything
  // decided at translation time.
  constexpr F opt_v{Rat{2L}, Rat{2L}};
  STATIC_CHECK(opt(opt_v));
  STATIC_CHECK(G.contains(opt_v));
  STATIC_CHECK(U(opt_v) == Rat{10L});

  // Negative witnesses through the Set DSL contract.
  STATIC_CHECK_FALSE(G.contains(F{Rat{3L}, Rat{3L}}));   // x + y = 6 > 4
  STATIC_CHECK_FALSE(G.contains(F{Rat{-1L}, Rat{0L}}));  // x < 0

  // Bracketing invariance: `(H1 & H2) & (H3 & H4)` (polytope × polytope)
  // must collapse to the same `Polytope2DPredicate` as the left-folded
  // form above.  Without the polytope × polytope `structured_and`
  // overload, the Set DSL would fall through to a lambda meet here and
  // `decltype(G2)` would not match — so the static_assert is the
  // mechanical guard against that regression.
  constexpr auto G2 = (halfspace_set(H1{}) & halfspace_set(H2{})) &
                      (halfspace_set(H3{}) & halfspace_set(H4{}));
  static_assert(std::same_as<decltype(G2), decltype(G)>);
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
