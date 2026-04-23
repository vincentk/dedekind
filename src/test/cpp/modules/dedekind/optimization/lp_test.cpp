/** @file dedekind/optimization/lp_test.cpp
 *
 * Unit coverage for the `:lp` partition of `dedekind.optimization`.
 * The paper-facing existential proof is `maximize<3, 2, H1, H2, H3, H4>()`
 * reducing to `Vec2<Rat, 2, 2>` at compile time, with a non-axis-aligned
 * active set `{H1, H2}` solved via `Invertible2x2`.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.optimization;

using namespace dedekind::linear_algebra;
using namespace dedekind::numbers;
using namespace dedekind::optimization;

namespace {
using Rat = Rational<long>;

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

  // The value-level reduction carries the objective value too.
  constexpr auto v = maximize_value<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>();
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
  constexpr auto v = maximize_value<Rat, Rat{3L}, Rat{2L}, H1, H2, H3, H4>();
  STATIC_CHECK(!(v.x == Rat{0L} && v.y == Rat{4L}));  // not (0, 4)
  STATIC_CHECK(!(v.x == Rat{3L} && v.y == Rat{0L}));  // not (3, 0)
  STATIC_CHECK(!(v.x == Rat{0L} && v.y == Rat{0L}));  // not (0, 0)
}

TEST_CASE("optimization:lp — infeasible polytope reports no optimum",
          "[optimization][lp][infeasible]") {
  // Intersect x ≤ 1 with x ≥ 3 (i.e. -x ≤ -3): infeasible — no (x, y) is
  // in both halfspaces. The value-level reduction reports `!feasible`
  // rather than returning a bogus vertex.
  using InfX1 = Halfspace2D<Rat, Rat{1L}, Rat{0L}, Rat{1L}>;    //  x ≤ 1
  using InfX2 = Halfspace2D<Rat, Rat{-1L}, Rat{0L}, Rat{-3L}>;  // -x ≤ -3
  using InfY = Halfspace2D<Rat, Rat{0L}, Rat{1L}, Rat{5L}>;     //  y ≤ 5

  constexpr auto v =
      maximize_value<Rat, Rat{1L}, Rat{1L}, InfX1, InfX2, InfY>();
  STATIC_CHECK_FALSE(v.feasible);
  // Note: the NTTP `maximize<...>()` form would fire a static_assert at
  // instantiation; we exercise the value-level `maximize_value<...>` here
  // so the test can observe the infeasibility flag rather than failing
  // to compile.
}

// The Dual<ℚ> parametric-LP showcase (compile-time sensitivity analysis)
// is deferred until Dual<F> is made structural (NTTP-compatible) in a
// follow-on commit. The concept is clean — "ℚ → 𝔻(ℚ)" layering makes the
// optimum's sensitivity to a perturbed bound fall out as the dual part —
// but `Dual<F>` currently has private members and can't serve as `T` in
// `Halfspace2D<T, a, b, c>`. Surgical upstream change to come.
