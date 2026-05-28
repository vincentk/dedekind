"""Python coverage for the runtime-coefficient LP binding (#743).

The compile-time LP showcase pins the optimum of a 2D polytope as an NTTP-
returned typed constant.  ``dedekind.maximize_lp`` is the companion runtime
entry point: same active-set kernel, polytope coefficients supplied as
Python values.  These tests guard parity on the paper's locked polytope and
exercise the kernel on a second instance so the runtime path cannot be
accidentally constant-folded by coincidence.
"""

import unittest

import dedekind


class DedekindMaximizeLpTest(unittest.TestCase):
    """Smoke tests for ``dedekind.maximize_lp`` (#743)."""

    def test_paper_polytope_matches_compile_time_centerpiece(self) -> None:
        # The §5 paper-facing instance:
        #   maximize 3x + 2y
        #   s.t.   x +  y <= 4   (H1)
        #          2x + y <= 6   (H2)
        #         -x      <= 0   (H3:  x >= 0)
        #               -y <= 0  (H4:  y >= 0)
        # NTTP path: Vec2<Rat, 2, 2>; objective value 10.
        x, y, feasible = dedekind.maximize_lp(
            (3.0, 2.0),
            [
                (1.0, 1.0, 4.0),
                (2.0, 1.0, 6.0),
                (-1.0, 0.0, 0.0),
                (0.0, -1.0, 0.0),
            ],
        )
        self.assertTrue(feasible)
        self.assertEqual(x, 2.0)
        self.assertEqual(y, 2.0)
        self.assertEqual(3.0 * x + 2.0 * y, 10.0)

    def test_second_polytope_distinct_optimum(self) -> None:
        # A different polytope and different objective: max x + y over
        # the triangle {x >= 0, y >= 0, x + 2y <= 6, 2x + y <= 6}.
        # Active set {x + 2y = 6, 2x + y = 6} gives x = y = 2,
        # objective = 4.  Distinct from the centrepiece's objective
        # value (10), so a coincidental constant-fold to (2, 2) would
        # still mis-report the objective.
        x, y, feasible = dedekind.maximize_lp(
            (1.0, 1.0),
            [
                (1.0, 2.0, 6.0),
                (2.0, 1.0, 6.0),
                (-1.0, 0.0, 0.0),
                (0.0, -1.0, 0.0),
            ],
        )
        self.assertTrue(feasible)
        self.assertEqual(x, 2.0)
        self.assertEqual(y, 2.0)
        self.assertEqual(x + y, 4.0)

    def test_shrunk_polytope_moves_vertex(self) -> None:
        # Shrink the first halfspace's bound so the optimum vertex
        # moves: active set {x + 2y = 3, 2x + y = 6} gives x = 3, y = 0.
        x, y, feasible = dedekind.maximize_lp(
            (1.0, 1.0),
            [
                (1.0, 2.0, 3.0),
                (2.0, 1.0, 6.0),
                (-1.0, 0.0, 0.0),
                (0.0, -1.0, 0.0),
            ],
        )
        self.assertTrue(feasible)
        self.assertEqual(x, 3.0)
        self.assertEqual(y, 0.0)

    def test_dual_sensitivity_perturbed_h1_bound(self) -> None:
        # Across the bridge on a Dual<double> carrier: the same active-set
        # kernel returns primal AND first-order sensitivity.  Perturbing
        # H1's bound by ε moves the active set {H1', H2} optimum to
        #   x* = 2 - ε     (primal 2, tangent -1)
        #   y* = 2 + 2ε    (primal 2, tangent +2)
        # — the chain rule has already run inside the Cramer solve.
        D = dedekind.Dual
        x, y, feasible = dedekind.maximize_lp_dual(
            (D(3.0, 0.0), D(2.0, 0.0)),
            [
                (D(1.0, 0.0), D(1.0, 0.0), D(4.0, 1.0)),    # H1 bound + ε
                (D(2.0, 0.0), D(1.0, 0.0), D(6.0, 0.0)),    # H2
                (D(-1.0, 0.0), D(0.0, 0.0), D(0.0, 0.0)),   # H3
                (D(0.0, 0.0), D(-1.0, 0.0), D(0.0, 0.0)),   # H4
            ],
        )
        self.assertTrue(feasible)
        self.assertEqual(x.value(), 2.0)
        self.assertEqual(x.derivative(), -1.0)
        self.assertEqual(y.value(), 2.0)
        self.assertEqual(y.derivative(), 2.0)

    def test_dual_no_perturbation_matches_primal_path(self) -> None:
        # With all tangents zero, the dual path must agree with the plain
        # double path on the primal — the Dual<double> instantiation
        # subsumes the double instantiation as ε → 0.
        D = dedekind.Dual
        x_dual, y_dual, feasible = dedekind.maximize_lp_dual(
            (D(3.0, 0.0), D(2.0, 0.0)),
            [
                (D(1.0, 0.0), D(1.0, 0.0), D(4.0, 0.0)),
                (D(2.0, 0.0), D(1.0, 0.0), D(6.0, 0.0)),
                (D(-1.0, 0.0), D(0.0, 0.0), D(0.0, 0.0)),
                (D(0.0, 0.0), D(-1.0, 0.0), D(0.0, 0.0)),
            ],
        )
        x_plain, y_plain, _ = dedekind.maximize_lp(
            (3.0, 2.0),
            [(1.0, 1.0, 4.0), (2.0, 1.0, 6.0),
             (-1.0, 0.0, 0.0), (0.0, -1.0, 0.0)],
        )
        self.assertTrue(feasible)
        self.assertEqual(x_dual.value(), x_plain)
        self.assertEqual(y_dual.value(), y_plain)
        self.assertEqual(x_dual.derivative(), 0.0)
        self.assertEqual(y_dual.derivative(), 0.0)

    def test_dual_rational_sensitivity_matches_compile_time_carrier(self) -> None:
        # Across the bridge on the SAME carrier the compile-time NTTP
        # exhibit uses — Dual<Rational<SignedExtensionalCardinal<>>>.
        # The Python call instantiates `detail::maximize_impl` at the
        # identical template specialisation as the C++ [dual][sensitivity]
        # test: maximum carrier-overlap, ℚ-exact primal AND tangent.
        D = dedekind.DualRational
        R = dedekind.Rational
        x, y, feasible = dedekind.maximize_lp_dual_rational(
            (D(3, 0), D(2, 0)),
            [
                (D(1, 0), D(1, 0), D(R(4), R(1))),  # H1' bound + ε
                (D(2, 0), D(1, 0), D(6, 0)),        # H2
                (D(-1, 0), D(0, 0), D(0, 0)),       # H3
                (D(0, 0), D(-1, 0), D(0, 0)),       # H4
            ],
        )
        self.assertTrue(feasible)
        # x* = 2 - ε in exact ℚ.
        self.assertEqual(x.value(), R(2))
        self.assertEqual(x.derivative(), R(-1))
        # y* = 2 + 2ε in exact ℚ.
        self.assertEqual(y.value(), R(2))
        self.assertEqual(y.derivative(), R(2))
        # Numerator/denominator pinning the rational normal form.
        self.assertEqual((x.value().num(), x.value().den()), (2, 1))
        self.assertEqual((x.derivative().num(), x.derivative().den()), (-1, 1))

    def test_rational_arithmetic_normalises(self) -> None:
        # The Rational binding round-trips through the project's exact
        # ℚ carrier — denominators simplify, fractions equal.
        R = dedekind.Rational
        self.assertEqual(R(4, 2), R(2))
        self.assertEqual(R(3, 7) + R(2, 7), R(5, 7))
        self.assertEqual(R(1, 2) * R(2, 3), R(1, 3))
        # Numerator/denominator are accessible as Python ints.
        self.assertEqual(R(7, 3).num(), 7)
        self.assertEqual(R(7, 3).den(), 3)

    def test_infeasible_polytope_reports_flag(self) -> None:
        # x <= 1 and x >= 3 cannot both hold; the runtime entry point
        # reports the feasibility flag (rather than the NTTP path's
        # static_assert failure) so Python callers can branch on it.
        _, _, feasible = dedekind.maximize_lp(
            (1.0, 1.0),
            [
                (1.0, 0.0, 1.0),
                (-1.0, 0.0, -3.0),
                (0.0, 1.0, 5.0),
            ],
        )
        self.assertFalse(feasible)


if __name__ == "__main__":
    unittest.main()
