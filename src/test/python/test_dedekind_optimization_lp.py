"""Python coverage for the runtime-coefficient LP binding (#743).

The compile-time LP showcase pins the optimum of the §5 polytope as an
NTTP-returned typed constant ``Vec2<Rat, 2, 2>``.  ``dedekind.maximize_lp``
is the runtime call site of the same ``constexpr`` kernel: the polytope's
coefficients arrive as Python values; the carrier is ``Dual<Rational>``
so the result carries primal and first-order sensitivity, ℚ-exact in
both components.  Same template specialisation the C++
``[dual][sensitivity]`` test exercises.
"""

import unittest

import dedekind


class DedekindMaximizeLpTest(unittest.TestCase):
    """Smoke tests for ``dedekind.maximize_lp`` on ``Dual<Rational>`` (#743)."""

    def test_paper_polytope_perturbed_h1_bound(self) -> None:
        # §5 polytope with H1's bound perturbed by ε:
        #   maximize 3x + 2y
        #   s.t.   x +  y <= 4 + ε   (H1')
        #          2x + y <= 6        (H2)
        #         -x      <= 0        (H3:  x >= 0)
        #               -y <= 0       (H4:  y >= 0)
        # Active set at the optimum: {H1', H2}.
        #   x* = 2 - ε   (primal 2, tangent -1)
        #   y* = 2 + 2ε  (primal 2, tangent +2)
        # The chain rule has already run inside the Cramer solve.
        D = dedekind.DualRational
        R = dedekind.Rational
        x, y, feasible = dedekind.maximize_lp(
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
        # Numerator / denominator pinning the rational normal form.
        self.assertEqual((x.value().num(), x.value().den()), (2, 1))
        self.assertEqual((x.derivative().num(), x.derivative().den()), (-1, 1))

    def test_unperturbed_polytope_matches_compile_time_centerpiece(self) -> None:
        # All tangents zero — the kernel collapses to the same vertex as
        # the §5 NTTP exhibit (no sensitivity carried).
        D = dedekind.DualRational
        R = dedekind.Rational
        x, y, feasible = dedekind.maximize_lp(
            (D(3, 0), D(2, 0)),
            [
                (D(1, 0), D(1, 0), D(4, 0)),
                (D(2, 0), D(1, 0), D(6, 0)),
                (D(-1, 0), D(0, 0), D(0, 0)),
                (D(0, 0), D(-1, 0), D(0, 0)),
            ],
        )
        self.assertTrue(feasible)
        self.assertEqual(x.value(), R(2))
        self.assertEqual(y.value(), R(2))
        self.assertEqual(x.derivative(), R(0))
        self.assertEqual(y.derivative(), R(0))

    def test_infeasible_polytope_reports_flag(self) -> None:
        # x <= 1 and x >= 3 cannot both hold; the runtime entry point
        # reports the feasibility flag rather than the NTTP path's
        # ``static_assert`` failure, so Python callers can branch on it.
        D = dedekind.DualRational
        _, _, feasible = dedekind.maximize_lp(
            (D(1, 0), D(1, 0)),
            [
                (D(1, 0), D(0, 0), D(1, 0)),
                (D(-1, 0), D(0, 0), D(-3, 0)),
                (D(0, 0), D(1, 0), D(5, 0)),
            ],
        )
        self.assertFalse(feasible)

    def test_rational_arithmetic_normalises(self) -> None:
        # The Rational binding round-trips through the project's exact
        # ℚ carrier — denominators simplify, fractions equal.
        R = dedekind.Rational
        self.assertEqual(R(4, 2), R(2))
        self.assertEqual(R(3, 7) + R(2, 7), R(5, 7))
        self.assertEqual(R(1, 2) * R(2, 3), R(1, 3))
        self.assertEqual(R(7, 3).num(), 7)
        self.assertEqual(R(7, 3).den(), 3)


if __name__ == "__main__":
    unittest.main()
