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
