"""The bounded Collatz exhibit: the rule from the library's arrows, the orbit as
its iterate, the search read in K₃, the window ∀, and the refusals."""

import itertools
import unittest

from dedekind.collatz import (
    Ternary, at_least, below, collatz_step, cond, everything, forall, iterate,
    nothing, reaches_within, singleton, Σ, π,
)

F, U, T = Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE
step = cond(π % 2 == 0, π // 2, 3 * π + 1)


class ArrowTest(unittest.TestCase):
    def test_the_rule_is_a_term_in_the_arrows(self) -> None:
        self.assertEqual(step(27), 82)
        self.assertEqual(step(6), 3)
        self.assertEqual(collatz_step(27), 82)
        self.assertEqual((π % 2)(7), 1)
        self.assertEqual((3 * π + 1)(27), 82)
        self.assertEqual((π // 2)(7), 3)
        self.assertTrue((π % 2 == 0)(4))
        self.assertFalse((π % 2 == 0)(7))
        self.assertIn(4, π % 2 == 0)
        self.assertEqual((π >> (π + 1))(1), 2)

    def test_refusals(self) -> None:
        with self.assertRaises(ValueError):
            π // 0
        with self.assertRaises(ValueError):
            (-3) * π
        with self.assertRaises(ValueError):
            step(-1)


class OrbitTest(unittest.TestCase):
    def test_the_orbit_is_the_iterate(self) -> None:
        self.assertEqual(iterate(step, 6)[:9], [6, 3, 10, 5, 16, 8, 4, 2, 1])
        self.assertEqual(iterate(step, 27)[:6], [27, 82, 41, 124, 62, 31])
        self.assertEqual(iterate(step, 27)[1], 82)
        self.assertEqual(max(iterate(step, 27)[:112]), 9232)
        self.assertEqual(list(itertools.islice(iterate(step, 1), 6)), [1, 4, 2, 1, 4, 2])
        self.assertEqual(iterate(collatz_step, 27)[:6], iterate(step, 27)[:6])

    def test_first_where_is_the_bounded_search(self) -> None:
        self.assertEqual(iterate(step, 6).first_where(π == 1, 100), 8)
        self.assertEqual(iterate(step, 27).first_where(π == 1, 120), 111)
        self.assertIsNone(iterate(step, 27).first_where(π == 1, 50))
        self.assertEqual(iterate(step, 1).first_where(π == 1, 0), 0)

    def test_refusals(self) -> None:
        with self.assertRaises(ValueError):
            iterate(step, -1)
        with self.assertRaises(TypeError):
            iterate(step, 27)[:]
        with self.assertRaises(IndexError):
            iterate(step, 27)[-1]


class VerdictTest(unittest.TestCase):
    def test_sigma_reads_a_search_in_k3(self) -> None:
        self.assertEqual(Σ(True), T)
        self.assertEqual(Σ(False), U)

    def test_the_verdict_is_the_composition(self) -> None:
        P = reaches_within(step, 120)
        self.assertEqual(P(27), T)
        self.assertEqual(P(27), Σ(iterate(step, 27).first_where(π == 1, 120) is not None))
        self.assertEqual(reaches_within(step, 50)(27), U)
        self.assertEqual(reaches_within(step, 8)(6), T)
        self.assertEqual(reaches_within(step, 7)(6), U)
        self.assertEqual(P.budget, 120)
        with self.assertRaises(ValueError):
            P(-3)


class WindowForallTest(unittest.TestCase):
    def test_the_window_decides_with_a_sufficient_budget(self) -> None:
        W = at_least(1) & below(1000)
        self.assertEqual(forall(W, reaches_within(step, 50)), U)
        self.assertEqual(forall(W, reaches_within(step, 177)), U)  # 871 needs 178
        self.assertEqual(forall(W, reaches_within(step, 178)), T)
        self.assertEqual(forall(singleton(27), reaches_within(step, 111)), T)
        self.assertEqual(forall(singleton(27), reaches_within(step, 110)), U)
        self.assertEqual(forall(nothing(), reaches_within(step, 0)), T)  # vacuous

    def test_a_changed_constant_changes_the_computation(self) -> None:
        five = cond(π % 2 == 0, π // 2, 5 * π + 1)
        self.assertEqual(iterate(five, 1)[:8], [1, 6, 3, 16, 8, 4, 2, 1])
        self.assertEqual(reaches_within(five, 20)(7), U)
        self.assertEqual(forall(at_least(1) & below(100), reaches_within(five, 1000)), U)

    def test_without_a_window_the_forall_is_the_conjecture(self) -> None:
        for S in (everything(), at_least(1), below(1000)):
            with self.assertRaises(TypeError):
                forall(S, reaches_within(step, 300))
        with self.assertRaises(TypeError):
            forall(at_least(-5) & below(10), reaches_within(step, 300))


if __name__ == "__main__":
    unittest.main()
