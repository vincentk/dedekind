"""The bounded Collatz exhibit: the orbit, the budgeted verdict in K₃, the
window ∀, and the refusals that are the open problem."""

import itertools
import unittest

from dedekind.collatz import (
    Ternary, at_least, below, collatz, everything, forall, nothing,
    reaches_within, singleton,
)

F, U, T = Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE


class OrbitTest(unittest.TestCase):
    def test_the_orbit_is_the_iterate(self) -> None:
        self.assertEqual(collatz(6)[:9], [6, 3, 10, 5, 16, 8, 4, 2, 1])
        self.assertEqual(collatz(27)[:6], [27, 82, 41, 124, 62, 31])
        self.assertEqual(collatz(27)[1], 82)
        self.assertEqual(max(collatz(27)[:112]), 9232)
        self.assertEqual(list(itertools.islice(collatz(1), 6)), [1, 4, 2, 1, 4, 2])
        self.assertEqual(collatz(27).seed, 27)

    def test_reach_time_is_the_first_index_at_one(self) -> None:
        self.assertEqual(collatz(6).reach_time(100), 8)
        self.assertEqual(collatz(27).reach_time(120), 111)
        self.assertIsNone(collatz(27).reach_time(50))
        self.assertEqual(collatz(1).reach_time(0), 0)

    def test_refusals(self) -> None:
        with self.assertRaises(ValueError):
            collatz(-1)
        with self.assertRaises(TypeError):
            collatz(27)[:]  # no end to slice to
        with self.assertRaises(IndexError):
            collatz(27)[-1]


class VerdictTest(unittest.TestCase):
    def test_true_or_unknown_never_false(self) -> None:
        self.assertEqual(reaches_within(120)(27), T)
        self.assertEqual(reaches_within(50)(27), U)
        self.assertEqual(reaches_within(8)(6), T)
        self.assertEqual(reaches_within(7)(6), U)
        self.assertEqual(reaches_within(10).budget, 10)
        with self.assertRaises(ValueError):
            reaches_within(10)(-3)


class WindowForallTest(unittest.TestCase):
    def test_the_window_decides_with_a_sufficient_budget(self) -> None:
        W = at_least(1) & below(1000)
        self.assertEqual(forall(W, reaches_within(50)), U)
        self.assertEqual(forall(W, reaches_within(177)), U)  # 871 needs 178
        self.assertEqual(forall(W, reaches_within(178)), T)
        self.assertEqual(forall(singleton(27), reaches_within(111)), T)
        self.assertEqual(forall(singleton(27), reaches_within(110)), U)
        self.assertEqual(forall(nothing(), reaches_within(0)), T)  # vacuous

    def test_without_a_window_the_forall_is_the_conjecture(self) -> None:
        for S in (everything(), at_least(1), below(1000)):
            with self.assertRaises(TypeError):
                forall(S, reaches_within(300))


if __name__ == "__main__":
    unittest.main()
