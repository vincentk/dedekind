"""The normal form of sets of ℤ built from intervals, driven from Python.

A set is a run list: finitely many disjoint intervals in order, the canonical
form of the Boolean algebra the intervals generate.  ``&``, ``|`` and ``~``
fold through the same lattice reducer the C++ ``static_assert`` exhibits fold,
here on pivots that exist only at run time, and every spelling of one set
reaches the same run list.
"""

import unittest

from dedekind.lwv import (
    above,
    at_least,
    at_most,
    below,
    everything,
    nothing,
    singleton,
)


class TheBoundsAreValues(unittest.TestCase):
    def test_the_bounds_meet_and_join_as_values(self) -> None:
        s = above(3)
        self.assertEqual(everything() & s, s)
        self.assertEqual(nothing() & s, nothing())
        self.assertEqual(everything() | s, everything())
        self.assertEqual(nothing() | s, s)


class TheComplementIsARunList(unittest.TestCase):
    def test_the_parts_below_and_above(self) -> None:
        u = ~singleton(5)
        self.assertEqual(u.runs(), [below(5), above(5)])
        self.assertEqual(repr(u), "{x | x < 5} ∪ {x | x >= 6}")
        self.assertIn(4, u)
        self.assertNotIn(5, u)
        self.assertIn(6, u)

    def test_the_complement_of_a_ray_is_one_run(self) -> None:
        self.assertEqual((~above(3)).runs(), [at_most(3)])
        self.assertEqual(~above(3), at_most(3))

    def test_the_complement_of_a_boundary(self) -> None:
        self.assertEqual(~nothing(), everything())
        self.assertEqual(~everything(), nothing())
        self.assertFalse(~everything())

    def test_the_complement_is_an_involution(self) -> None:
        s = at_least(0) & ~singleton(5)
        self.assertEqual(~~s, s)


class OneSetOneNormalForm(unittest.TestCase):
    """Every spelling of a set reaches the same run list."""

    def test_a_meet_with_a_complement(self) -> None:
        u = at_least(0) & ~singleton(5)  # [0, 5) ∪ (5, ∞)
        self.assertEqual(u.runs(), [at_least(0) & below(5), above(5)])
        self.assertEqual(u, (at_least(0) & below(5)) | above(5))
        self.assertEqual(u, ~(below(0) | singleton(5)))  # De Morgan
        for x, member in ((-1, False), (0, True), (4, True), (5, False), (6, True)):
            self.assertEqual(x in u, member)

    def test_meet_and_join_commute(self) -> None:
        a, b = at_least(0), ~singleton(5)
        self.assertEqual(a & b, b & a)
        self.assertEqual(a | b, b | a)

    def test_adjacent_and_overlapping_runs_merge(self) -> None:
        self.assertEqual(at_most(4) | at_least(5), everything())
        self.assertEqual((at_least(0) & at_most(3)) | (at_least(4) & at_most(9)),
                         at_least(0) & at_most(9))
        self.assertEqual(len((at_least(0) & below(10)) | (above(20) & below(25))), 14)

    def test_a_contradiction_is_empty(self) -> None:
        s = at_least(0) & at_most(10)
        self.assertEqual(s & ~s, nothing())
        self.assertEqual(s | ~s, everything())
        self.assertFalse(s & ~s)

    def test_a_union_of_runs_iterates_in_order(self) -> None:
        u = (at_least(0) & below(3)) | (above(6) & at_most(8))
        self.assertEqual(list(u), [0, 1, 2, 7, 8])
        self.assertEqual(len(u), 5)
        self.assertTrue(u.is_bounded)


if __name__ == "__main__":
    unittest.main()
