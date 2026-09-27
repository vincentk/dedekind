"""Lwv set-comprehension DSL (#965), value-based iteration 2: runtime halfspaces.

The SET twin of the Jlt arrow tests.  Iteration 2 is value-based -- the pivot is
a runtime argument, so a single ``above(k)`` / ``below(k)`` / ... covers every
pivot (the iteration-1 ``gt_3`` / ``lt_5`` one-constant-per-pivot stopgap is
gone).  Membership is ``s(x)`` / ``x in s``; the meet ``a & b`` dispatches to the
same ``constexpr`` ``:order`` ``reduce_meet`` the compile-time ``static_assert``s
fold -- so the README collapse ``{x>3} ∩ {x<5} = {4}`` now runs with *runtime*
pivots.  No Python-side reducer.
"""

import unittest

from dedekind.lwv import above, at_least, at_most, below, everything, nothing, singleton


class LwvMembershipTest(unittest.TestCase):
    """Each set is a characteristic map χ: int -> bool, over a runtime pivot."""

    def test_open_halfspaces(self) -> None:
        self.assertTrue(above(3)(4))
        self.assertFalse(above(3)(3))  # strict
        self.assertFalse(above(3)(2))
        self.assertTrue(below(5)(4))
        self.assertFalse(below(5)(5))  # strict

    def test_closed_halfspaces(self) -> None:
        self.assertTrue(at_least(3)(3))  # non-strict includes the pivot
        self.assertTrue(at_most(5)(5))
        self.assertFalse(at_least(3)(2))

    def test_singleton_and_boundaries(self) -> None:
        self.assertTrue(4 in singleton(4))
        self.assertFalse(3 in singleton(4))
        self.assertTrue(everything()(42))  # 𝔸 contains everything
        self.assertFalse(nothing()(42))  # Ø contains nothing

    def test_arbitrary_runtime_pivots(self) -> None:
        # The point of value-based: pivots are runtime, not baked per-name.
        for k in (-100, 0, 7, 999):
            self.assertTrue(above(k)(k + 1))
            self.assertFalse(above(k)(k))
            self.assertTrue(at_most(k)(k))


class LwvMeetCollapseTest(unittest.TestCase):
    """`a & b` runs the value-first reduce_meet at runtime -- structural collapse."""

    def test_readme_collapse_to_singleton(self) -> None:
        # {x>3} ∩ {x<5} = {4}: the crossing law, folded at Python runtime.
        region = above(3) & below(5)
        self.assertEqual(region.kind, "singleton")
        self.assertEqual(region.cardinality, 1)
        self.assertEqual(repr(region), "{4}")
        self.assertTrue(region(4))
        self.assertFalse(region(3))
        self.assertFalse(region(5))

    def test_crossing_interval(self) -> None:
        # A wider crossing stays an interval (more than one inhabitant).
        region = above(3) & below(20)
        self.assertEqual(region.kind, "interval")
        self.assertEqual(region.cardinality, 16)  # {4..19}
        self.assertTrue(region(10))
        self.assertFalse(region(3))
        self.assertFalse(region(20))

    def test_disjoint_is_empty(self) -> None:
        region = above(5) & below(3)
        self.assertEqual(region.kind, "empty")
        self.assertEqual(region.cardinality, 0)
        self.assertFalse(region(4))

    def test_same_direction_keeps_tighter(self) -> None:
        # ↑3 ∩ ↑5 = ↑5 (larger pivot wins); ↓5 ∩ ↓3 = ↓3 (smaller wins).
        up = above(3) & above(5)
        self.assertEqual(up.kind, "halfspace")
        self.assertFalse(up(4))  # excluded: 4 not > 5
        self.assertTrue(up(6))
        down = below(5) & below(3)
        self.assertEqual(down.kind, "halfspace")
        self.assertTrue(down(2))
        self.assertFalse(down(4))  # excluded: 4 not < 3

    def test_boundary_units(self) -> None:
        # 𝔸 is the meet unit, Ø the annihilator.
        gt = above(3)
        self.assertEqual((gt & everything()).kind, "halfspace")
        self.assertEqual((gt & nothing()).kind, "empty")

    def test_singleton_meet(self) -> None:
        # {4} ∩ {x>3} = {4} (the point is in the halfspace); ∩ {x>10} = Ø.
        self.assertEqual((singleton(4) & above(3)).kind, "singleton")
        self.assertEqual((singleton(4) & above(10)).kind, "empty")


if __name__ == "__main__":
    unittest.main()
