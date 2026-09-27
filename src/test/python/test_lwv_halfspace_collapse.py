"""Lwv set-comprehension DSL (#965): the README halfspace collapse at runtime.

The SET twin of the Jlt arrow tests.  These pin the README exhibit --
``{x > 3} ∩ {x < 5} = {4}`` -- observed through the Python bindings, matching
the compile-time ``static_assert`` collapse.  The two halfspaces are
duck-typed handles (membership via ``s(x)`` / ``x in s``); their intersection
``&`` dispatches to the real C++ ``:order`` reducer (``structured_and``), which
collapses the crossing meet to the singleton ``{4}`` by cardinality analysis.
No Python-side reducer: the handle routes to the C++ one.

First iteration: pivots are compile-time, so the exhibit binds the curated
README sets (as ``jlt`` binds the fixed generators ``id`` / ``refl``).  A fluent
constructor over runtime pivots is the next iteration (#922 slice 2).
"""

import unittest

from dedekind.lwv import gt_3, lt_5


class LwvHalfspaceMembershipTest(unittest.TestCase):
    """Each halfspace is a characteristic map χ: int -> bool."""

    def test_upper_halfspace_membership(self) -> None:
        # gt_3 = {x ∈ int | x > 3}: the strict principal filter ↑3.
        self.assertIs(gt_3(4), True)
        self.assertIs(gt_3(100), True)
        self.assertIs(gt_3(3), False)  # strict: the pivot itself is excluded
        self.assertIs(gt_3(2), False)

    def test_lower_halfspace_membership(self) -> None:
        # lt_5 = {x ∈ int | x < 5}: the strict principal ideal ↓5.
        self.assertIs(lt_5(4), True)
        self.assertIs(lt_5(-10), True)
        self.assertIs(lt_5(5), False)  # strict
        self.assertIs(lt_5(6), False)

    def test_contains_is_membership(self) -> None:
        # `x in s` is the same characteristic map, Python-idiomatic.
        self.assertTrue(4 in gt_3)
        self.assertFalse(3 in gt_3)
        self.assertTrue(4 in lt_5)
        self.assertFalse(5 in lt_5)


class LwvReadmeCollapseTest(unittest.TestCase):
    """{x>3} ∩ {x<5} collapses to the singleton {4} -- the README exhibit."""

    def test_meet_collapses_to_singleton(self) -> None:
        # gt_3 & lt_5 dispatches to the real :order reducer (structured_and),
        # which reduces the crossing meet to the singleton {4} at Python
        # runtime -- the same collapse the compile-time static_assert folds.
        collapse = gt_3 & lt_5
        self.assertEqual(collapse.cardinality, 1)
        self.assertEqual(repr(collapse), "{4}")

    def test_singleton_membership_is_exactly_four(self) -> None:
        # The collapsed set contains 4 and nothing else in the window.
        collapse = gt_3 & lt_5
        self.assertIs(collapse(4), True)
        self.assertIs(collapse(3), False)  # boundary of gt_3
        self.assertIs(collapse(5), False)  # boundary of lt_5
        self.assertIs(collapse(2), False)
        self.assertIs(collapse(6), False)


if __name__ == "__main__":
    unittest.main()
