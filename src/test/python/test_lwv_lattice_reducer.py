"""The lattice reducer at run time, driven from Python (one law, two binding
times).

The C++ ``static_assert`` exhibits fold ``Meet`` / ``Join`` / ``Not`` terms at
compile time.  The same algebra (``category::Reduce`` under ``cata``) folds
the same terms here on pivots that exist only at run time.  The laws decide on
the operand types where those differ; a leaf's data is combined by the carrier
(the interval meet).  The one law whose work is visible from here is
distributivity: ``a & ~b`` is ``Meet<Interval, Join<Interval, Interval>>``, and
the reducer spreads the meet over the union and rebuilds the two parts from the
values ``~b`` holds.
"""

import unittest

from dedekind.lwv import (
    Union,
    above,
    at_least,
    at_most,
    below,
    everything,
    nothing,
    singleton,
)


class TheMeetFoldsThroughTheReducer(unittest.TestCase):
    def test_the_bounds_meet_as_values(self) -> None:
        # Ø and 𝔸 are values of the one leaf type, so the carrier's meet, not
        # a type law, decides them: the result is the same set either way.
        s = above(3)
        self.assertEqual(everything() & s, s)
        self.assertEqual(s & everything(), s)
        self.assertEqual(nothing() & s, nothing())


class TheComplementIsAUnion(unittest.TestCase):
    def test_the_parts_below_and_above(self) -> None:
        u = ~singleton(5)
        self.assertIsInstance(u, Union)
        self.assertEqual(u.parts, (below(5), above(5)))
        self.assertEqual(repr(u), "{x | x < 5} ∪ {x | x > 5}")
        self.assertIn(4, u)
        self.assertNotIn(5, u)
        self.assertIn(6, u)

    def test_the_complement_of_a_ray_has_an_empty_part(self) -> None:
        u = ~above(3)  # {x <= 3} ∪ Ø
        self.assertEqual(u.parts, (at_most(3), nothing()))
        self.assertIn(3, u)
        self.assertNotIn(4, u)

    def test_the_complement_of_a_boundary(self) -> None:
        self.assertEqual((~nothing()).parts, (everything(), nothing()))
        self.assertFalse(~everything())  # Ø ∪ Ø: uninhabited


class DistributivityRebuildsItsValue(unittest.TestCase):
    """``a & (b ∪ c) → (a & b) ∪ (a & c)``: decided on the types at compile
    time, the two meets computed on the run-time pivots."""

    def test_a_meet_with_a_complement(self) -> None:
        u = at_least(0) & ~singleton(5)  # [0, 5) ∪ (5, ∞)
        self.assertIsInstance(u, Union)
        self.assertEqual(u.parts, (at_least(0) & below(5), above(5)))
        self.assertIn(0, u)
        self.assertIn(4, u)
        self.assertNotIn(5, u)
        self.assertIn(6, u)
        self.assertNotIn(-1, u)

    def test_the_meet_commutes(self) -> None:
        a, u = at_least(0), ~singleton(5)
        self.assertEqual(a & u, u & a)

    def test_a_part_may_vanish(self) -> None:
        u = above(10) & ~singleton(5)  # Ø ∪ (10, ∞)
        self.assertEqual(u.parts, (nothing(), above(10)))
        self.assertTrue(u)
        self.assertEqual((below(0) & ~at_least(0)).parts, (below(0), nothing()))

    def test_a_contradiction_is_uninhabited(self) -> None:
        s = at_least(0) & at_most(10)
        u = s & ~s  # Ø ∪ Ø
        self.assertFalse(u)
        for x in (-1, 0, 5, 10, 11):
            self.assertNotIn(x, u)

    def test_meets_chain(self) -> None:
        u = (at_least(0) & ~singleton(5)) & at_most(7)  # [0,5) ∪ (5,7]
        self.assertEqual(u.parts, (at_least(0) & below(5), above(5) & at_most(7)))
        self.assertNotIn(8, u)
        self.assertIn(7, u)


if __name__ == "__main__":
    unittest.main()
