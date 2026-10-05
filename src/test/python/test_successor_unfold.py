"""The successor across the three Python idioms (#1001).

``jlt``: ``succ(T)`` / ``pred(T)`` are typed arrows beside ``id`` / ``refl``,
composable with ``>>``.  ``lwv``: ``image`` / ``preimage`` (paper §4) move a
set's bounds in closed form for the structural arrows, and refuse an erased
composite rather than guess; a set is iterable by the successor from its least
element, so a bounded set is ``range`` and a ray is ``itertools.count``.  No
Python-side loop decides anything: the unfold's seed and stop come from C++.
"""

import itertools
import unittest

from dedekind.jlt import cod, dom, id, pred, refl, succ
from dedekind.lwv import (
    above,
    at_least,
    at_most,
    below,
    everything,
    image,
    nothing,
    preimage,
    singleton,
)


class SuccessorArrowTest(unittest.TestCase):
    def test_step_arrows_have_the_arrow_protocol(self) -> None:
        for arrow in (succ(int), pred(int), succ(bool), pred(bool)):
            self.assertTrue(hasattr(arrow, "__call__"))
            self.assertTrue(hasattr(arrow, "__rshift__"))
        self.assertIs(dom(succ(int)), int)
        self.assertIs(cod(pred(int)), int)
        self.assertIs(dom(succ(bool)), bool)

    def test_step_on_int_and_its_composition(self) -> None:
        self.assertEqual(succ(int)(41), 42)
        self.assertEqual(pred(int)(42), 41)
        self.assertEqual((succ(int) >> succ(int))(5), 7)
        self.assertEqual((succ(int) >> pred(int))(5), 5)
        self.assertEqual((id(int) >> succ(int))(5), 6)

    def test_step_on_bool_saturates(self) -> None:
        self.assertIs(succ(bool)(False), True)
        self.assertIs(succ(bool)(True), True)
        self.assertIs(pred(bool)(True), False)
        self.assertIs(pred(bool)(False), False)

    def test_bad_carrier_raises(self) -> None:
        with self.assertRaises(TypeError):
            succ(str)


class ImagePreimageTest(unittest.TestCase):
    def test_image_of_the_successor_is_the_affine_pushforward(self) -> None:
        # Paper §4: the image of {n > 5} under the successor is {n > 6}, decided
        # with no search of the domain.
        self.assertEqual(image(succ(int), above(5)), above(6))
        self.assertEqual(preimage(succ(int), above(6)), above(5))
        self.assertEqual(image(pred(int), at_most(5)), at_most(4))
        self.assertEqual(image(succ(int), singleton(4)), singleton(5))
        self.assertEqual(image(succ(int), at_least(3) & below(8)), at_least(4) & below(9))
        self.assertEqual(image(id(int), above(5)), above(5))

    def test_boundaries_are_fixed(self) -> None:
        self.assertEqual(image(succ(int), everything()), everything())
        self.assertEqual(image(succ(int), nothing()), nothing())

    def test_erased_composite_is_refused(self) -> None:
        # refl and any >> composite are erased: their image is intensional.
        with self.assertRaises(TypeError):
            image(refl(int), above(5))
        with self.assertRaises(TypeError):
            image(succ(int) >> succ(int), above(5))


class SetSlicingByValueTest(unittest.TestCase):
    """s[a:b] is s ∩ [a, b): pandas' .loc, never .iloc."""

    def test_slice_is_the_meet_with_the_interval(self) -> None:
        self.assertEqual(everything()[3:8], at_least(3) & below(8))
        self.assertEqual(list(everything()[3:8]), list(range(3, 8)))
        self.assertEqual(above(0)[-5:3], at_least(1) & below(3))  # -5 is a VALUE
        self.assertEqual(at_least(3)[:8], at_least(3) & below(8))  # the lower cut
        self.assertEqual(below(8)[3:], at_least(3) & below(8))  # the upper ray
        self.assertEqual(everything()[:], everything())
        self.assertEqual(singleton(4)[0:4], nothing())

    def test_positional_and_stepped_forms_are_refused(self) -> None:
        with self.assertRaises(TypeError):
            above(3)[0]  # no enumeration to index into
        with self.assertRaises(TypeError):
            above(3)[-1]
        with self.assertRaises(TypeError):
            everything()[0:10:2]  # awaits the congruence sets


class WindowBoundaryTest(unittest.TestCase):
    """Python's int is ℤ; the carrier is its 64-bit window.  Where ℤ continues
    and the window cannot, the surface raises OverflowError rather than wrap; a
    bounded unfold that ends at the window's last value ends normally."""

    LAST = 2**63 - 1
    FIRST = -(2**63)

    def test_step_at_the_ends_raises(self) -> None:
        self.assertEqual(succ(int)(self.LAST - 1), self.LAST)
        self.assertEqual(pred(int)(self.FIRST + 1), self.FIRST)
        with self.assertRaises(OverflowError):
            succ(int)(self.LAST)
        with self.assertRaises(OverflowError):
            pred(int)(self.FIRST)
        with self.assertRaises((OverflowError, TypeError)):
            succ(int)(2**63)  # not in the window at all

    def test_image_past_the_end_raises(self) -> None:
        with self.assertRaises(OverflowError):
            image(succ(int), at_least(self.LAST))
        with self.assertRaises(OverflowError):
            preimage(succ(int), at_most(self.FIRST))

    def test_bounded_unfold_ends_at_the_window_last_value(self) -> None:
        self.assertEqual(list(singleton(self.LAST)), [self.LAST])
        self.assertEqual(
            list(at_least(self.LAST - 1) & at_most(self.LAST)),
            [self.LAST - 1, self.LAST],
        )

    def test_ray_unfold_raises_where_the_window_ends(self) -> None:
        it = iter(at_least(self.LAST - 1))
        self.assertEqual(next(it), self.LAST - 1)
        self.assertEqual(next(it), self.LAST)
        with self.assertRaises(OverflowError):
            next(it)


class SetUnfoldTest(unittest.TestCase):
    def test_bounded_set_is_range(self) -> None:
        for a, b in ((3, 8), (0, 1), (-2, 2)):
            self.assertEqual(list(at_least(a) & below(b)), list(range(a, b)))
            self.assertEqual(len(at_least(a) & below(b)), b - a)
        self.assertEqual(list(singleton(4)), [4])
        self.assertEqual(list(nothing()), [])
        self.assertEqual(len(nothing()), 0)
        self.assertEqual(list(above(3) & at_most(5)), [4, 5])

    def test_ray_is_count(self) -> None:
        self.assertEqual(
            list(itertools.islice(at_least(3), 4)),
            list(itertools.islice(itertools.count(3), 4)),
        )
        self.assertEqual(list(itertools.islice(above(3), 2)), [4, 5])

    def test_unbounded_sets_have_no_length_and_no_bottom(self) -> None:
        with self.assertRaises(TypeError):
            len(above(3))  # ℵ₀ is not a Python int
        with self.assertRaises(TypeError):
            iter(below(3))  # no least element to unfold from
        with self.assertRaises(TypeError):
            iter(everything())

    def test_classification_and_truthiness(self) -> None:
        self.assertTrue((at_least(3) & below(8)).is_bounded)
        self.assertTrue((at_least(3) & below(8)).is_finite)
        self.assertFalse(above(3).is_bounded)
        self.assertTrue(above(3))  # inhabited
        self.assertFalse(nothing())  # empty
        self.assertTrue(singleton(0))  # inhabited, even at 0
