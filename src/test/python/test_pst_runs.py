"""Sets of the word u64 built from π ⋈ k atoms are run lists: one normal form.

The former binds the term, and the order fragment (comparisons of π with a
constant under & and |) is read as the set's run list, so every spelling of one
set reaches the same canonical form and equality, ``canonical``, ``len``, iteration
and slicing by value are the library's.  ``image`` / ``preimage`` under the
structural arrows substitute the variable; on the word the run list translates
and wraps at the end.  Nothing is decided in Python.
"""

import unittest

from dedekind.jlt import id, pred, refl, succ
from dedekind.pst import A, canonical, image, preimage, u64, π

U = A(u64)
MAX = 2**64 - 1


def above(k):
    return U | (π > k)


def below(k):
    return U | (π < k)


def at_least(k):
    return U | (π >= k)


def at_most(k):
    return U | (π <= k)


def point(k):
    return U | (π == k)


everything = at_least(0)  # the universe, as a term set
nothing = below(0)


class OneSetOneNormalForm(unittest.TestCase):
    def test_the_readme_collapse(self) -> None:
        # {x > 3} ∩ {x < 5} = {4}: the crossing law, on runtime pivots.
        region = above(3) & below(5)
        self.assertEqual(region, point(4))
        self.assertEqual(repr(canonical(region)), "𝔸(u64) | π == 4")
        self.assertEqual(region.cardinality, 1)
        self.assertEqual(len(region), 1)
        self.assertTrue(region(4))
        self.assertFalse(region(5))

    def test_the_canonical_form_is_the_run_list_written_back(self) -> None:
        self.assertEqual(repr(canonical(U | (π < 0))), "𝔸(u64) | π < 0")  # Ø
        self.assertEqual(repr(canonical(~(U | (π < 0)))), "𝔸(u64) | π >= 0")  # all
        self.assertEqual(repr(canonical(above(3) & below(9))), "𝔸(u64) | (π >= 4) & (π <= 8)")
        self.assertEqual(repr(canonical(above(MAX - 1))), f"𝔸(u64) | π == {MAX}")
        S = (above(3) & below(9)) | point(12)
        self.assertEqual(repr(canonical(canonical(S))), repr(canonical(S)))  # idempotent

    def test_a_set_applied_to_a_term_substitutes(self) -> None:
        S = above(5)
        self.assertEqual(U | S(π + 1), preimage(succ(int), S))  # {x | x + 1 ∈ S}
        self.assertEqual(repr(S(π + 1)), "(π + 1) > 5")
        with self.assertRaises(TypeError):
            U | (π + 1)  # a term valued in u64 is not a predicate

    def test_an_atom_normalised_into_the_fragment_reads_as_runs(self) -> None:
        # π * 1 < 5 is no atom of the chain fragment; its polynomial form π < 5
        # is, and canonical reads it as runs in the same pass: a fixpoint.
        S = U | (π * 1 < 5)
        self.assertEqual(repr(canonical(S)), "𝔸(u64) | π <= 4")
        self.assertEqual(repr(canonical(canonical(S))), repr(canonical(S)))
        self.assertEqual(repr(canonical(U | (π + 1 - 1 == 4))), "𝔸(u64) | π == 4")

    def test_the_complement_is_a_run_list(self) -> None:
        u = ~point(5)
        self.assertEqual(repr(canonical(u)), "𝔸(u64) | (π <= 4) | (π >= 6)")
        self.assertEqual(~above(3), at_most(3))
        self.assertEqual(repr(canonical(at_least(0) & ~point(5))), repr(canonical(u)))

    def test_every_spelling_reaches_the_same_run_list(self) -> None:
        u = at_least(0) & ~point(5)
        self.assertEqual(u, below(5) | above(5))
        self.assertEqual(u, ~(below(0) | point(5)))  # De Morgan
        self.assertEqual(u, U | ((π < 5) | (π > 5)))  # combined before binding
        a, b = at_least(0), ~point(5)
        self.assertEqual(a & b, b & a)
        self.assertEqual(a | b, b | a)
        self.assertEqual(at_most(4) | at_least(5), everything)  # adjacent runs merge
        self.assertEqual((at_least(0) & at_most(3)) | (at_least(4) & at_most(9)), at_least(0) & at_most(9))
        self.assertEqual(U | (5 < π), above(5))  # the converse

    def test_the_boundaries(self) -> None:
        s = at_least(0) & at_most(10)
        self.assertEqual(s & ~s, nothing)
        self.assertFalse(s & ~s)
        self.assertTrue(s)
        self.assertEqual(s | ~s, everything)
        self.assertEqual(everything.cardinality, 2**64)
        self.assertEqual(U.cardinality, 2**64)

    def test_a_term_outside_the_catalogue_has_no_normal_form(self) -> None:
        evens = U | (π % 2 == 0)
        self.assertIn(4, evens)  # membership is pointwise
        with self.assertRaises(TypeError):
            evens == everything  # noqa: B015 (the refusal is the point)
        with self.assertRaises(TypeError):
            canonical(evens)  # % has no canonical form


class TheElementsAreReadOffTheRuns(unittest.TestCase):
    def test_a_bounded_set_iterates_like_range(self) -> None:
        for a, b in ((3, 8), (0, 1), (0, 0)):
            self.assertEqual(list(at_least(a) & below(b)), list(range(a, b)))
            self.assertEqual(len(at_least(a) & below(b)), b - a)
        self.assertEqual(list(point(4)), [4])
        self.assertEqual(list(nothing), [])
        u = (at_least(0) & below(3)) | (above(6) & at_most(8))
        self.assertEqual(list(u), [0, 1, 2, 7, 8])
        self.assertEqual(len(u), 5)
        it = iter(at_least(MAX - 1))
        self.assertEqual(next(it), MAX - 1)
        self.assertEqual(next(it), MAX)
        with self.assertRaises(StopIteration):
            next(it)

    def test_slicing_is_by_value(self) -> None:
        self.assertEqual(everything[3:8], at_least(3) & below(8))
        self.assertEqual(list(everything[3:8]), list(range(3, 8)))
        self.assertEqual(at_least(3)[:8], at_least(3) & below(8))
        self.assertEqual(below(8)[3:], at_least(3) & below(8))
        self.assertEqual(everything[:], everything)
        self.assertFalse(point(4)[0:4])
        with self.assertRaises(TypeError):
            above(3)[0]  # no enumeration to index into
        with self.assertRaises(TypeError):
            everything[0:10:2]  # the meet with a residue class
        with self.assertRaises(ValueError):
            everything[-5:3]  # -5 is a value the word cannot hold


class ImageAndPreimage(unittest.TestCase):
    def test_structural_arrows_shift_the_set(self) -> None:
        bounded = at_least(3) & below(8)
        self.assertEqual(image(succ(int), bounded), at_least(4) & below(9))
        self.assertEqual(preimage(succ(int), at_least(4) & below(9)), bounded)
        self.assertEqual(image(pred(int), at_most(5)), at_most(4) | point(MAX))  # pred(0) wraps
        self.assertEqual(image(succ(int), point(4)), point(5))
        self.assertEqual(image(id(int), above(5)), above(5))
        self.assertEqual(image(succ(int), everything), everything)

    def test_the_word_wraps(self) -> None:
        # The successor sends the word's end to 0, so a ray gains the point 0.
        self.assertEqual(image(succ(int), above(5)), U | ((π > 6) | (π == 0)))
        self.assertEqual(preimage(succ(int), at_most(0)), point(MAX))
        self.assertIn(MAX, preimage(succ(int), at_most(0)))
        self.assertEqual(succ(int)(MAX), 0)
        self.assertEqual(pred(int)(0), MAX)

    def test_an_erased_composite_is_refused(self) -> None:
        with self.assertRaises(TypeError):
            image(refl(int), above(5))
        with self.assertRaises(TypeError):
            image(succ(int) >> succ(int), above(5))


if __name__ == "__main__":
    unittest.main()
