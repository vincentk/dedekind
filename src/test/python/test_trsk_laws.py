"""The laws of an arrow, inferred over the term: the sets of relations
``total``, ``univalent``, ``injective``, ``surjective`` (#1045)."""

import unittest

from dedekind.pst import K3, U64, Ternary, ap, π_1, π_2
from dedekind.trsk import copy, injective, surjective, total, univalent

succ = U64 * U64 & (π_2 == π_1 + 1)
sq = U64 * U64 & (π_2 * π_2 == π_1)


class Laws(unittest.TestCase):
    def test_a_graph_is_a_map(self) -> None:
        self.assertIn(succ, total)
        self.assertIn(succ, univalent)
        self.assertIn(succ, total & univalent)
        # Not inferred: the rules certify, they do not refute.
        self.assertNotIn(succ, injective)
        self.assertEqual(ap(injective, succ), Ternary.UNKNOWN)
        self.assertEqual(ap(total, succ), Ternary.TRUE)

    def test_the_rules_over_the_term(self) -> None:
        self.assertIn(succ >> succ, total & univalent)  # maps compose
        self.assertIn(sq & succ, univalent)  # a meet keeps univalence
        self.assertNotIn(sq & succ, total)  # and may lose totality
        self.assertNotIn(~succ, total | univalent | injective | surjective)
        self.assertNotIn(sq, univalent)  # no rule says so

    def test_the_complement_certifies_nothing(self) -> None:
        self.assertEqual(ap(~total, succ), Ternary.FALSE)  # refuted: inferred
        self.assertEqual(ap(~total, ~succ), Ternary.UNKNOWN)  # Σ, not Π
        self.assertEqual((total & univalent).codomain, K3)
        self.assertEqual(repr(total & univalent), "𝔸(Relation) & total & univalent")


class OneCertificate(unittest.TestCase):
    """``R.is_function`` is ``R in total & univalent``; ``ap`` evaluates the
    terms that certify it, one per codomain column."""

    def test_is_function_is_the_law_set(self) -> None:
        for R in (succ, sq, succ >> succ, sq & succ, ~succ, copy(U64)):
            self.assertIs(R.is_function, R in total & univalent)

    def test_a_map_into_a_product(self) -> None:
        c = copy(U64)
        self.assertIs(c.is_function, True)
        self.assertEqual(ap(c, 3), (3, 3))


if __name__ == "__main__":
    unittest.main()
