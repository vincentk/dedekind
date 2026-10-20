"""The laws of an arrow, inferred over the term: the sets of relations
``total``, ``univalent``, ``injective``, ``surjective`` (#1045)."""

import unittest

from dedekind.pst import K3, U64, Ternary, ap, u64, π_1, π_2, π_3, codomain
from dedekind.trsk import Function, copy, function, injective, surjective, total, univalent

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
        self.assertEqual(codomain((total & univalent)), K3)
        self.assertEqual(repr(total & univalent), "𝔸(Relation) & total & univalent")


class Functions(unittest.TestCase):
    """``function`` is the set ``total & univalent``; ``ap`` evaluates the
    terms that certify it, one per codomain column."""

    def test_function_is_the_meet_of_two_laws(self) -> None:
        for R in (succ, sq, succ >> succ, sq & succ, ~succ, copy(U64)):
            self.assertIs(R in function, R in total and R in univalent)
        self.assertEqual(ap(function, sq), Ternary.UNKNOWN)

    def test_a_map_into_a_product(self) -> None:
        c = copy(U64)
        self.assertIn(c, function)
        self.assertEqual(ap(c, 3), (3, 3))
        # A function, but not of the signature Function[u64, u64, u64]
        # promises (two inputs, one output): the annotation reads the split.
        with self.assertRaises(TypeError):
            Function[u64, u64, u64](c)
        add = U64 * U64 * U64 & (π_3 == π_1 + π_2)
        self.assertIs(Function[u64, u64, u64](add), add)

    def test_functions_compose(self) -> None:
        # function is closed under >> because total and univalent each are:
        # function(g) ∧ function(h) ≤ function(g >> h), and the composite
        # applies as the two in turn.
        triple = U64 * U64 & (π_2 == π_1 * 3)
        fs = (succ, triple, succ >> triple)
        for g in fs:
            for h in fs:
                self.assertIn(g >> h, function)
                for x in (0, 1, 7):
                    self.assertEqual(ap(g >> h, x), ap(h, ap(g, x)))


if __name__ == "__main__":
    unittest.main()
