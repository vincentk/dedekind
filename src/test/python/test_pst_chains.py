"""Pst chains (#1001): the successor as the covering map, from Python.

Three bounded chains, one shape: ``B`` and ``K3`` are truth objects, ``N`` is
ℕ's proxy with ⊤ = ℵ₀.  The step is read twice -- the total, saturating
``succ`` / ``pred`` and the partial ``cover`` (``None`` at ⊤) -- and iterating a
chain unfolds its bottom by the cover, stopping where the cover stops.  The
classification attributes are the C++ concepts' verdicts, not Python flags.
"""

import itertools
import unittest

from dedekind.pst import B, K3, N, Ternary, aleph0


class PstEndpointsAndStepTest(unittest.TestCase):
    def test_bool_chain(self) -> None:
        self.assertIs(B.bottom, False)
        self.assertIs(B.top, True)
        self.assertIs(B.succ(False), True)
        self.assertIs(B.succ(True), True)  # saturates at ⊤
        self.assertIs(B.pred(False), False)  # saturates at ⊥
        self.assertIs(B.cover(False), True)
        self.assertIsNone(B.cover(True))  # the cover is partial at ⊤

    def test_kleene_chain(self) -> None:
        self.assertEqual(K3.bottom, Ternary.FALSE)
        self.assertEqual(K3.top, Ternary.TRUE)
        self.assertEqual(K3.succ(Ternary.FALSE), Ternary.UNKNOWN)
        self.assertEqual(K3.succ(Ternary.UNKNOWN), Ternary.TRUE)
        self.assertEqual(K3.succ(Ternary.TRUE), Ternary.TRUE)
        self.assertEqual(K3.cover(Ternary.UNKNOWN), Ternary.TRUE)
        self.assertIsNone(K3.cover(Ternary.TRUE))
        self.assertTrue(K3.le(Ternary.FALSE, Ternary.UNKNOWN))
        self.assertFalse(K3.le(Ternary.TRUE, Ternary.UNKNOWN))

    def test_natural_chain(self) -> None:
        self.assertEqual(N.bottom, 0)
        self.assertEqual(N.top, aleph0)
        self.assertEqual(N.succ(41), 42)
        self.assertEqual(N.succ(aleph0), aleph0)  # saturating at the top
        self.assertEqual(N.pred(0), 0)  # the monus: 0 is a fixpoint
        self.assertEqual(N.pred(aleph0), aleph0)
        self.assertEqual(N.cover(41), 42)
        self.assertIsNone(N.cover(aleph0))  # ⊤ has no cover
        self.assertTrue(N.le(3, aleph0))
        with self.assertRaises(ValueError):
            N.succ(-1)  # ℕ has no negatives


class PstUnfoldTest(unittest.TestCase):
    def test_finite_chains_unfold_to_their_elements(self) -> None:
        self.assertEqual(list(B), [False, True])
        self.assertEqual(list(K3), [Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE])

    def test_natural_chain_unfolds_like_count(self) -> None:
        # ℕ's top is a limit, not a successor: the unfold never reaches it.
        self.assertEqual(list(itertools.islice(N, 5)), [0, 1, 2, 3, 4])
        self.assertEqual(
            list(itertools.islice(N, 5)), list(itertools.islice(itertools.count(0), 5))
        )


class PstClassificationTest(unittest.TestCase):
    def test_concept_verdicts(self) -> None:
        for chain in (B, K3, N):
            self.assertTrue(chain.is_bounded)
            self.assertFalse(chain.is_dense)  # a chain with the step is discrete
            self.assertTrue(chain.saturates)
        self.assertTrue(B.is_truth_object)
        self.assertTrue(K3.is_truth_object)
        self.assertFalse(N.is_truth_object)  # a chain of the same shape, not Pst proper

    def test_cardinalities(self) -> None:
        self.assertEqual(B.cardinality, 2)
        self.assertEqual(K3.cardinality, 3)
        self.assertEqual(N.cardinality, aleph0)
        self.assertEqual(repr(aleph0), "ℵ₀")
