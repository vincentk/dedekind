"""The truth chains' universes (#1001): the successor as the covering map.

``B = A(bool)`` and ``K3 = A(Ternary)`` are the universes over the two truth
chains, and carry the chain's order.  The step is read twice -- the total,
saturating ``succ`` / ``pred`` and the partial ``cover`` (``None`` at ⊤) -- and
iterating a universe unfolds its bottom by the cover, stopping where the cover
stops.  The classification attributes are the C++ concepts' verdicts, not
Python flags.
"""

import unittest

from dedekind.pst import 𝔹, A, K3, Function, Set, Ternary, π

# The written forms are the names: NFKC normalisation makes 𝔹 the identifier B.
# Subscript digits are not identifier characters, so K₃ is spelled K3 and
# prints as itself.
B = 𝔹


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


class PstUnfoldTest(unittest.TestCase):
    def test_finite_chains_unfold_to_their_elements(self) -> None:
        self.assertEqual(list(B), [False, True])
        self.assertEqual(list(K3), [Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE])


class PstIndexingByPositionTest(unittest.TestCase):
    """C[i] is the element at position i from ⊥: pandas' .iloc on the enumeration."""

    def test_finite_chains_index_like_their_lists(self) -> None:
        self.assertEqual(len(B), 2)
        self.assertEqual(len(K3), 3)
        self.assertIs(B[0], False)
        self.assertIs(B[-1], True)
        self.assertEqual(K3[1], Ternary.UNKNOWN)
        self.assertEqual(K3[-1], Ternary.TRUE)
        self.assertEqual([K3[i] for i in range(len(K3))], list(K3))
        with self.assertRaises(IndexError):
            K3[3]


class PstClassificationTest(unittest.TestCase):
    def test_concept_verdicts(self) -> None:
        for chain in (B, K3):
            self.assertTrue(chain.is_bounded)
            self.assertFalse(chain.is_dense)  # a chain with the step is discrete
            self.assertTrue(chain.saturates)
        self.assertTrue(B.is_truth_object)
        self.assertTrue(K3.is_truth_object)

    def test_cardinalities(self) -> None:
        self.assertEqual(B.cardinality, 2)
        self.assertEqual(K3.cardinality, 3)

    def test_written_forms_are_the_names(self) -> None:
        self.assertEqual(repr(𝔹), "𝔹")
        self.assertEqual(repr(K3), "K₃")

    def test_a_universe_is_a_former_of_its_carrier_type(self) -> None:
        self.assertEqual(A(bool), B)
        self.assertEqual(A(Ternary), K3)
        with self.assertRaises(TypeError):
            A(K3)  # a universe, not a carrier type


class AUniverseIsItsFullRunList(unittest.TestCase):
    """A universe answers as the set it is: every operation, query and piece
    of metadata of the full run list, against any operand."""

    def test_the_lattice_against_a_k3_valued_set(self) -> None:
        H = K3.identity  # χ(x) = x, valued in K₃
        F, U, T = Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE
        self.assertEqual([(K3 & H)(v) for v in (F, U, T)], [F, U, T])  # H itself
        self.assertEqual((K3 | H) == K3, Ternary.TRUE)
        self.assertFalse((K3 ^ H).is_decidable)

    def test_subset_queries_between_universes(self) -> None:
        self.assertIs(𝔹 <= 𝔹, True)
        self.assertIs(K3 >= K3, True)
        self.assertIs(K3 <= (K3 | (π > Ternary.FALSE)), False)

    def test_applied_to_a_term(self) -> None:
        self.assertEqual(K3 | K3(π), K3)

    def test_its_metadata(self) -> None:
        self.assertIs(K3.is_decidable, True)
        self.assertEqual(K3.carrier, K3)
        self.assertIsInstance(K3, Set[K3])
        self.assertIs(Function[K3, 𝔹](K3), K3)
