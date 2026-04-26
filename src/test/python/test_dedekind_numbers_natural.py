"""External-API tests for ``dedekind.numbers.natural`` (#401 carrier mirror)."""

import unittest

from dedekind.numbers.natural import ℕ, N_, Variable, var, successor


class NaturalCarrierContractTest(unittest.TestCase):
    """The carrier symbol resolves to the Python primitive."""

    def test_ℕ_is_int(self) -> None:
        self.assertIs(ℕ, int)

    def test_N_ascii_alias_is_int(self) -> None:
        self.assertIs(N_, int)

    def test_python_int_is_unbounded(self) -> None:
        # Python's int has unbounded magnitude — closer to the conceptual
        # ℕ ("infinite number line") than C++ unsigned int's modular ring.
        n = 2**1000
        self.assertEqual(n + 1 - n, 1)
        self.assertGreater(successor(n), n)


class NaturalVariableSurfaceTest(unittest.TestCase):
    """`var(ℕ)` is a symbolic scout over the int carrier."""

    def test_var_returns_variable(self) -> None:
        n = var(ℕ)
        self.assertIsInstance(n, Variable)
        self.assertIs(n.domain, int)

    def test_var_relational_predicates(self) -> None:
        n = var(ℕ)
        self.assertTrue((n > 5)(7))
        self.assertFalse((n > 5)(3))
        self.assertTrue((n <= 10)(10))
        self.assertFalse((n <= 10)(11))


class PeanoSuccessorTest(unittest.TestCase):
    """Successor is the Peano-flavoured carrier morphism on ℕ."""

    def test_successor_increments(self) -> None:
        self.assertEqual(successor(0), 1)
        self.assertEqual(successor(41), 42)

    def test_successor_chain(self) -> None:
        n = 0
        for _ in range(100):
            n = successor(n)
        self.assertEqual(n, 100)

    def test_successor_rejects_negatives(self) -> None:
        with self.assertRaises(ValueError):
            successor(-1)

    def test_successor_rejects_non_int(self) -> None:
        with self.assertRaises(TypeError):
            successor(1.5)
        with self.assertRaises(TypeError):
            successor("not a number")


class NaturalSemiringAxiomsTest(unittest.TestCase):
    """(ℕ, +, ·) is a commutative semiring (no additive inverse).

    Walks the axioms over a small witness window for an external observer
    to verify the structure runs.  Distinct from a ring witness because
    ℕ has no additive inverse: ``a + (−a) = 0`` is not generally
    realisable on ℕ.
    """

    def test_additive_monoid_axioms(self) -> None:
        zero = 0
        for a in range(5):
            self.assertEqual(a + zero, a)
            for b in range(5):
                self.assertEqual(a + b, b + a)
                for c in range(5):
                    self.assertEqual((a + b) + c, a + (b + c))

    def test_multiplicative_monoid_axioms(self) -> None:
        one = 1
        for a in range(5):
            self.assertEqual(a * one, a)
            for b in range(5):
                self.assertEqual(a * b, b * a)
                for c in range(5):
                    self.assertEqual((a * b) * c, a * (b * c))

    def test_distributivity(self) -> None:
        for a in range(5):
            for b in range(5):
                for c in range(5):
                    self.assertEqual(a * (b + c), a * b + a * c)

    def test_no_additive_inverse_in_ℕ(self) -> None:
        # Distinguishes ℕ from ℤ: no a > 0 has an inverse in ℕ.
        # Operationally, ``a - a = 0`` exits ℕ for any positive ``a``
        # only via the embedding ℕ ↪ ℤ; the witness here pins the
        # absence by checking that the closest unsigned-arithmetic
        # analog (``-1 mod 2^N``) doesn't land back in the small-window
        # ℕ-fragment we care about.
        for a in range(1, 5):
            # In Python, a - a = 0 ∈ ℕ — so the axiom holds for x = 0;
            # the structural absence is "no b ∈ ℕ with a + b = 0 except
            # b = 0 = a", which is exactly the cancellation property.
            self.assertEqual(a + 0, a)
            # And no positive b ∈ ℕ exists with a + b = 0.
            for b in range(1, 5):
                self.assertNotEqual(a + b, 0)


if __name__ == "__main__":
    unittest.main()
