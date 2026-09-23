"""Vertical prototype (#886): the C++ canonical sets 𝔹, ℕ and the ℕ⊂ℤ
classifier, exposed natively through nanobind and queried from Python.

The point is the *meat*, not the skin: membership runs the NATIVE characteristic
morphism χ from the C++ core, not a Python reimplementation.

Two shapes are bound, per the maintainer's option (b):

* ``ℕ`` is the ambient natural-numbers universe (``𝔸<Cardinality>``).  Its χ is
  universally true by the UniversalSet axiom, so ``4 in ℕ`` is True, exactly as
  ``True in 𝔹`` is.  ℕ is the universe, not a discriminator.
* ``Nat`` is the discriminating ℤ-subobject classifier (``NaturalNumbersOf``,
  χ: x ↦ x ≥ 0).  The money shot is ``-7 not in Nat``: it is decided in C++,
  across the nanobind boundary.

NFKC note: ``ℕ`` / ``𝔹`` in Python source normalise to ``N`` / ``B`` (Python
normalises identifiers; the double-struck letters collapse to ASCII).  Because
the ambient ``ℕ`` and the classifier would both normalise to ``N``, the ambient
universe is keyed ``N`` (reached as ``ℕ``) and the classifier is keyed ``Nat``.
"""

import unittest

from dedekind import 𝔹, ℕ, Nat, ext  # public re-export; 𝔹→B, ℕ→N (NFKC)


class NativeCanonicalSetsTest(unittest.TestCase):
    def test_boolean_universe_membership(self) -> None:
        # 𝔹 = 𝔸<bool>: both truth values are members of the universe.
        self.assertIn(True, 𝔹)
        self.assertIn(False, 𝔹)

    def test_naturals_ambient_universe(self) -> None:
        # ℕ is the ambient universe: χ_ℕ is universally true (4 in ℕ).
        for n in (0, 1, 4, 100):
            self.assertIn(n, ℕ)

    def test_naturals_native_classifier(self) -> None:
        # Membership decided by the native ℕ⊂ℤ classifier (χ: x ≥ 0).
        for n in (0, 1, 4, 100):
            self.assertIn(n, Nat)
        self.assertNotIn(-7, Nat)  # the money shot: -7 ∉ ℕ, decided in C++
        self.assertNotIn(-1, Nat)

    def test_ext_materialises_through_native_chi(self) -> None:
        # ext = the native retraction μ: Int ⇀ Ext.  A finite candidate
        # universe is filtered by the native ℕ⊂ℤ classifier χ, entirely in C++.
        self.assertEqual(ext([-2, -1, 0, 1, 2]), {0, 1, 2})
        self.assertEqual(ext([-7, -1]), set())

    def test_repr_is_mathy(self) -> None:
        self.assertEqual(repr(ℕ), "ℕ")
        self.assertEqual(repr(𝔹), "𝔹")
        self.assertEqual(repr(Nat), "ℕ⊂ℤ")


if __name__ == "__main__":
    unittest.main()
