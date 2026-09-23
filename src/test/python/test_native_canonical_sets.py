"""Vertical prototype (#886): the C++ canonical sets 𝔹 and ℕ, exposed natively
through nanobind, queried from Python.

The point is the *meat*, not the skin: membership runs the NATIVE characteristic
morphism χ from the C++ core, not a Python reimplementation.  The money shot is
``-7 not in ℕ``.  It is decided by the C++ ``NaturalNumbersOf`` classifier
(ℕ ⊂ ℤ, χ: x ↦ x ≥ 0) across the nanobind boundary.

NFKC note: ``ℕ`` / ``𝔹`` in Python source normalise to ``N`` / ``B`` (Python
normalises identifiers; the double-struck letters collapse to ASCII).  The
native attrs are keyed on the normalised form; the source stays mathy and the
``repr`` is mathy, but the identity is ASCII.
"""

import unittest

from dedekind._dedekind import 𝔹, ℕ  # source 𝔹→B, ℕ→N (NFKC) → native attrs


class NativeCanonicalSetsTest(unittest.TestCase):
    def test_boolean_universe_membership(self) -> None:
        # 𝔹 = 𝔸<bool>: both truth values are members of the universe.
        self.assertIn(True, 𝔹)
        self.assertIn(False, 𝔹)

    def test_naturals_native_classifier(self) -> None:
        # Membership decided by the native ℕ ⊂ ℤ classifier (χ: x ≥ 0).
        for n in (0, 1, 4, 100):
            self.assertIn(n, ℕ)
        self.assertNotIn(-7, ℕ)  # the money shot: -7 ∉ ℕ, decided in C++
        self.assertNotIn(-1, ℕ)

    def test_repr_is_mathy(self) -> None:
        self.assertEqual(repr(ℕ), "ℕ")
        self.assertEqual(repr(𝔹), "𝔹")


if __name__ == "__main__":
    unittest.main()
