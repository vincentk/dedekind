"""The term surface (#1006, exhibit 3): a term names no carrier, the former
binds it, and every operator and constant is checked when the set is built.

The tree is the library's own term grammar with its shape a value; evaluation
and the checks are the same algebras the typed grammar uses.  What a carrier
admits is the C++ concepts' verdict: + − · where total, / on a field, // on
an ordered word, % where the carrier has it, each division by a nonzero
constant; order comparisons on an ordered carrier.
"""

import unittest

from dedekind.pst import A, B, K3, Ternary, u64, π, π1, π2, 𝔽ₚ

P = 2**64 - 59


class TermsAreCheckedWhenBuilt(unittest.TestCase):
    def test_a_division_by_the_constant_zero(self) -> None:
        for bad in (lambda: π / 0, lambda: π // 0, lambda: π % 0, lambda: π1 / 𝔽ₚ(0)):
            with self.assertRaises(ValueError):
                bad()

    def test_a_constant_is_a_scalar(self) -> None:
        with self.assertRaises(TypeError):
            π + "1"
        with self.assertRaises(TypeError):
            π == [1]

    def test_the_spelling(self) -> None:
        self.assertEqual(repr(π1 * 3 + 1 == π2), "((π1 * 3) + 1) == π2")
        self.assertEqual(repr((π > 2) & (π < 5)), "(π > 2) & (π < 5)")
        self.assertEqual(repr(~((π > 2) & (π < 5))), "(π <= 2) | (π >= 5)")
        self.assertEqual(repr(-π), "(0 - π)")
        self.assertEqual(repr(1 - π), "(1 - π)")


class TheRingAdmits(unittest.TestCase):
    def test_wrapping_arithmetic_and_euclidean_division(self) -> None:
        U = A(u64)
        self.assertNotIn(0, U | (π + 1 == 0))
        self.assertIn(2**64 - 1, U | (π + 1 == 0))  # it wraps
        self.assertIn(4, U | (π % 2 == 0))
        self.assertNotIn(5, U | (π % 2 == 0))
        self.assertIn(7, U | (π // 2 == 3))
        self.assertIn(3, U | (π * 3 - 1 == 8))
        self.assertIn(2**63, U | (π * 2 == 0))  # 2⁶³ · 2 wraps to 0

    def test_refusals(self) -> None:
        U = A(u64)
        with self.assertRaises(TypeError):
            U | (π / 2 == 1)  # 2 is not a unit of ℤ/2⁶⁴
        with self.assertRaises(TypeError):
            U * U | (π2 == π2 // π1)  # a term divisor could be 0
        with self.assertRaises(TypeError):
            U * U | (π2 == π2 % π1)
        with self.assertRaises(ValueError):
            U | (π > -1)  # the word cannot hold -1
        with self.assertRaises(ValueError):
            U | (π == 2**64)
        with self.assertRaises(TypeError):
            U | (π1 > 1)  # π1 names a pair's coordinate
        with self.assertRaises(TypeError):
            U * U | (π > 1)  # π names an element

    def test_order(self) -> None:
        U = A(u64)
        small = U | (π < 10)
        self.assertIn(9, small)
        self.assertNotIn(10, small)
        self.assertNotIn(-1, small)  # not a representative: not a member
        self.assertIn(5, U | (3 < π))  # reflected


class TheFieldAdmits(unittest.TestCase):
    def test_division_by_a_nonzero_constant(self) -> None:
        F = A(𝔽ₚ)
        third = F * F | (π2 == π1 / 3)
        self.assertTrue(third((3, 1)))
        self.assertTrue(third((1, int(𝔽ₚ(1) / 3))))
        self.assertFalse(third((3, 2)))
        self.assertIn((3, 1), third)
        self.assertNotIn((3, 2), third)
        self.assertNotIn("x", third)

    def test_the_quotient_map_on_constants(self) -> None:
        F = A(𝔽ₚ)
        self.assertIn(2, F | (π == P + 2))
        self.assertIn(-1, F | (π == P - 1))
        self.assertIn(-1, F | (π * π == 1))
        self.assertIn(𝔽ₚ(5), F | (π - 𝔽ₚ(5) == 0))

    def test_refusals(self) -> None:
        F = A(𝔽ₚ)
        with self.assertRaises(TypeError):
            F * F | (π2 == π1 / π2)  # a term divisor could be 0
        with self.assertRaises(TypeError):
            F | (π < 3)  # no order
        with self.assertRaises(TypeError):
            F | (π % 3 == 0)  # % is not a field operation
        with self.assertRaises(TypeError):
            F | (π // 3 == 0)  # nor is Euclidean division


class TheSetsCombine(unittest.TestCase):
    def test_lattice_on_sets_and_relations(self) -> None:
        U = A(u64)
        window = (U | (π > 2)) & (U | (π < 5))
        self.assertIn(3, window)
        self.assertNotIn(5, window)
        outside = ~window
        self.assertIn(5, outside)
        self.assertNotIn(3, outside)
        self.assertIn(7, (U | (π < 3)) | (U | (π > 6)))
        self.assertEqual(repr(window), "𝔸(u64) | (π > 2) & (π < 5)")
        self.assertIs(window.universe.carrier, u64)
        succ = U * U | (π2 == π1 + 1)
        self.assertIn((4, 5), succ)
        self.assertNotIn((4, 6), succ)
        self.assertIn((2**64 - 1, 0), succ)  # it wraps
        self.assertIn((4, 6), ~succ)
        self.assertIn((1, 2), succ & (U * U | (π1 < 3)))
        self.assertEqual(repr(U * U), "𝔸(u64) × 𝔸(u64)")
        self.assertIn((1, 2), U * U)
        self.assertNotIn((1, -2), U * U)

    def test_predicates_combine_before_binding(self) -> None:
        U = A(u64)
        self.assertIn(3, U | ((π > 2) & (π < 5)))
        self.assertIn(5, U | ~((π > 2) & (π < 5)))


class OnTheTruthChains(unittest.TestCase):
    def test_a_term_is_read_at_the_chain_elements(self) -> None:
        self.assertEqual((A(B) | (π != True)).runs(), [(False, False)])  # noqa: E712
        U = Ternary.UNKNOWN
        self.assertEqual((A(K3) | (π != U)).runs(), [(Ternary.FALSE, Ternary.FALSE), (Ternary.TRUE, Ternary.TRUE)])
        with self.assertRaises(TypeError):
            A(B) | (π + 1 == True)  # noqa: E712 — 𝔹 is not closed under +
        with self.assertRaises(TypeError):
            A(B) | (π == 1)  # a constant on 𝔹 is a bool


if __name__ == "__main__":
    unittest.main()
