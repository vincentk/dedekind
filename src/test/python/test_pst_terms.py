"""The term surface (#1006, exhibit 3): a term names no carrier, the former
binds it, and every operator and constant is checked when the set is built.

The tree is the library's own term grammar with its shape a value; evaluation
and the checks are the same algebras the typed grammar uses.  What a carrier
admits is the C++ concepts' verdict: + − · where total, / on a field, // on
an ordered word, % where the carrier has it, each division by a nonzero
constant; order comparisons on an ordered carrier.
"""

import unittest

from dedekind.pst import A, B, K3, Ternary, ap, cond, u64, π, π_1, π_2, 𝔽ₚ

P = 2**64 - 59


class TermsAreCheckedWhenBuilt(unittest.TestCase):
    def test_a_division_by_the_constant_zero(self) -> None:
        for bad in (lambda: π / 0, lambda: π // 0, lambda: π % 0, lambda: π_1 / 𝔽ₚ(0)):
            with self.assertRaises(ValueError):
                bad()

    def test_a_constant_is_a_scalar(self) -> None:
        with self.assertRaises(TypeError):
            π + "1"
        with self.assertRaises(TypeError):
            π == [1]

    def test_the_projections_are_one_family(self) -> None:
        self.assertEqual(repr(π[1]), "π_1")
        self.assertEqual(repr(π[2] + π[1]), repr(π_2 + π_1))
        self.assertEqual(repr(π[0]), "π")
        with self.assertRaises(TypeError):
            π_1[2]  # the family is indexed from π

    def test_mccarthys_conditional(self) -> None:
        U = A(u64)
        step = U * U & (cond(π_1 % 2 == 0, π_1 // 2, π_1 * 3 + 1) == π_2)  # Collatz
        self.assertIn((6, 3), step)
        self.assertIn((7, 22), step)
        self.assertNotIn((7, 3), step)
        self.assertEqual(repr(cond(π == 0, 1, 2)), "cond(π == 0, 1, 2)")

    def test_the_spelling(self) -> None:
        self.assertEqual(repr(π_1 * 3 + 1 == π_2), "((π_1 * 3) + 1) == π_2")
        self.assertEqual(repr((π > 2) & (π < 5)), "(π > 2) & (π < 5)")
        self.assertEqual(repr(~((π > 2) & (π < 5))), "(π <= 2) | (π >= 5)")
        self.assertEqual(repr(-π), "(0 - π)")
        self.assertEqual(repr(1 - π), "(1 - π)")



class TheFilterIsTheMeet(unittest.TestCase):
    """#826: S & pred is the former; | is the join of two sets only."""

    def test_the_former_is_spelled_with_and(self) -> None:
        U = A(u64)
        self.assertIn(5, U & (π > 3))
        for S, pred in ((U, π > 3), (K3, π > Ternary.FALSE), (U * U, π_1 > 3)):
            with self.assertRaisesRegex(TypeError, "the filter is the meet"):
                S | pred

    def test_a_term_is_no_filter(self) -> None:
        U = A(u64)
        with self.assertRaisesRegex(TypeError, "compare it"):
            U * U & (π_1 + 1)

    def test_the_join_stays(self) -> None:
        U = A(u64)
        self.assertIn(7, (U & (π < 3)) | (U & (π > 6)))


class TheRingAdmits(unittest.TestCase):
    def test_wrapping_arithmetic_and_euclidean_division(self) -> None:
        U = A(u64)
        self.assertNotIn(0, U & (π + 1 == 0))
        self.assertIn(2**64 - 1, U & (π + 1 == 0))  # it wraps
        self.assertIn(4, U & (π % 2 == 0))
        self.assertNotIn(5, U & (π % 2 == 0))
        self.assertIn(7, U & (π // 2 == 3))
        self.assertIn(3, U & (π * 3 - 1 == 8))
        self.assertIn(2**63, U & (π * 2 == 0))  # 2⁶³ · 2 wraps to 0

    def test_refusals(self) -> None:
        U = A(u64)
        with self.assertRaises(TypeError):
            U & (π / 2 == 1)  # 2 is not a unit of ℤ/2⁶⁴
        with self.assertRaises(TypeError):
            U * U & (π_2 == π_2 // π_1)  # a term divisor could be 0
        with self.assertRaises(TypeError):
            U * U & (π_2 == π_2 % π_1)
        with self.assertRaises(ValueError):
            U & (π > -1)  # the word cannot hold -1
        with self.assertRaises(ValueError):
            U & (π == 2**64)
        with self.assertRaises(TypeError):
            U & (π_1 > 1)  # π_1 names a pair's coordinate
        with self.assertRaises(TypeError):
            U * U & (π > 1)  # π names an element

    def test_order(self) -> None:
        U = A(u64)
        small = U & (π < 10)
        self.assertIn(9, small)
        self.assertNotIn(10, small)
        self.assertNotIn(-1, small)  # not a representative: not a member
        self.assertIn(5, U & (3 < π))  # reflected


class TheFieldAdmits(unittest.TestCase):
    def test_division_by_a_nonzero_constant(self) -> None:
        F = A(𝔽ₚ)
        third = F * F & (π_2 == π_1 / 3)
        self.assertTrue(ap(third, (3, 1)))
        self.assertTrue(ap(third, (1, int(𝔽ₚ(1) / 3))))
        self.assertFalse(ap(third, (3, 2)))
        self.assertIn((3, 1), third)
        self.assertNotIn((3, 2), third)
        self.assertNotIn("x", third)

    def test_the_quotient_map_on_constants(self) -> None:
        F = A(𝔽ₚ)
        self.assertIn(2, F & (π == P + 2))
        self.assertIn(-1, F & (π == P - 1))
        self.assertIn(-1, F & (π * π == 1))
        self.assertIn(𝔽ₚ(5), F & (π - 𝔽ₚ(5) == 0))

    def test_a_point_is_an_element_or_its_representative(self) -> None:
        # Constants enter by the quotient map; points, like `in A(𝔽ₚ)`, are
        # elements or symmetric representatives: p - 1 names none.
        minus_one = A(𝔽ₚ) & (π == -1)
        self.assertIn(-1, minus_one)
        self.assertIn(𝔽ₚ(P - 1), minus_one)
        self.assertNotIn(P - 1, minus_one)
        with self.assertRaises(TypeError):
            minus_one(P - 1)

    def test_refusals(self) -> None:
        F = A(𝔽ₚ)
        with self.assertRaises(TypeError):
            F * F & (π_2 == π_1 / π_2)  # a term divisor could be 0
        with self.assertRaises(TypeError):
            F & (π < 3)  # no order
        with self.assertRaises(TypeError):
            F & (π % 3 == 0)  # % is not a field operation
        with self.assertRaises(TypeError):
            F & (π // 3 == 0)  # nor is Euclidean division
        with self.assertRaises(ValueError):
            F & (π / P == 1)  # p is 0 in 𝔽ₚ: refused when bound


class TheSetsCombine(unittest.TestCase):
    def test_lattice_on_sets_and_relations(self) -> None:
        U = A(u64)
        window = (U & (π > 2)) & (U & (π < 5))
        self.assertIn(3, window)
        self.assertNotIn(5, window)
        outside = ~window
        self.assertIn(5, outside)
        self.assertNotIn(3, outside)
        self.assertIn(7, (U & (π < 3)) | (U & (π > 6)))
        self.assertEqual(repr(window), "𝔸(u64) & ((π > 2) & (π < 5))")
        self.assertIs(window.universe.carrier, u64)
        succ = U * U & (π_2 == π_1 + 1)
        self.assertIn((4, 5), succ)
        self.assertNotIn((4, 6), succ)
        self.assertIn((2**64 - 1, 0), succ)  # it wraps
        self.assertIn((4, 6), ~succ)
        self.assertIn((1, 2), succ & (U * U & (π_1 < 3)))
        self.assertEqual(repr(U * U), "𝔸(u64) × 𝔸(u64)")
        self.assertIn((1, 2), U * U)
        self.assertNotIn((1, -2), U * U)

    def test_predicates_combine_before_binding(self) -> None:
        U = A(u64)
        self.assertIn(3, U & ((π > 2) & (π < 5)))
        self.assertIn(5, U & ~((π > 2) & (π < 5)))


class OnTheTruthChains(unittest.TestCase):
    def test_a_term_is_read_at_the_chain_elements(self) -> None:
        self.assertEqual(repr(B & (π != True)), "{⊥}")  # noqa: E712
        U = Ternary.UNKNOWN
        self.assertEqual(repr(K3 & (π != U)), "{⊥} ∪ {⊤}")
        with self.assertRaises(TypeError):
            B & (π + 1 == True)  # noqa: E712 (𝔹's rig is (∨, ∧): no +)
        with self.assertRaises(TypeError):
            B & (π == 1)  # a constant on 𝔹 is a bool


if __name__ == "__main__":
    unittest.main()
