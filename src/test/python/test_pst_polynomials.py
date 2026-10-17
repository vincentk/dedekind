"""The ring's normal form (#1023): a set in, the same set out.

canonical(S) rewrites S's predicate atom by atom: an equation a == b on a
ring becomes a - b == 0 with the difference a polynomial over the carrier
(monic on a field), any other atom keeps its comparison with its sides
normalised.  The variables are the set's own: π on A(C), π_1 and π_2 on
A(C) * A(C).

Equal normal forms cut out equal sets; different ones need not cut out
different sets (π**p == π holds everywhere on 𝔽ₚ), so == answers True or
raises TypeError, never a False it cannot back.  //, % and a division by a term
have no normal form and are refused.
"""

import unittest

from dedekind.pst import A, canonical, u64, π, π_1, π_2, 𝔽ₚ  # 𝔽ₚ is Fp (NFKC)

P = 2**64 - 59
U, F = A(u64), A(𝔽ₚ)
UU, FF = U * U, F * F


class ASetInTheSameSetOut(unittest.TestCase):
    def test_an_equation_becomes_a_polynomial_equal_to_zero(self) -> None:
        self.assertEqual(repr(canonical(UU & (π_1 * π_1 == π_2 - 1))),
                         "𝔸(u64) × 𝔸(u64) & ((((π_1 * π_1) + (18446744073709551615 * π_2)) + 1) == 0)")
        self.assertEqual(repr(canonical(U & (π + 1 == 1))), "𝔸(u64) & (π == 0)")

    def test_it_is_the_same_set(self) -> None:
        S = UU & ((π_1 + π_2) * (π_1 + π_2) == 4)
        N = canonical(S)
        for xy in ((0, 2), (1, 1), (2, 0), (3, 2**64 - 1), (1, 2)):
            self.assertEqual(xy in S, xy in N)

    def test_it_is_idempotent(self) -> None:
        S = FF & (π_1 * 2 == π_2 / 3)
        self.assertEqual(repr(canonical(canonical(S))), repr(canonical(S)))

    def test_the_chain_fragment_is_its_run_list(self) -> None:
        self.assertEqual(repr(canonical(U & (π + 0 < 5))), "𝔸(u64) & (π <= 4)")

    def test_beside_an_equation_an_order_atom_keeps_its_comparison(self) -> None:
        self.assertEqual(repr(canonical(U & ((π * π == 4) & (π + 0 < 5)))),
                         "𝔸(u64) & ((π < 5) & (((π * π) + 18446744073709551612) == 0))")

    def test_the_order_of_conjuncts_is_forgotten(self) -> None:
        # #884: a connective's operands are sorted and kept once, so == decides.
        either = U & ((π * π == 4) & (π < 5))
        self.assertIs(either == U & ((π < 5) & (π * π == 4)), True)
        self.assertIs(either == U & ((π < 5) & ((π * π == 4) & (π < 5))), True)
        self.assertIs(U & ((π * π == 4) | (π < 5)) == U & ((π < 5) | (π * π == 4)), True)


class EqualityByNormalForm(unittest.TestCase):
    def test_the_ring_laws_decide(self) -> None:
        self.assertEqual(UU & ((π_1 + π_2) * (π_1 + π_2) == π_2),
                         UU & (π_1 * π_1 + 2 * π_1 * π_2 + π_2 * π_2 == π_2))
        self.assertEqual(U & (π * (2**64 - 1) + π == 0), U & (0 * π == 0))

    def test_a_unit_factor_is_divided_out(self) -> None:
        self.assertEqual(U & (1 - π == 1), U & (π == 0))  # -1 is a unit
        self.assertEqual(U & (3 * π == 0), U & (π == 0))  # so is every odd word
        self.assertEqual(repr(canonical(U & (6 * π == 4))), "𝔸(u64) & (((2 * π) + 6148914691236517204) == 0)")
        with self.assertRaises(TypeError):
            U & (2 * π == 0) == U & (π == 0)  # 2 is no unit: not the same equation

    def test_a_field_makes_the_equation_monic(self) -> None:
        self.assertEqual(F & (2 * π == 4), F & (π == 2))
        self.assertEqual(FF & (π_1 * 3 / 3 == π_2), FF & (π_1 == π_2))

    def test_different_normal_forms_decide_nothing(self) -> None:
        with self.assertRaises(TypeError):
            F & (π * π == 0) == F & (π == 0)  # the same set, different forms
        with self.assertRaises(TypeError):
            UU & (π_1 == π_2) != UU & (π_1 == 0)

    def test_the_chain_still_decides_both_ways(self) -> None:
        self.assertNotEqual(U & (π < 5), U & (π < 6))
        self.assertEqual(U & (π < 5), (U & (π <= 4)))


class OutsideTheCatalogue(unittest.TestCase):
    def test_no_normal_form(self) -> None:
        for S in (U & (π // 2 == 1), U & (π % 2 == 0)):
            with self.assertRaises(TypeError):
                canonical(S)
        with self.assertRaises(TypeError):
            FF & (π_1 / π_2 == 1)  # refused when the set is built


if __name__ == "__main__":
    unittest.main()
