"""The polynomial normal form (#1023): a term on the ring or the field, read in
the free algebra its laws give it.

The normal form is a polynomial over the carrier: the finite function monomial
↦ coefficient, and two terms are equal in the free algebra iff these finite
functions are.  The coefficients live on the carrier, so on u64 they wrap, and
a field's division by a nonzero constant is a coefficient.  //, % and a
division by a term have no polynomial and are refused.  Equality of polynomials
is not equality of the functions they induce on the finite carrier (πᵖ and π
agree at every point of 𝔽ₚ); that quotient is a further reading.
"""

import unittest

from dedekind.pst import A, u64, π, π_1, π_2, 𝔽ₚ  # 𝔽ₚ is the name Fp (NFKC)

P = 2**64 - 59
U, F = A(u64), A(𝔽ₚ)


class EqualityInTheFreeAlgebra(unittest.TestCase):
    def test_the_ring_laws_decide(self) -> None:
        self.assertEqual(U.normal_form(π + 1 - 1), U.normal_form(π))
        self.assertEqual(
            U.normal_form((π_1 + π_2) * (π_1 + π_2)),
            U.normal_form(π_1 * π_1 + 2 * π_1 * π_2 + π_2 * π_2),
        )
        self.assertNotEqual(U.normal_form(π * π), U.normal_form(π))

    def test_the_coefficients_wrap_with_the_word(self) -> None:
        self.assertEqual(U.normal_form(π * (2**64 - 1) + π), U.normal_form(0 * π))
        self.assertFalse(U.normal_form(π - π))
        self.assertTrue(U.normal_form(π))

    def test_a_fields_division_by_a_constant_is_a_coefficient(self) -> None:
        self.assertEqual(F.normal_form(π * 3 / 3), F.normal_form(π))
        self.assertEqual(F.normal_form(π / 3).support, [((1, 0, 0, 0), 𝔽ₚ(1) / 3)])
        with self.assertRaises(ValueError):
            F.normal_form(π / P)  # nonzero as written, 0 on the carrier


class TheFiniteFunction(unittest.TestCase):
    def test_support_and_degree(self) -> None:
        p = U.normal_form((π_1 + π_2) * (π_1 + π_2))
        self.assertEqual(p.degree, 2)
        # Sorted by degree, then by the slots: π_2² < π_1·π_2 < π_1².
        self.assertEqual(p.support, [((0, 0, 2, 0), 1), ((0, 1, 1, 0), 2), ((0, 2, 0, 0), 1)])
        self.assertEqual(U.normal_form(5).support, [((0, 0, 0, 0), 5)])
        self.assertEqual(U.normal_form(π - π).support, [])

    def test_it_is_written_as_its_pairs(self) -> None:
        self.assertEqual(
            repr(U.normal_form((π_1 + π_2) * (π_1 + π_2))), "{π_2² ↦ 1, π_1·π_2 ↦ 2, π_1² ↦ 1}"
        )
        self.assertEqual(repr(U.normal_form(π - π)), "{}")
        self.assertEqual(repr(U.normal_form(5)), "{1 ↦ 5}")
        self.assertEqual(repr(F.normal_form(0 - π)), "{π ↦ -1}")
        self.assertEqual(repr(U.normal_form(π * π * π * π * π * π * π * π * π * π * π)), "{π¹¹ ↦ 1}")


class OutsideTheCatalogue(unittest.TestCase):
    def test_no_polynomial(self) -> None:
        for term in (π // 2, π % 2, π / π, π / 2):  # the word has no field division
            with self.assertRaises(TypeError):
                U.normal_form(term)
        with self.assertRaises(TypeError):
            F.normal_form(π_1 / π_2)

    def test_a_constant_the_carrier_cannot_hold(self) -> None:
        with self.assertRaises(ValueError):
            U.normal_form(π + 2**64)


if __name__ == "__main__":
    unittest.main()
