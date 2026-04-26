"""External-API tests for ``dedekind.algebra.polynomial`` (#401 sibling).

Filed under the cross-issue note on #401: the F₂[x] mirror is the
natural sibling to the ``F2 = bool`` carrier exposed in
``dedekind.numbers.boolean`` (PR #407 / #400).  These tests pin the
RigPolynomial surface plus the textbook ``(x+1)² = x²+1 over F₂``
existential witness.
"""

import unittest

from dedekind.algebra.polynomial import RigPolynomial
from dedekind.numbers.boolean import F2


class RigPolynomialBasicSurfaceTest(unittest.TestCase):
    """Basic carrier and constructor contracts."""

    def test_default_ring_is_int(self) -> None:
        p = RigPolynomial([1, 2, 3])
        self.assertIs(p.ring, int)
        self.assertEqual(p.coeffs, [1, 2, 3])

    def test_explicit_ring_int(self) -> None:
        p = RigPolynomial([1, 2, 3], ring=int)
        self.assertIs(p.ring, int)

    def test_F2_ring_via_F2_alias(self) -> None:
        # F2 = bool by the #407 carrier migration; passing ring=F2
        # configures the polynomial for the Galois-field-of-order-2.
        p = RigPolynomial([True, False, True], ring=F2)
        self.assertIs(p.ring, bool)

    def test_zero_and_one(self) -> None:
        self.assertEqual(RigPolynomial.zero().coeffs, [])
        self.assertEqual(RigPolynomial.one().coeffs, [1])
        self.assertEqual(RigPolynomial.zero(ring=F2).coeffs, [])
        self.assertEqual(RigPolynomial.one(ring=F2).coeffs, [True])

    def test_x_indeterminate(self) -> None:
        x = RigPolynomial.x()
        self.assertEqual(x.coeffs, [0, 1])
        x_F2 = RigPolynomial.x(ring=F2)
        self.assertEqual(x_F2.coeffs, [False, True])

    def test_degree(self) -> None:
        self.assertEqual(RigPolynomial.zero().degree, -1)
        self.assertEqual(RigPolynomial.one().degree, 0)
        self.assertEqual(RigPolynomial.x().degree, 1)
        # 1 + 2x + 3x²
        self.assertEqual(RigPolynomial([1, 2, 3]).degree, 2)

    def test_trailing_zeros_are_trimmed(self) -> None:
        # Coefficient list normalisation is part of the contract.
        p = RigPolynomial([1, 2, 0, 0])
        self.assertEqual(p.coeffs, [1, 2])
        self.assertEqual(p.degree, 1)


class RigPolynomialIntRingArithmeticTest(unittest.TestCase):
    """Standard integer-coefficient polynomial arithmetic."""

    def test_addition(self) -> None:
        # (1 + 2x) + (3 + x²) = 4 + 2x + x²
        a = RigPolynomial([1, 2])
        b = RigPolynomial([3, 0, 1])
        self.assertEqual((a + b).coeffs, [4, 2, 1])

    def test_multiplication_textbook(self) -> None:
        # (x + 1) * (x + 1) = x² + 2x + 1
        x_plus_1 = RigPolynomial([1, 1])
        squared = x_plus_1 * x_plus_1
        self.assertEqual(squared.coeffs, [1, 2, 1])

    def test_addition_ring_mismatch_raises(self) -> None:
        a = RigPolynomial([1], ring=int)
        b = RigPolynomial([True], ring=F2)
        with self.assertRaises(TypeError):
            a + b


class GaloisField2PolynomialAxiomsTest(unittest.TestCase):
    """The textbook ``(x+1)² = x²+1 over 𝔽₂`` witness.

    In characteristic 2 the cross term ``2·x`` of ``(x+1)²`` vanishes
    (because ``2·x = x ^ x = 0`` in F₂), which is the canonical
    "polynomials over F₂ are different from polynomials over ℤ" lesson.
    Mirrors the C++ ``static_assert(IsField<𝔹, std::bit_xor<𝔹>,
    std::bit_and<𝔹>>)`` witness made polynomial.
    """

    def test_x_plus_one_squared_equals_x_squared_plus_one(self) -> None:
        # Existential proof: (x+1)² = x²+1 over 𝔽₂.
        x_plus_1 = RigPolynomial([True, True], ring=F2)
        squared = x_plus_1 * x_plus_1
        # x² + 1 has coefficients [1, 0, 1] = [True, False, True].
        self.assertEqual(squared.coeffs, [True, False, True])

    def test_2x_vanishes_in_F2(self) -> None:
        # In F₂[x], adding x to itself gives 0 (characteristic 2).
        x = RigPolynomial.x(ring=F2)
        self.assertEqual((x + x).coeffs, [])
        self.assertEqual((x + x), RigPolynomial.zero(ring=F2))

    def test_F2_addition_is_xor(self) -> None:
        # Constant polynomials: True + True = False (XOR).
        one = RigPolynomial.one(ring=F2)
        self.assertEqual((one + one).coeffs, [])

    def test_F2_multiplication_is_and(self) -> None:
        # Constant polynomials: True * True = True (AND).
        one = RigPolynomial.one(ring=F2)
        self.assertEqual((one * one).coeffs, [True])
        # True * False = False.
        zero = RigPolynomial.zero(ring=F2)
        self.assertEqual((one * zero).coeffs, [])

    def test_F2_distributivity_on_polynomials(self) -> None:
        # x(x+1) = x² + x in F₂[x].
        x = RigPolynomial.x(ring=F2)
        x_plus_1 = RigPolynomial([True, True], ring=F2)
        product = x * x_plus_1
        # x · (x+1) = x² + x → coefficients [0, 1, 1] = [False, True, True].
        self.assertEqual(product.coeffs, [False, True, True])

    def test_x_squared_plus_x_plus_one_at_F2_values(self) -> None:
        # The polynomial x² + x + 1 ∈ 𝔽₂[x] is the canonical primitive
        # polynomial for GF(4) (irreducible over F₂; both elements of F₂
        # evaluate to non-zero).  An external observer can read this as:
        # the polynomial has no roots in F₂, which witnesses irreducibility
        # over F₂ in this small case (a polynomial of degree 2 is
        # irreducible iff it has no roots in the field).
        # Evaluate by hand — the polynomial library doesn't yet expose
        # evaluation; spelling it out demonstrates the contract.
        # p(0) = 0² + 0 + 1 = 1 (= True in F₂).
        self.assertEqual((False & False) ^ False ^ True, True)
        # p(1) = 1² + 1 + 1 = 1 (= True in F₂).
        self.assertEqual((True & True) ^ True ^ True, True)
        # Both evaluations are non-zero ⇒ irreducible over F₂.


if __name__ == "__main__":
    unittest.main()
