"""The canonical ring and field (#1019): u64 = ℤ/2⁶⁴ and 𝔽ₚ, p = 2⁶⁴ − 59.

Each is as large as a machine word allows.  The ring wraps and divides only by
a nonzero word with remainder; the field divides by any nonzero element and has
no order.  An int enters u64 by the checked cast and 𝔽ₚ by the quotient map;
𝔽ₚ shows its symmetric representative.  Which is a field is the C++ concepts'
verdict, not a Python flag.
"""

import unittest

from dedekind.pst import A, u64, 𝔽ₚ  # 𝔽ₚ is the name Fp (NFKC)

P = 2**64 - 59


class TheRing(unittest.TestCase):
    def test_it_wraps(self) -> None:
        self.assertEqual(u64(2**64 - 1) + 1, u64(0))
        self.assertEqual(u64(0) - 1, u64(2**64 - 1))
        self.assertEqual(u64(2**32) * u64(2**32), u64(0))
        self.assertEqual(-u64(1), u64(2**64 - 1))
        self.assertEqual(int(u64(5)), 5)
        self.assertEqual(repr(u64(5)), "u64(5)")

    def test_it_divides_with_remainder_by_a_nonzero_word(self) -> None:
        self.assertEqual(u64(7) // 2, u64(3))
        self.assertEqual(u64(7) % 2, u64(1))
        with self.assertRaises(ZeroDivisionError):
            u64(7) // 0
        with self.assertRaises(TypeError):
            u64(7) / 2  # 2 is not a unit of ℤ/2⁶⁴

    def test_an_int_enters_by_the_checked_cast(self) -> None:
        with self.assertRaises(ValueError):
            u64(-1)
        with self.assertRaises(ValueError):
            u64(2**64)
        with self.assertRaises(TypeError):
            u64("5")

    def test_it_equals_the_int_it_represents(self) -> None:
        self.assertEqual(u64(5), 5)
        self.assertNotEqual(u64(5), 2**64 + 5)
        self.assertEqual(hash(u64(5)), hash(5))
        self.assertEqual({u64(5), 5}, {5})

    def test_it_is_ordered_by_its_representatives(self) -> None:
        self.assertLess(u64(3), u64(5))
        self.assertGreater(u64(2**64 - 1), u64(0))


class TheField(unittest.TestCase):
    def test_it_divides_by_a_nonzero_element(self) -> None:
        third = 𝔽ₚ(1) / 3
        self.assertEqual(third * 3, 𝔽ₚ(1))
        self.assertEqual(𝔽ₚ(3).inverse(), third)
        self.assertEqual(1 / 𝔽ₚ(3), third)
        with self.assertRaises(ZeroDivisionError):
            𝔽ₚ(1) / 0
        with self.assertRaises(ZeroDivisionError):
            𝔽ₚ(0).inverse()

    def test_an_int_enters_by_the_quotient_map(self) -> None:
        self.assertEqual(𝔽ₚ(P + 2), 𝔽ₚ(2))
        self.assertEqual(𝔽ₚ(-1), 𝔽ₚ(P - 1))
        self.assertEqual(𝔽ₚ(-1) * 𝔽ₚ(-1), 𝔽ₚ(1))

    def test_it_shows_its_symmetric_representative(self) -> None:
        self.assertEqual(int(𝔽ₚ(-1)), -1)
        self.assertEqual(int(𝔽ₚ(P - 2)), -2)
        self.assertEqual(int(𝔽ₚ((P - 1) // 2)), (P - 1) // 2)
        self.assertEqual(int(𝔽ₚ((P + 1) // 2)), -((P - 1) // 2))
        self.assertEqual(repr(-𝔽ₚ(3)), "𝔽ₚ(-3)")

    def test_it_equals_the_int_it_represents(self) -> None:
        self.assertEqual(𝔽ₚ(-1), -1)
        self.assertNotEqual(𝔽ₚ(-1), P - 1)  # the quotient map is not equality
        self.assertEqual(hash(𝔽ₚ(-1)), hash(-1))
        self.assertEqual({𝔽ₚ(2), 2, 𝔽ₚ(P + 2)}, {2})

    def test_it_has_no_order(self) -> None:
        with self.assertRaises(TypeError):
            𝔽ₚ(1) < 𝔽ₚ(2)  # noqa: B015 — the comparison is the point


class TheUniverses(unittest.TestCase):
    def test_the_concepts_classify_them(self) -> None:
        self.assertTrue(u64.is_ring)
        self.assertFalse(u64.is_field)
        self.assertTrue(𝔽ₚ.is_ring)
        self.assertTrue(𝔽ₚ.is_field)
        self.assertEqual(u64.modulus, 2**64)
        self.assertEqual(𝔽ₚ.modulus, P)

    def test_membership_is_by_representative(self) -> None:
        R, F = A(u64), A(𝔽ₚ)
        self.assertIn(5, R)
        self.assertIn(u64(5), R)
        self.assertNotIn(-1, R)
        self.assertNotIn(2**64, R)
        self.assertIn(-1, F)
        self.assertIn(𝔽ₚ(7), F)
        self.assertNotIn(P - 1, F)  # its representative is -1
        self.assertFalse(R("5"))
        self.assertEqual(R.cardinality, 2**64)
        self.assertEqual(F.cardinality, P)
        self.assertIs(R.carrier, u64)
        self.assertIs(F.carrier, 𝔽ₚ)
        self.assertEqual(repr(R), "𝔸(u64)")
        self.assertEqual(repr(F), "𝔸(𝔽ₚ)")


if __name__ == "__main__":
    unittest.main()
