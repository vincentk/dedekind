"""The tensor exhibit of the Jlt paper (#1107), from Python.

A genome, the words shorter than 100 over {A, C, G, T}, is no sequence a word
can index: about 2**200 of them, refused.  As a tensor it is counted, factor by
factor, and its symmetries are merges of legs: the palindromes, the
reverse-complement palindromes (none of odd length), and, on the bit patterns,
the squares.  The handles are the core's; Python multiplies the exact factors
into its own integers, which is the only arithmetic it does.
"""

import math
import time
import unittest

from dedekind.jlt import (
    alphabet,
    count,
    count_factors,
    exact_count,
    free_monoid,
    is_involution,
    is_square,
    letter_map,
    pack,
    power,
    reverse,
    rotate,
    symmetric,
)


def total(tensor):
    """The exact count from the factors, in Python's integers."""
    return math.prod(base**exponent for base, exponent in count_factors(tensor))


class JltTensorExhibitTest(unittest.TestCase):
    nucleotides = alphabet("ACGT")
    bits = alphabet("01")
    complement = letter_map("ACGT", "TGCA")  # Watson–Crick pairing

    def test_the_genome_is_refused_as_a_sequence(self) -> None:
        self.assertIsNone(exact_count(free_monoid(self.nucleotides, 100)))
        self.assertEqual(exact_count(free_monoid(self.nucleotides, 4)), 1 + 4 + 16 + 64)

    def test_the_genome_is_counted_as_a_tensor(self) -> None:
        self.assertEqual(count_factors(power(self.nucleotides, 99)), [(4, 99)])
        palindromes = symmetric(power(self.nucleotides, 13), reverse)
        self.assertEqual(count_factors(palindromes), [(4, 6), (4, 1)])
        self.assertEqual(exact_count(palindromes), 4**7)
        sites = symmetric(
            power(self.nucleotides, 12), power(self.complement, 12) >> reverse
        )
        self.assertEqual(exact_count(sites), 4**6)
        odd = symmetric(
            power(self.nucleotides, 13), power(self.complement, 13) >> reverse
        )
        self.assertEqual(exact_count(odd), 0)  # no base is its own complement

    def test_the_reverse_complement_is_an_involution(self) -> None:
        self.assertTrue(is_involution(self.complement))  # decided on its table
        self.assertFalse(is_involution(letter_map("ACGT", "CGTA")))
        # derived: the complement and the reversal commute
        self.assertTrue(is_involution(power(self.complement, 6) >> reverse))
        with self.assertRaises(ValueError):
            symmetric(power(self.nucleotides, 12), power(self.complement, 6) >> reverse)

    def test_the_squares_of_128_bits(self) -> None:
        squares = symmetric(power(self.bits, 128), rotate(64))
        self.assertEqual(count_factors(squares), [(2, 64)])
        self.assertIsNone(exact_count(squares))  # one bit past the word
        self.assertEqual(total(squares), 2**64)
        self.assertTrue(is_square(0b1010, 4) and is_square(0, 128))
        self.assertFalse(is_square(0b1001, 4) or is_square(1, 128))
        self.assertEqual(pack(0x1A5, 8), 0xA5)

    def test_the_exhibit_answers_at_once(self) -> None:
        start = time.perf_counter()
        box = symmetric(power(self.nucleotides, 99), reverse) * symmetric(
            power(self.bits, 128), rotate(64)
        )
        factors = count_factors(box)
        exact = total(box)
        elapsed = time.perf_counter() - start
        self.assertEqual(factors, [(4, 49), (4, 1), (2, 64)])
        self.assertEqual(exact, 2 ** (100 + 64))
        self.assertIsNone(exact_count(box))
        self.assertEqual(count(box), 0)  # its residue modulo 2**64
        self.assertLess(elapsed, 0.1)  # the size of the box costs nothing


if __name__ == "__main__":
    unittest.main()
