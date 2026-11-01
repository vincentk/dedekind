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
    cylinder,
    exact_count,
    exact_occurrences,
    free_monoid,
    is_involution,
    is_square,
    letter_map,
    occurrences,
    pack,
    power,
    reverse,
    rotate,
    symmetric,
    window,
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
            power(self.nucleotides, 12), power(self.complement) >> reverse
        )
        self.assertEqual(exact_count(sites), 4**6)
        odd = symmetric(
            power(self.nucleotides, 13), power(self.complement) >> reverse
        )
        self.assertEqual(exact_count(odd), 0)  # no base is its own complement

    def test_a_site_in_a_window_is_pushed_down(self) -> None:
        sites = symmetric(power(self.nucleotides, 6), power(self.complement) >> reverse)
        genomes = power(self.nucleotides, 12)
        # the window's legs merged through the reverse complement, the rest free
        self.assertEqual(count_factors(cylinder(genomes, window(3, 6), sites)), [(4, 3), (4, 6)])
        # wherever the window sits; empty past the end
        self.assertEqual(
            count_factors(cylinder(genomes, window(0, 6), sites)),
            count_factors(cylinder(genomes, window(6, 6), sites)),
        )
        self.assertEqual(exact_count(cylinder(genomes, window(7, 6), sites)), 0)
        self.assertEqual(occurrences(genomes, sites), 7 * 4**3 * 4**6)
        # past the word, refused exactly; Python's integers hold it
        long = power(self.nucleotides, 40)
        self.assertIsNone(exact_occurrences(long, sites))
        self.assertEqual(
            sum(total(cylinder(long, window(i, 6), sites)) for i in range(35)),
            35 * 4**3 * 4**34,
        )

    def test_a_power_multiplies_on_either_side(self) -> None:
        a = self.nucleotides
        self.assertEqual(count_factors(power(a, 2) * power(a, 3)), [(4, 2), (4, 3)])
        palindromes = symmetric(power(a, 3), reverse)
        self.assertEqual(
            count_factors(power(a, 2) * palindromes),
            count_factors(power(a, 2)) + count_factors(palindromes),
        )
        self.assertEqual(exact_count(free_monoid(a, 1_000_000_000)), None)

    def test_the_reverse_complement_is_an_involution(self) -> None:
        self.assertTrue(is_involution(self.complement))  # decided on its table
        self.assertFalse(is_involution(letter_map("ACGT", "CGTA")))
        # derived: the complement and the reversal commute
        self.assertTrue(is_involution(power(self.complement) >> reverse))

    def test_the_squares_of_128_bits(self) -> None:
        squares = symmetric(power(self.bits, 128), rotate(64))
        self.assertEqual(count_factors(squares), [(2, 64)])
        self.assertIsNone(exact_count(squares))  # one bit past the word
        self.assertEqual(total(squares), 2**64)
        self.assertTrue(is_square(0b1010, 4) and is_square(0, 128))
        self.assertFalse(is_square(0b1001, 4) or is_square(1, 128))
        self.assertEqual(pack(0x1A5, 8), 0xA5)
        self.assertEqual(pack(0xFF, 4), 0xF)  # the low w bits, no padding
        self.assertTrue(is_square(0xAA, 4))  # 1010 above padding 1010
        # the letters are ordered as bytes: "é" is two bytes above "A", in UTF-8
        self.assertEqual(count_factors(power(alphabet("Aé"), 1)), [(3, 1)])

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
