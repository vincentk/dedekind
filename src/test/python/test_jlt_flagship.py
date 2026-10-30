"""The flagship of the Jlt paper (#1104), a first attempt, from Python.

Five periodic vectors of 2**60 entries, their outer product of 2**300 entries,
and its count over the box of all indices: by distributivity the product of the
factors' counts, exact in Python's integers, with its residue mod 2**64.  The
same exhibit as the C++ test ``flagship_test.cpp``; it must answer at once.
"""

import time
import unittest

from dedekind.jlt import count_factors, fib_even, is_even

N = 2**60


class JltFlagshipTest(unittest.TestCase):
    def test_the_box_counts_at_once(self) -> None:
        evens, odds, fibs = is_even(N), ~is_even(N), fib_even(N)
        start = time.perf_counter()
        box = evens * odds * fibs * evens * fibs
        factors = count_factors(box)
        exact = 1
        for f in factors:
            exact *= f
        elapsed = time.perf_counter() - start
        self.assertEqual(
            factors, [N // 2, N // 2, N // 3 + 1, N // 2, N // 3 + 1])
        self.assertEqual(exact, (N // 2) ** 3 * (N // 3 + 1) ** 2)
        self.assertGreater(exact.bit_length(), 128)  # beyond 128 bits
        self.assertLess(elapsed, 0.1)  # the size of the box costs nothing


if __name__ == "__main__":
    unittest.main()
