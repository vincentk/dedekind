"""The tensor exhibit of the Jlt paper (#1104), a first attempt, from Python.

Five periodic vectors of 2**60 entries, their outer product of 2**300 entries,
and its count over the box of all indices: by distributivity the product of the
factors' counts, exact in Python's integers, with its residue mod 2**64.  The
same exhibit as the C++ test ``tensor_exhibit_test.cpp``; it must answer at once.
"""

import time
import unittest

try:  # the exhibit's bindings follow the core's arrows (#1104)
    from dedekind.jlt import count, fib, is_even  # noqa: F401
except ImportError:
    count = fib = is_even = None

N = 2**60


class JltTensorExhibitTest(unittest.TestCase):
    # Python binds handles to the core's arrows, never a second copy of their
    # laws, so the test waits for those handles (#1104).
    @unittest.skipIf(count is None, "the exhibit's bindings are not in yet")
    def test_the_box_counts_at_once(self) -> None:
        evens, odds, fibs = is_even(N), ~is_even(N), fib(N)
        start = time.perf_counter()
        box = evens * odds * fibs * evens * fibs
        exact = count(box)
        elapsed = time.perf_counter() - start
        self.assertEqual(exact, (N // 2) ** 3 * (N // 3 + 1) ** 2)
        self.assertGreater(exact.bit_length(), 128)  # beyond 128 bits
        self.assertLess(elapsed, 0.1)  # the size of the box costs nothing


if __name__ == "__main__":
    unittest.main()
