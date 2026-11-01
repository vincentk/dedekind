"""The doctests of the Jlt facade: the paper's Listing 5 as Python runs it.

Run here, beside the unit tests, so ``unittest discover`` and ``pytest`` both
execute them; the docs will show the same examples with their output.
"""

import doctest
import unittest

import dedekind.jlt


class JltDoctests(unittest.TestCase):
    def test_the_facade_examples_hold(self) -> None:
        failures, tried = doctest.testmod(dedekind.jlt)
        self.assertGreater(tried, 0)
        self.assertEqual(failures, 0)


if __name__ == "__main__":
    unittest.main()
