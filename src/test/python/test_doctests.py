"""The doctests of every pynzli module: the examples the Python API reference
shows, with their output, executed.

Run here, beside the unit tests, so ``unittest discover`` and ``pytest`` both
execute them.  A module with no example is checked too: it has none to fail.
The facade, pynzli.jlt, must have some (the paper's Listing 5 as Python runs
it).
"""

import doctest
import unittest

import pynzli
import pynzli._jlt
import pynzli._pst
import pynzli.jlt
import pynzli.pst
import pynzli.trsk

MODULES = (pynzli, pynzli.jlt, pynzli.pst, pynzli.trsk, pynzli._jlt, pynzli._pst)


class Doctests(unittest.TestCase):
    def test_every_module_examples_hold(self) -> None:
        for module in MODULES:
            with self.subTest(module=module.__name__):
                failures, _ = doctest.testmod(module)
                self.assertEqual(failures, 0)

    def test_the_facade_has_examples(self) -> None:
        _, tried = doctest.testmod(pynzli.jlt)
        self.assertGreater(tried, 0)


if __name__ == "__main__":
    unittest.main()
