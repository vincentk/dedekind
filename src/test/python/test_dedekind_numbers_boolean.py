"""External-API tests for ``dedekind.numbers.boolean`` (#400 carrier mirror).

These exercise the Python mirror of the C++ canonical-species spine.  Per
the issue, the test contract is "the same words read the same way" —
``𝔹`` is a carrier (``bool``), ``var(𝔹)`` is a symbolic scout, and
predicate / set-builder expressions over ``var(𝔹)`` behave the way they
do in C++.

The power-set stress test (suggested by the user during #400) walks the
power set ``2^𝔹 = {∅, {False}, {True}, {False, True}}`` and pins the
size of the "contains at least one true" and "contains at least one
false" subfamilies.
"""

import unittest

from dedekind.numbers.boolean import 𝔹, B, Variable, var
from dedekind.sets import 𝔓, power_set


class BooleanCarrierContractTest(unittest.TestCase):
    """The carrier symbol resolves to the Python primitive."""

    def test_𝔹_is_bool(self) -> None:
        self.assertIs(𝔹, bool)

    def test_B_is_bool(self) -> None:
        self.assertIs(B, bool)

    def test_𝔹_universe(self) -> None:
        # The whole carrier {False, True}.
        universe = frozenset({False, True})
        self.assertEqual(universe, {x for x in (False, True)})


class VariableSymbolicScoutTest(unittest.TestCase):
    """`var(𝔹)` is a symbolic scout over the bool carrier."""

    def test_var_returns_variable(self) -> None:
        b = var(𝔹)
        self.assertIsInstance(b, Variable)
        self.assertIs(b.domain, bool)

    def test_var_eq_builds_predicate(self) -> None:
        b = var(𝔹)
        truthy = (b == True)
        self.assertTrue(truthy(True))
        self.assertFalse(truthy(False))

    def test_var_not_builds_predicate(self) -> None:
        b = var(𝔹)
        falsy = b.not_()
        self.assertTrue(falsy(False))
        self.assertFalse(falsy(True))

    def test_var_invert_aliases_not(self) -> None:
        b = var(𝔹)
        self.assertEqual((~b)(False), b.not_()(False))
        self.assertEqual((~b)(True), b.not_()(True))


class PowerSetOfBooleansStressTest(unittest.TestCase):
    """Walk 2^𝔹 and pin the cardinality of predicate-filtered subfamilies.

    User suggestion (PR for #400): "all the elements of the power set of
    the booleans which contain at least one true (or false) value".

    For the carrier 𝔹 = {False, True}:

      * ``2^𝔹 = {∅, {False}, {True}, {False, True}}`` — 4 elements.
      * Subsets containing at least one ``True``:  ``{True}, {False, True}`` — 2 elements.
      * Subsets containing at least one ``False``: ``{False}, {False, True}`` — 2 elements.
      * Subsets containing both:                    ``{False, True}`` — 1 element.
      * Subsets containing neither (= ``∅``):       1 element.
    """

    def setUp(self) -> None:
        self.carrier = (False, True)
        self.powerset = 𝔓(self.carrier)
        # Sanity: |𝔓(𝔹)| = 2^|𝔹| = 4.
        self.assertEqual(len(self.powerset), 4)

    def test_𝔓_alias_is_power_set(self) -> None:
        self.assertIs(𝔓, power_set)

    def test_power_set_contains_canonical_subsets(self) -> None:
        self.assertIn(frozenset(), self.powerset)
        self.assertIn(frozenset({False}), self.powerset)
        self.assertIn(frozenset({True}), self.powerset)
        self.assertIn(frozenset({False, True}), self.powerset)

    def test_subsets_with_at_least_one_true(self) -> None:
        b = var(𝔹)
        truthy = (b == True)

        contains_true = [s for s in self.powerset if any(truthy(x) for x in s)]
        self.assertEqual(len(contains_true), 2)
        self.assertIn(frozenset({True}), contains_true)
        self.assertIn(frozenset({False, True}), contains_true)

    def test_subsets_with_at_least_one_false(self) -> None:
        b = var(𝔹)
        falsy = b.not_()

        contains_false = [s for s in self.powerset if any(falsy(x) for x in s)]
        self.assertEqual(len(contains_false), 2)
        self.assertIn(frozenset({False}), contains_false)
        self.assertIn(frozenset({False, True}), contains_false)

    def test_subsets_with_both_true_and_false(self) -> None:
        b = var(𝔹)
        truthy = (b == True)
        falsy = b.not_()

        contains_both = [
            s for s in self.powerset
            if any(truthy(x) for x in s) and any(falsy(x) for x in s)
        ]
        self.assertEqual(contains_both, [frozenset({False, True})])

    def test_partition_by_truthy_falsy_predicates(self) -> None:
        # The two predicates partition every subset into four cells:
        #   neither, truthy-only, falsy-only, both — and the cells sum to |2^𝔹|.
        b = var(𝔹)
        truthy = (b == True)
        falsy = b.not_()

        cells = {"neither": 0, "truthy_only": 0, "falsy_only": 0, "both": 0}
        for s in self.powerset:
            has_true = any(truthy(x) for x in s)
            has_false = any(falsy(x) for x in s)
            if has_true and has_false:
                cells["both"] += 1
            elif has_true:
                cells["truthy_only"] += 1
            elif has_false:
                cells["falsy_only"] += 1
            else:
                cells["neither"] += 1

        self.assertEqual(cells, {
            "neither": 1,      # ∅
            "truthy_only": 1,  # {True}
            "falsy_only": 1,   # {False}
            "both": 1,         # {False, True}
        })
        self.assertEqual(sum(cells.values()), len(self.powerset))


if __name__ == "__main__":
    unittest.main()
