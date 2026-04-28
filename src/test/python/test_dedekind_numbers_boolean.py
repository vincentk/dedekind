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

from dedekind.numbers.boolean import 𝔹, B, F2, Variable, var
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


class GaloisField2OnBooleansTest(unittest.TestCase):
    """The (𝔹, ^, &) reading is the Galois field F2.

    Mirrors the C++ witness
    ``static_assert(IsField<𝔹, std::bit_xor<𝔹>, std::bit_and<𝔹>>)``
    in ``dedekind.numbers:boolean``: same carrier type as 𝔹, with
    addition := XOR and multiplication := AND.  Walks the field axioms
    explicitly over the two-element universe so an external observer can
    verify the structure by running the test.
    """

    def test_F2_alias(self) -> None:
        self.assertIs(F2, bool)
        self.assertIs(F2, bool)
        # Same carrier type — the Galois reading is a relabelling, not a
        # different type.
        self.assertIs(F2, 𝔹)

    def test_F2_python_operator_typing(self) -> None:
        # Python's bool is an int subclass; ``bool ^ bool`` and
        # ``bool & bool`` already return ``bool``, so the C++
        # ``std::bit_xor<𝔹>`` / ``std::bit_and<𝔹>`` functor parametrisation
        # has a zero-overhead Python mirror.
        for a in (False, True):
            for b in (False, True):
                self.assertIsInstance(a ^ b, bool)
                self.assertIsInstance(a & b, bool)

    def test_F2_additive_group_axioms(self) -> None:
        # (𝔹, ^, 0) is the additive abelian group: closure, identity,
        # inverse (every element is its own inverse — characteristic 2),
        # associativity, commutativity.
        zero = False
        for a in (False, True):
            self.assertEqual(a ^ zero, a)               # left identity
            self.assertEqual(zero ^ a, a)               # right identity
            self.assertEqual(a ^ a, zero)               # self-inverse (char 2)
            for b in (False, True):
                self.assertEqual(a ^ b, b ^ a)          # commutative
                for c in (False, True):
                    self.assertEqual((a ^ b) ^ c, a ^ (b ^ c))  # associative

    def test_F2_multiplicative_monoid_axioms(self) -> None:
        # (𝔹, &, 1) is a commutative monoid; the unit 1 is True.
        one = True
        for a in (False, True):
            self.assertEqual(a & one, a)                # left identity
            self.assertEqual(one & a, a)                # right identity
            for b in (False, True):
                self.assertEqual(a & b, b & a)          # commutative
                for c in (False, True):
                    self.assertEqual((a & b) & c, a & (b & c))  # associative

    def test_F2_distributivity(self) -> None:
        # & distributes over ^: a & (b ^ c) = (a & b) ^ (a & c).
        for a in (False, True):
            for b in (False, True):
                for c in (False, True):
                    self.assertEqual(a & (b ^ c), (a & b) ^ (a & c))

    def test_F2_multiplicative_inverses_for_non_zero(self) -> None:
        # In a field, every non-zero element has a multiplicative inverse.
        # F2 has exactly one non-zero element (True), and True is its own
        # inverse: True & True == True.
        self.assertEqual(True & True, True)

    def test_F2_no_zero_divisors(self) -> None:
        # Field ⇒ no zero divisors: a & b == 0 ⇒ a == 0 or b == 0.
        for a in (False, True):
            for b in (False, True):
                if (a & b) is False:
                    self.assertTrue(a is False or b is False)


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

    def test_var_does_not_overload_invert(self) -> None:
        # `~` is reserved for set complement project-wide; do NOT overload
        # it on Variable.  Use ``b.not_()`` for the bool-domain shorthand.
        b = var(𝔹)
        with self.assertRaises(TypeError):
            ~b  # noqa: B015 — intentional: this should fail.

    def test_var_relational_predicates_on_int_domain(self) -> None:
        # The Variable surface is domain-agnostic — exercise the lambda
        # bodies built by `__ne__` / `__lt__` / `__le__` / `__gt__` /
        # `__ge__` on an integer carrier so the predicate-builder paths
        # are actually invoked, not just constructed.
        n = var(int)
        self.assertTrue((n != 3)(2))
        self.assertFalse((n != 3)(3))
        self.assertTrue((n < 5)(4))
        self.assertFalse((n < 5)(5))
        self.assertTrue((n <= 5)(5))
        self.assertFalse((n <= 5)(6))
        self.assertTrue((n > 5)(6))
        self.assertFalse((n > 5)(5))
        self.assertTrue((n >= 5)(5))
        self.assertFalse((n >= 5)(4))

    def test_var_not_rejects_non_bool_domain(self) -> None:
        # `not_()` is the bool-domain shorthand; calling it on a non-bool
        # carrier should refuse rather than silently produce a
        # nonsensical predicate.
        n = var(int)
        with self.assertRaises(TypeError):
            n.not_()

    def test_var_repr_names_the_domain(self) -> None:
        self.assertEqual(repr(var(𝔹)), "var(bool)")
        self.assertEqual(repr(var(int)), "var(int)")


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
        # `𝔓` is a generator (lazy, mathematically unordered); materialise
        # to a list once for the cardinality / membership assertions below.
        self.powerset = list(𝔓(self.carrier))
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
