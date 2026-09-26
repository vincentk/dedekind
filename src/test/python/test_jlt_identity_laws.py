"""Jlt arrow DSL (#961), first iteration: the unary boolean operations.

The Python side is a fluent DSL over the *real* category arrows, duck-typed:
an "arrow" is the protocol ``__call__`` / ``__rshift__`` / ``dom`` / ``cod``.
Arrows are extensional (functions), so these tests pin (a) the protocol
conformance across the distinct arrow types and (b) the monoid/group laws
holding *behaviourally* -- the identity laws ``id∘f = f = f∘id`` and, since
``not`` is an involution, ``not∘not = id``.  Structural ``simplify`` is
intensional and lives on the C++ side, so it is absent here by design.
"""

import unittest

from dedekind.jlt import Identity, Morphism, id, not_

_BOOLS = (False, True)
_ARROW_METHODS = ("__call__", "__rshift__", "dom", "cod")


class JltArrowProtocolTest(unittest.TestCase):
    def test_arrows_duck_type_the_protocol(self) -> None:
        # id, not_, and a composite all satisfy the arrow protocol -- no shared
        # base class; "arrow" is the protocol (IsArrow is witnessed in C++).
        for arrow in (id, not_, id >> not_, not_ >> not_):
            for method in _ARROW_METHODS:
                self.assertTrue(hasattr(arrow, method), (arrow, method))

    def test_primitive_types_are_the_real_arrows(self) -> None:
        # id is the category Identity; not_ is a type-erased Morphism.
        self.assertIsInstance(id, Identity)
        self.assertIsInstance(not_, Morphism)

    def test_dom_cod_are_the_boolean_type(self) -> None:
        # numpy/pandas style: dom/cod are type objects (the boolean type).
        for arrow in (id, not_, id >> not_):
            self.assertIs(arrow.dom(), bool)
            self.assertIs(arrow.cod(), bool)

    def test_composition_type_erases_to_a_morphism(self) -> None:
        # Composition is extensional: it lands in the type-erased Morphism.
        self.assertIsInstance(id >> not_, Morphism)
        self.assertIsInstance(not_ >> id, Morphism)
        self.assertIsInstance(not_ >> not_, Morphism)


class JltMonoidLawsExtensionalTest(unittest.TestCase):
    """The monoid/group laws hold behaviourally on {False, True}."""

    def test_apply(self) -> None:
        self.assertIs(id(True), True)
        self.assertIs(not_(False), True)
        self.assertIs(not_(True), False)

    def test_left_and_right_identity(self) -> None:
        # id ∘ f = f = f ∘ id, extensionally.
        for x in _BOOLS:
            self.assertEqual((id >> not_)(x), not_(x))
            self.assertEqual((not_ >> id)(x), not_(x))
            self.assertEqual((id >> id)(x), id(x))

    def test_not_is_an_involution(self) -> None:
        # not ∘ not = id, extensionally (the Z/2 group law).
        for x in _BOOLS:
            self.assertEqual((not_ >> not_)(x), x)

    def test_repr(self) -> None:
        self.assertEqual(repr(id), "id")


if __name__ == "__main__":
    unittest.main()
