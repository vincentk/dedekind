"""Jlt arrow DSL (#961): involutive endomorphisms over two objects (bool, int).

The Python side is a fluent DSL over the *real* category arrows, duck-typed: an
"arrow" is the protocol ``__call__`` / ``__rshift__`` plus the free functions
``dom`` / ``cod``.  The primitive arrows on a carrier ``T`` are ``id(T)`` and
``refl(T)`` (the reflection involution).  These tests pin: (a) the protocol and
``dom``/``cod`` as type objects, (b) the monoid/group laws holding
*extensionally* per carrier (``id∘f = f = f∘id`` and ``refl∘refl = id``), and
(c) composability -- composing across objects (bool vs int) raises.  Structural
``simplify`` is intensional and lives on the C++ side, so it is absent here.
"""

import unittest

from dedekind.jlt import cod, dom, id, refl

_ARROW_OPERATORS = ("__call__", "__rshift__")


class JltArrowProtocolTest(unittest.TestCase):
    def test_arrows_duck_type_the_protocol(self) -> None:
        # id(T), refl(T), and composites all satisfy the arrow protocol -- no
        # shared base class; "arrow" is the protocol (IsArrow witnessed in C++).
        arrows = (
            id(bool),
            refl(bool),
            id(bool) >> refl(bool),
            id(int),
            refl(int),
            refl(int) >> refl(int),
        )
        for arrow in arrows:
            for op in _ARROW_OPERATORS:
                self.assertTrue(hasattr(arrow, op), (arrow, op))

    def test_dom_cod_are_the_carrier_type(self) -> None:
        # Free-function accessors; numpy/pandas style, dom/cod are type objects.
        for arrow in (id(bool), refl(bool), id(bool) >> refl(bool)):
            self.assertIs(dom(arrow), bool)
            self.assertIs(cod(arrow), bool)
        for arrow in (id(int), refl(int), refl(int) >> refl(int)):
            self.assertIs(dom(arrow), int)
            self.assertIs(cod(arrow), int)

    def test_bad_carrier_raises(self) -> None:
        with self.assertRaises(TypeError):
            id(str)


class JltGroupLawsExtensionalTest(unittest.TestCase):
    """The monoid/group laws hold behaviourally on each carrier."""

    def test_apply_bool(self) -> None:
        self.assertIs(id(bool)(True), True)
        self.assertIs(refl(bool)(False), True)
        self.assertIs(refl(bool)(True), False)

    def test_apply_int(self) -> None:
        self.assertEqual(id(int)(7), 7)
        self.assertEqual(refl(int)(7), -7)
        self.assertEqual(refl(int)(-3), 3)

    def test_identity_laws(self) -> None:
        # id ∘ f = f = f ∘ id, extensionally, per carrier.
        for x in (False, True):
            self.assertEqual((id(bool) >> refl(bool))(x), refl(bool)(x))
            self.assertEqual((refl(bool) >> id(bool))(x), refl(bool)(x))
        for n in (-2, 0, 5):
            self.assertEqual((id(int) >> refl(int))(n), refl(int)(n))
            self.assertEqual((refl(int) >> id(int))(n), refl(int)(n))

    def test_refl_is_an_involution(self) -> None:
        # refl ∘ refl = id, extensionally (the Z/2 group law), per carrier.
        for x in (False, True):
            self.assertEqual((refl(bool) >> refl(bool))(x), x)
        for n in (-4, 0, 9):
            self.assertEqual((refl(int) >> refl(int))(n), n)


class JltComposabilityTest(unittest.TestCase):
    """cod(f) == dom(g) is enforced structurally: cross-object >> raises."""

    def test_cross_object_composition_raises(self) -> None:
        # bool-arrow >> int-arrow is not defined (cod=bool != dom=int).
        with self.assertRaises(TypeError):
            id(bool) >> refl(int)
        with self.assertRaises(TypeError):
            refl(int) >> id(bool)

    def test_same_object_composition_is_fine(self) -> None:
        self.assertIs(dom(id(bool) >> refl(bool)), bool)
        self.assertIs(dom(id(int) >> refl(int)), int)


if __name__ == "__main__":
    unittest.main()
