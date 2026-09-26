"""Jlt identity-law exhibit (#961), first iteration: the unary boolean ops.

Objects are ``True`` / ``False``; the primitive arrows are ``id`` and ``not_``.
``simplify`` is the value-first ``cata`` (``ArrowTerm::reduce``), run in C++;
Python only holds handles.  These pin the monoid unit law ``id ∘ f = f = f ∘ id``,
the inert two-non-units case, and apply.  Scope is the identity laws only:
``not`` is opaque to the reducer, so ``not ∘ not`` stays inert for now (the
involution law is the next slice).
"""

import unittest

from dedekind.jlt import id, not_, simplify


class JltIdentityLawsTest(unittest.TestCase):
    def test_id_compose_id_is_id(self) -> None:
        # id ∘ id = id
        self.assertEqual(simplify(id >> id), id)

    def test_right_identity_drops(self) -> None:
        # not ∘ id = not
        self.assertEqual(simplify(not_ >> id), not_)

    def test_left_identity_drops(self) -> None:
        # id ∘ not = not
        self.assertEqual(simplify(id >> not_), not_)

    def test_two_non_units_stay_inert(self) -> None:
        # Neither leg is the unit, so the composite is its own normal form.
        # (not ∘ not = id is the involution law -- the NEXT slice, not yet.)
        self.assertEqual(repr(simplify(not_ >> not_)), "(>> not not)")

    def test_apply_to_boolean_objects(self) -> None:
        # The arrows act on the objects true | false.
        self.assertIs(id(True), True)
        self.assertIs(not_(False), True)
        self.assertIs(not_(True), False)
        # not ∘ id = not; (id >> not_)(False) = not(False) = True.
        self.assertIs((id >> not_)(False), True)
        # Reduction preserves the map.
        self.assertIs(simplify(id >> not_)(False), True)

    def test_sexpr_repr(self) -> None:
        # S-expression convention for repr.
        self.assertEqual(repr(id), "id")
        self.assertEqual(repr(not_), "not")
        self.assertEqual(repr(id >> not_), "(>> id not)")


if __name__ == "__main__":
    unittest.main()
