"""Jlt identity-law exhibit (#961): the monoid unit law, reduced in C++.

``simplify`` is the value-first ``cata`` (``ArrowTerm::reduce``); Python only
holds handles and asks C++ to normalise.  These pin the unit law
``id ∘ f = f = f ∘ id``, the inert two-non-units case, and apply.  Scope is the
identity laws only (atoms are opaque; no inverse/involution law yet).
"""

import unittest

from dedekind.jlt import atom, id, simplify


class JltIdentityLawsTest(unittest.TestCase):
    def test_id_compose_id_is_id(self) -> None:
        # id ∘ id = id
        self.assertEqual(simplify(id >> id), id)

    def test_right_identity_drops(self) -> None:
        # f ∘ id = f
        f = atom("f", lambda x: x + 1)
        self.assertEqual(simplify(f >> id), f)

    def test_left_identity_drops(self) -> None:
        # id ∘ f = f
        f = atom("f", lambda x: x + 1)
        self.assertEqual(simplify(id >> f), f)

    def test_two_non_units_stay_inert(self) -> None:
        # Neither leg is the unit, so the composite is its own normal form.
        f = atom("f", lambda x: x + 1)
        g = atom("g", lambda x: x * 2)
        self.assertEqual(repr(simplify(f >> g)), "(>> f g)")

    def test_apply_runs_the_wrapped_map(self) -> None:
        # f >> g = "apply f, then g"; (id >> f)(41) = f(41) = 42.
        f = atom("f", lambda x: x + 1)
        self.assertEqual((id >> f)(41), 42)
        # Reduction preserves the map: simplify(id >> f) applies as f.
        self.assertEqual(simplify(id >> f)(41), 42)

    def test_sexpr_repr(self) -> None:
        # S-expression convention for repr.
        f = atom("f", lambda x: x + 1)
        self.assertEqual(repr(id), "id")
        self.assertEqual(repr(f), "f")
        self.assertEqual(repr(id >> f), "(>> id f)")


if __name__ == "__main__":
    unittest.main()
