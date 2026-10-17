"""Relations as terms (#1037): a composite, a projection, a meet are nodes of
a term over relations; the predicate is a reading of it (``R.predicate``), a
composite's the ∃ over the middle.  ``canonical`` eliminates a quantifier
where it can (the fusion law, Shannon's expansion on a finite chain) and keeps
it otherwise; membership searches what remains, valued in Σ (∃) or Π (∀).
Relations are the Kleisli arrows of the powerset monad: ``diag`` is η, ``>>``
Kleisli composition, the ∃ its μ, and the monad laws are decided by
``canonical``.  They form a cartesian bicategory: ``copy`` and ``merge`` are
the Frobenius spiders, ``R.converse`` the dagger, and those laws are
witnessed at points (deciding them by ``canonical`` waits for the canonical
order of conjuncts, #884)."""

import unittest

from dedekind.pst import B, K3, U64, Ternary, ap, canonical, cond, copy, diag, merge, u64, π_1, π_2

succ = U64 * U64 | (π_2 == π_1 + 1)
sq = U64 * U64 | (π_2 * π_2 == π_1)
cube = U64 * U64 | (π_1 * π_1 * π_1 == π_2)


class Composition(unittest.TestCase):
    def test_a_composite_is_a_term_and_its_predicate_a_reading(self) -> None:
        two = succ >> succ
        self.assertEqual(repr(two), "(𝔸(u64) × 𝔸(u64) | π_2 == (π_1 + 1)) >> "
                                    "(𝔸(u64) × 𝔸(u64) | π_2 == (π_1 + 1))")
        self.assertEqual(repr(two.predicate), "∃π_3 ∈ 𝔸(u64). "
                                              "(π_3 == (π_1 + 1)) & (π_2 == (π_3 + 1))")
        self.assertIn((3, 5), two)
        self.assertNotIn((3, 6), two)

    def test_a_defined_middle_is_eliminated_by_substitution(self) -> None:
        two = succ >> succ
        self.assertIs(two == (U64 * U64 | (π_2 == π_1 + 2)), True)
        self.assertNotIn("∃", repr(canonical(two)))
        self.assertIs(two.is_function, True)  # the fusion law
        self.assertEqual(ap(two, 3), 5)

    def test_a_middle_on_a_small_carrier_is_expanded(self) -> None:
        # ∃b ∈ 𝔹. a > 5 ∧ c = (1 if b else 0): Shannon's expansion.
        R = U64 * B | (π_1 > 5)
        S = B * U64 | (π_2 == cond(π_1 == True, 1, 0))  # noqa: E712
        RS = R >> S
        self.assertIn((7, 1), RS)
        self.assertIn((7, 0), RS)
        self.assertNotIn((7, 2), RS)
        self.assertNotIn((3, 1), RS)
        self.assertNotIn("∃", repr(canonical(RS)))

    def test_an_undefined_middle_on_a_word_is_searched(self) -> None:
        # ∃b. b² = a ∧ b³ = c: no leg determines b, and 2⁶⁴ is too many to
        # expand.  canonical keeps the quantifier; membership searches, in Σ.
        sc = sq >> cube
        self.assertIn("∃π_3 ∈ 𝔸(u64)", repr(sc.predicate))
        self.assertIn("∃π_3 ∈ 𝔸(u64)", repr(canonical(sc)))
        self.assertIs(sc.is_function, False)
        self.assertEqual(sc.codomain, K3)
        self.assertEqual(ap(sc, (4, 8)), Ternary.TRUE)  # b = 2, a witness
        self.assertEqual(ap(sc, (4, 9)), Ternary.UNKNOWN)  # none within the budget
        self.assertIn((4, 8), sc)  # in: χ = ⊤
        self.assertNotIn((4, 9), sc)
        self.assertEqual(ap(sc.within(1), (4, 8)), Ternary.UNKNOWN)  # only b = 0

    def test_the_middle_columns_agree(self) -> None:
        with self.assertRaises(TypeError):
            (U64 * B) >> (U64 * B)  # 𝔹 is not the word


class Projection(unittest.TestCase):
    def test_a_function_projects_onto_its_whole_domain(self) -> None:
        dom = succ.project(1)  # R >> π: composition with the structural projection
        self.assertEqual(repr(dom), "(𝔸(u64) × 𝔸(u64) | π_2 == (π_1 + 1)) >> "
                                    "(𝔸(u64) × 𝔸(u64) × 𝔸(u64) | π_3 == π_1)")
        # The reading is literal: the composite's ∃ over the whole middle, two
        # columns; canonical eliminates both (each defined by an atom).
        self.assertEqual(repr(dom.predicate), "∃π_2 ∈ 𝔸(u64). ∃π_3 ∈ 𝔸(u64). "
                                              "(π_3 == (π_2 + 1)) & (π_1 == π_2)")
        self.assertEqual(repr(canonical(dom)), "𝔸(u64)")  # entire
        self.assertIn((5,), dom)

    def test_projection_reorders_and_keeps(self) -> None:
        flipped = succ.project(2, 1)  # the converse
        self.assertIn((4, 3), flipped)
        self.assertNotIn((3, 4), flipped)
        with self.assertRaises(TypeError):
            succ.project(1, 1)

    def test_spans_compose_with_products_and_the_lattice(self) -> None:
        dom = succ.project(1)
        self.assertIn((5, True), dom * B)
        same = dom & (U64 * U64 | (π_2 == π_1)).project(1)
        self.assertIn((5,), same)


class TheTreeStays(unittest.TestCase):
    """& | ~ are the set grammar's nodes on relations, not one flattened
    predicate: the term is legible, its predicate a reading."""

    def test_a_meet_is_a_node(self) -> None:
        m = succ & sq
        self.assertEqual(repr(m), "(𝔸(u64) × 𝔸(u64) | π_2 == (π_1 + 1)) & "
                                  "(𝔸(u64) × 𝔸(u64) | (π_2 * π_2) == π_1)")
        self.assertEqual(repr(m.predicate), "(π_2 == (π_1 + 1)) & ((π_2 * π_2) == π_1)")
        self.assertIn((3, 4), ~m | m)  # a join of a complement: still a term
        self.assertEqual(repr((U64 * U64 | (π_1 > 3)) | (π_2 > 1)),
                         "(𝔸(u64) × 𝔸(u64) | π_1 > 3) | π_2 > 1")  # two filters, unfused


class TheKleisliStructure(unittest.TestCase):
    """Relations are the Kleisli arrows of the powerset monad: diag is η, >>
    Kleisli composition, the ∃ over the middle its μ.  The monad laws are
    decided by canonical where the ∃ eliminates (every leg here defines its
    middle)."""

    def test_the_unit_laws(self) -> None:
        Δ = diag(U64)
        self.assertEqual(repr(Δ), "𝔸(u64) × 𝔸(u64) | π_1 == π_2")
        self.assertIs((Δ >> succ) == succ, True)
        self.assertIs((succ >> Δ) == succ, True)

    def test_associativity(self) -> None:
        three = U64 * U64 | (π_2 == π_1 * 3)
        # sq is b² = a, a relation (b a root of a): its middle no term defines,
        # so the ∃ is kept on both sides and the shapes still agree.
        self.assertIs(((succ >> sq) >> three) == (succ >> (sq >> three)), True)
        self.assertIn("∃", repr(canonical((succ >> sq) >> three).predicate))
        # The square as a function: every middle defined, the ∃s eliminated.
        square = U64 * U64 | (π_2 == π_1 * π_1)
        self.assertIs(((succ >> square) >> three) == (succ >> (square >> three)), True)
        self.assertIs(((succ >> square) >> three) == (U64 * U64 | (π_2 == (π_1 + 1) * (π_1 + 1) * 3)), True)

    def test_the_identity_at_points(self) -> None:
        Δ = diag(B)
        self.assertIn((True, True), Δ)
        self.assertNotIn((True, False), Δ)
        self.assertEqual(Δ.dom, (B,))
        self.assertEqual(Δ.cod, (B,))
        self.assertEqual(succ.dom, ())  # a table, out of the unit
        self.assertEqual(succ.arrow.dom, (u64,))  # read as the arrow of its first column


class TheFrobeniusStructure(unittest.TestCase):
    """Relations form a cartesian bicategory: copy and merge are the spiders,
    R ∧ S = merge ∘ (R ⊗ S) ∘ copy, and the converse is the dagger.
    Witnessed at points: deciding these by canonical needs the canonical order
    of conjuncts (#884)."""

    def test_the_meet_is_a_spider(self) -> None:
        spider = copy(U64) >> (succ.arrow * sq.arrow) >> merge(U64)
        self.assertEqual(spider.dom, (u64,))
        self.assertEqual(spider.cod, (u64,))
        for row in ((0, 1), (3, 4), (4, 2), (1, 1), (2, 3)):
            self.assertEqual(ap(spider, row), ap(succ & sq, row), row)
        # The middle of copy >> (R ⊗ S) is two columns: ∃∃.
        self.assertTrue(repr((copy(U64) >> (succ.arrow * sq.arrow)).predicate)
                        .startswith("∃π_4 ∈ 𝔸(u64). ∃π_5 ∈ 𝔸(u64). "))

    def test_the_dagger(self) -> None:
        pred = succ.converse
        self.assertEqual(repr(pred), "(𝔸(u64) × 𝔸(u64) | π_2 == (π_1 + 1))°")
        self.assertIn((4, 3), pred)
        self.assertNotIn((3, 4), pred)
        self.assertIs(pred.converse == succ, True)  # R°° = R, by canonical
        sc = sq >> cube
        for row in ((8, 4), (9, 4), (27, 9), (8, 3)):  # (R;S)° = S°;R°
            self.assertEqual(ap(sc.converse, row), ap(cube.converse >> sq.converse, row), row)


class Duality(unittest.TestCase):
    """~ pushes down: ~(∃h. P) = ∀h. ~P; ∀ is eliminated by the dual rules
    and searched for a counterexample (Π)."""

    def test_the_complement_of_a_span_is_a_forall(self) -> None:
        dom = succ.project(1)
        co = ~dom
        self.assertTrue(repr(co).startswith("~((𝔸(u64) × 𝔸(u64) | π_2 == (π_1 + 1)) >> ("))
        self.assertEqual(repr(co.predicate), "∀π_2 ∈ 𝔸(u64). ∀π_3 ∈ 𝔸(u64). "
                                             "(π_3 != (π_2 + 1)) | (π_1 != π_2)")
        self.assertEqual(repr(canonical(co)), "𝔸(u64) | ⊥")  # a ∀ defined by π_2 ≠ t
        self.assertNotIn((5,), co)
        self.assertIs(canonical(~co) == canonical(dom), True)

    def test_a_forall_on_a_word_is_searched_for_a_counterexample(self) -> None:
        nsc = ~(sq >> cube)  # ∀b. b² ≠ a ∨ b³ ≠ c
        self.assertIn("∀π_3 ∈ 𝔸(u64)", repr(nsc.predicate))
        self.assertEqual(ap(nsc, (4, 8)), Ternary.FALSE)  # b = 2 refutes it
        self.assertEqual(ap(nsc, (4, 9)), Ternary.UNKNOWN)  # Π: never ⊤

    def test_alternating_quantifiers_answer_unknown(self) -> None:
        # ∃c ∀b. b² ≠ a ∨ b³ ≠ c: an ∃ outside a ∀ on the words.
        alt = (~(sq >> cube)).project(1)
        self.assertIn("∃π_2 ∈ 𝔸(u64). ∃π_3 ∈ 𝔸(u64). (∀π_4 ∈ 𝔸(u64).", repr(alt.predicate))
        self.assertEqual(ap(alt, (4,)), Ternary.UNKNOWN)

    def test_a_forall_on_a_small_carrier_is_a_conjunction(self) -> None:
        # R = ∃b ∈ 𝔹. a > 5 ∨ b holds everywhere (b = ⊤ is a witness), so
        # ~R = ∀b ∈ 𝔹. a ≤ 5 ∧ b ≠ ⊤ is empty (b = ⊤ refutes it): Shannon's ∧.
        R = (U64 * B | ((π_1 > 5) | (π_2 == True))).project(1)  # noqa: E712
        self.assertIn((3,), R)
        self.assertIn((7,), R)
        self.assertNotIn((3,), ~R)
        self.assertNotIn((7,), ~R)
        self.assertEqual(repr(canonical(~R)), "𝔸(u64) | ⊥")


if __name__ == "__main__":
    unittest.main()
