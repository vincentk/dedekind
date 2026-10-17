"""Two carriers in one relation: A(u64) * A(𝔽ₚ), each slot its own sort.

A term built at run time names no carrier until it is bound.  On a product of
two carriers each slot has its carrier's sort, a constant takes its context's,
an operator or a comparison needs its operands in one sort, and a term mixing
the two carriers is refused when the relation is built: an arrow between the
carriers is applied, never implied.  The canonical form is per axis.
"""

import unittest

from dedekind.pst import A, B, K3, U64, GF_p, Ternary, ap, canonical, cond, u64, π, π_1, π_2, π_3, 𝔽ₚ
from dedekind.trsk import Function, Relation
)

U, UF = A(u64), A(𝔽ₚ)
third = 𝔽ₚ(1) / 3


class TwoCarriersInOneRelation(unittest.TestCase):
    def test_each_slot_its_own_sort(self) -> None:
        R = U * UF | ((π_1 < 5) & (π_2 * 3 == 1))
        self.assertEqual(repr(U * UF), "𝔸(u64) × 𝔸(𝔽ₚ)")
        self.assertIn((3, third), R)
        self.assertIn((3, int(third)), R)  # a point by its representative
        self.assertNotIn((7, third), R)
        self.assertNotIn((3, 1), R)
        self.assertIsInstance(R, Relation[u64, 𝔽ₚ])
        self.assertNotIsInstance(R, Relation[𝔽ₚ, u64])
        self.assertIsInstance(UF * U | (π_1 * 3 == 1), Relation[𝔽ₚ, u64])

    def test_a_term_mixing_the_carriers_is_refused(self) -> None:
        for build in (
            lambda: U * UF | (π_1 == π_2),
            lambda: U * UF | (π_1 + π_2 == 0),
            lambda: U * UF | (π_2 < 5),  # the field has no order
            lambda: U * UF | (π_1 / 2 == 1),  # the word has no field division
        ):
            with self.assertRaises(TypeError):
                build()

    def test_the_canonical_form_is_per_axis(self) -> None:
        R = U * UF | ((π_1 < 5) & (π_2 * 3 == 1))
        self.assertEqual(R, U * UF | ((π_1 < 5) & (π_2 == third)))  # monic on 𝔽ₚ
        self.assertIn("(π_1 < 5) & (", repr(canonical(R)))
        with self.assertRaises(TypeError):
            R == (U * UF | ((π_1 < 6) & (π_2 == third)))  # different forms: undecided

    def test_a_decided_atom_is_written_as_a_truth_constant(self) -> None:
        N = canonical(U * UF | (π_2 == π_2))
        self.assertEqual(repr(N), "𝔸(u64) × 𝔸(𝔽ₚ)")  # ⊤: the product itself
        self.assertIs(N == N, True)  # rebinds: ⊤ names no sort
        self.assertEqual(repr(canonical(N)), repr(N))
        self.assertIn((1, 1), N)
        self.assertEqual(repr(canonical(U * UF | ((π_1 < 5) & (π_2 != π_2)))),
                         "𝔸(u64) × 𝔸(𝔽ₚ) | ⊥")  # ⊥ absorbs the meet

    def test_only_the_dominance_coerces(self) -> None:
        # 𝔹 ↪ K₃ is the grammar's one coercion: a bool is no word, and a word
        # meets 𝔹 only through an arrow applied.
        with self.assertRaises(TypeError):
            U * UF | (π_1 == True)  # noqa: E712

    def test_a_constant_graph_is_a_function_between_the_carriers(self) -> None:
        f = Function[u64, 𝔽ₚ](U * UF | (π_2 == 1))
        self.assertEqual(ap(f, 5), 𝔽ₚ(1))
        self.assertIs(ap(f, (5, 1)), True)
        with self.assertRaises(TypeError):
            U * UF | (π_2 == π_1)  # no arrow u64 → 𝔽ₚ is implied



class EveryPairOfCarriers(unittest.TestCase):
    """The four universes, B, K3, U64 and GF_p, pairwise: each product a
    relation's universe, each slot its own sort."""

    def test_every_ordered_pair_is_a_product(self) -> None:
        names = ((U64, "𝔸(u64)"), (GF_p, "𝔸(𝔽ₚ)"), (B, "𝔹"), (K3, "K₃"))
        for a, x in names:
            for b, y in names:
                self.assertEqual(repr(a * b), f"{x} × {y}")
                self.assertEqual((a * b).codomain, B)

    def test_a_function_across_carriers(self) -> None:
        # A set is an arrow into 𝔹: applied to π_1, it is the graph of χ.
        chi = Function[u64, B](U64 * B | (π_2 == ap(U64 | (π > 5), π_1)))
        self.assertIs(ap(chi, 7), True)
        self.assertIs(ap(chi, 2), False)
        self.assertIs(ap(chi, (7, True)), True)
        self.assertIs(ap(chi, (7, False)), False)
        # Into K₃: ⊤ past 5, Unknown at or below.
        k = U64 * K3 | (π_2 == cond(π_1 > 5, Ternary.TRUE, Ternary.UNKNOWN))
        self.assertIsInstance(k, Function[u64, K3])
        self.assertEqual(ap(k, 2), Ternary.UNKNOWN)
        self.assertEqual(ap(k, 9), Ternary.TRUE)
        # Back from 𝔹 to the field: the graph of b ↦ cond(b, 1, 0).
        one_hot = Function[B, 𝔽ₚ](B * GF_p | (π_2 == cond(π_1 == True, 1, 0)))  # noqa: E712
        self.assertEqual(ap(one_hot, True), 𝔽ₚ(1))
        with self.assertRaises(TypeError):
            Function[u64, B](U64 * B | (π_2 == π_1))  # a word is no truth value

    def test_the_truth_chains_meet_in_k3(self) -> None:
        same = B * K3 | (π_1 == π_2)  # 𝔹 ↪ K₃, the one coercion
        self.assertIn((True, Ternary.TRUE), same)
        self.assertIn((False, Ternary.FALSE), same)
        self.assertNotIn((True, Ternary.UNKNOWN), same)
        self.assertIsInstance(same, Relation[B, K3])


class AnArrowKeepsItsSorts(unittest.TestCase):
    """An arrow means the same wherever it is applied: its numerals are pinned
    to the sorts they had, and a function lands in its codomain."""

    def test_a_numeral_keeps_its_carrier_through_ap(self) -> None:
        p = 2**64 - 59
        f = Function[B, 𝔽ₚ](B * GF_p | (π_2 == p))  # p is 0 in 𝔽ₚ
        self.assertEqual(ap(f, True), 𝔽ₚ(0))
        self.assertEqual(repr(ap(f, π)), "𝔽ₚ(0)")
        with self.assertRaises(TypeError):
            B * U64 | (π_2 == ap(f, π_1))  # an element of 𝔽ₚ is no word

    def test_a_function_lands_in_its_codomain(self) -> None:
        R = K3 * B | (π_2 == π_1)  # a relation: U has no image in 𝔹
        self.assertIn((Ternary.TRUE, True), R)
        self.assertIs(R.is_function, False)
        self.assertNotIsInstance(R, Function[K3, B])
        with self.assertRaises(TypeError):
            ap(R, Ternary.UNKNOWN)
        self.assertIsInstance(B * K3 | (π_2 == π_1), Function[B, K3])  # 𝔹 ↪ K₃

    def test_a_truth_atom_off_the_factors_is_its_own_form(self) -> None:
        R = B * B | (π_1 == Ternary.TRUE)  # read in K₃, no factor of the pair
        self.assertEqual(repr(canonical(R)), repr(R))
        self.assertIs(R == R, True)

    def test_a_lifted_branch_keeps_its_sort(self) -> None:
        R = U64 * GF_p | (cond(π_1 > 5, π_2, 0) == 0)  # 0 is the field's
        canonical(R)  # the else branch, 0 == 0, is decided in 𝔽ₚ
        self.assertIs(R == R, True)

    def test_a_product_universe_is_an_arrow(self) -> None:
        self.assertIs(ap(U64 * B, (7, True)), True)
        self.assertIs(ap(U64 * B, (7, Ternary.TRUE)), False)  # not a pair of it

    def test_a_universe_applied_to_a_term_checks_its_sort(self) -> None:
        self.assertEqual(U64 | (ap(U64, π) == True), U64 | (π >= 0))  # noqa: E712
        with self.assertRaises(TypeError):
            B | (ap(U64, π) == True)  # a bool is no word  # noqa: E712
        with self.assertRaises(TypeError):
            B | (ap(U64 * B, π) == True)  # a point of the product is a pair  # noqa: E712
        self.assertIsInstance(
            U64 * B | (π_2 == ap(U64 * B, (π_1, True))), Function[u64, B])

    def test_a_numeral_takes_the_points_one_sort_when_pinned(self) -> None:
        S = U64 | (cond(π > 5, 1, 0) == 1)  # the branches are words
        chi = U64 * B | (π_2 == ap(S, π_1))
        self.assertIs(ap(chi, 7), True)
        self.assertIs(ap(chi, 3), False)


class Tables(unittest.TestCase):
    """A relation of any arity: one class, its schema (a sort per column) as
    data; a table is a multi-sorted relation (#1037)."""

    def test_a_product_of_three_carriers(self) -> None:
        T = U64 * B * GF_p
        self.assertEqual(repr(T), "𝔸(u64) × 𝔹 × 𝔸(𝔽ₚ)")
        self.assertEqual(T.factors, (u64, B, 𝔽ₚ))
        self.assertIsInstance(T, Relation[u64, B, 𝔽ₚ])
        self.assertIn((7, True, 𝔽ₚ(3)), T)
        self.assertNotIn((7, Ternary.TRUE, 𝔽ₚ(3)), T)  # K₃ is no column of it
        self.assertNotIn((7, True), T)  # a point has the product's arity
        self.assertEqual(type(T), type(U64 * B))  # one class for every arity

    def test_a_former_on_three_columns(self) -> None:
        R = U64 * B * GF_p | ((π_2 == ap(U64 | (π > 5), π_1)) & (π_3 * 2 == 1))
        self.assertIn((7, True, 𝔽ₚ(1) / 2), R)
        self.assertNotIn((7, False, 𝔽ₚ(1) / 2), R)
        with self.assertRaises(TypeError):
            U64 * B * GF_p | (π_1 == π_3)  # a word is no element of 𝔽ₚ

    def test_the_product_is_associative_and_shifts_the_slots(self) -> None:
        left = (U64 * B) * GF_p
        right = U64 * (B * GF_p)
        self.assertEqual(repr(left), repr(right))
        S = U64 * (B * GF_p | (π_2 == 1))  # B × GF_p's π_2 is the product's π_3
        self.assertIn((5, True, 𝔽ₚ(1)), S)
        self.assertNotIn((5, True, 𝔽ₚ(2)), S)
        self.assertEqual(repr(S), "𝔸(u64) × (𝔹 × 𝔸(𝔽ₚ) | π_2 == 1)")  # the term
        self.assertEqual(repr(S.predicate), "π_3 == 1")  # the reading, shifted

    def test_a_set_is_a_factor(self) -> None:
        S = (U64 * (U64 | (π > 5)))  # its π is the column π_2
        self.assertIn((0, 7), S)
        self.assertNotIn((0, 3), S)

    def test_a_function_of_two_arguments(self) -> None:
        fib = U64 * U64 * U64 | (π_3 == π_1 + π_2)
        self.assertIsInstance(fib, Function[u64, u64, u64])
        self.assertEqual(ap(fib, (3, 5)), 8)
        self.assertIs(ap(fib, (3, 5, 8)), True)  # χ at a whole row
        self.assertIs(ap(fib, (3, 5, 9)), False)
        self.assertEqual(repr(ap(fib, (π, 1))), "(π + 1)")  # the term, at terms
        mixed = Function[u64, B, 𝔽ₚ](U64 * B * GF_p | (π_3 == cond(π_2 == True, 1, 0)))  # noqa: E712
        self.assertEqual(ap(mixed, (4, True)), 𝔽ₚ(1))

    def test_relations_over_different_products_do_not_compare(self) -> None:
        with self.assertRaises(TypeError):
            (U64 * B) == (U64 * B * B)
        with self.assertRaises(TypeError):
            (U64 * B) & (B * U64)
        self.assertIs((U64 * B | (π_1 + 0 > 3)) == (U64 * B | (π_1 > 3)), True)

    def test_the_normal_form_reads_every_column(self) -> None:
        R = U64 * U64 * U64 | (π_3 + 0 == π_1 * 1 + π_2)
        self.assertEqual(R, U64 * U64 * U64 | (π_3 == π_1 + π_2))


if __name__ == "__main__":
    unittest.main()
