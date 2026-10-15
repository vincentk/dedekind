"""Two carriers in one relation: A(u64) * A(𝔽ₚ), each slot its own sort.

A term built at run time names no carrier until it is bound.  On a product of
two carriers each slot has its carrier's sort, a constant takes its context's,
an operator or a comparison needs its operands in one sort, and a term mixing
the two carriers is refused when the relation is built: an arrow between the
carriers is applied, never implied.  The canonical form is per axis.
"""

import unittest

from dedekind.pst import A, Function, Relation, canonical, u64, π_1, π_2, 𝔽ₚ  # 𝔽ₚ is Fp (NFKC)

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
        self.assertEqual(repr(N), "𝔸(u64) × 𝔸(𝔽ₚ) | ⊤")
        self.assertIs(N == N, True)  # rebinds: ⊤ names no sort
        self.assertEqual(repr(canonical(N)), repr(N))
        self.assertIn((1, 1), N)
        self.assertEqual(repr(canonical(U * UF | ((π_1 < 5) & (π_2 != π_2)))),
                         "𝔸(u64) × 𝔸(𝔽ₚ) | (π_1 < 5) & (⊥)")

    def test_a_constant_graph_is_a_function_between_the_carriers(self) -> None:
        f = Function[u64, 𝔽ₚ](U * UF | (π_2 == 1))
        self.assertEqual(f(5), 𝔽ₚ(1))
        self.assertIs(f((5, 1)), True)
        with self.assertRaises(TypeError):
            U * UF | (π_2 == π_1)  # no arrow u64 → 𝔽ₚ is implied


if __name__ == "__main__":
    unittest.main()
