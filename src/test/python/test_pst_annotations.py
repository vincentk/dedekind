"""Type annotations, checked at construction: Set[C], Relation[A, B], Function[A, B].

An annotation applied to a value checks it and returns it unchanged, or raises
TypeError; ``isinstance(x, Set[C])`` asks the same question.  The verdicts are
the library's: a set's carrier and codomain, a relation's factors, and whether
a relation is the graph of a function, decided when the relation was built.

A set is a function: its χ : C → Ω_L is total (every point gets a verdict) and
single-valued, so ``Set[C]`` implies ``Function[C, L]`` with the set itself as
the arrow.  A relation is a function when its predicate is a graph atom
``π_2 == t(π_1)``: functional by its shape, entire because every operator of a
bound term is total on the carrier (or divides by a nonzero constant).  Applied
to a point it gives ``t(x)``.  Functions between two carriers wait for the
many-sorted terms.
"""

import unittest

from pynzli.pst import A, B, K3, Kleene, Set, Ternary, ap, cond, u64, π, π_1, π_2, 𝔽ₚ, identity
from pynzli.trsk import Function, Relation

F, U_, T = Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE
U, UF = A(u64), A(𝔽ₚ)  # not Fp: that is the name 𝔽ₚ (NFKC)


class Sets(unittest.TestCase):
    def test_a_set_of_its_carrier(self) -> None:
        S = U & (π > 3)
        self.assertIs(Set[u64](S), S)  # checked, and returned unchanged
        self.assertIsInstance(S, Set[u64])
        self.assertNotIsInstance(S, Set[𝔽ₚ])
        with self.assertRaises(TypeError):
            Set[𝔽ₚ](S)
        self.assertIsInstance(K3 & (π > F), Set[K3])
        self.assertIsInstance(identity(K3), Set[K3])
        self.assertNotIsInstance(K3 & (π > F), Set[B])
        self.assertEqual(repr(Set[u64]), "Set[u64]")

    def test_a_relation_is_not_a_set_of_its_factor(self) -> None:
        with self.assertRaises(TypeError):
            Set[u64](U * U & (π_1 < π_2))


class ASetIsAFunction(unittest.TestCase):
    def test_into_its_truth_chain(self) -> None:
        S = U & (π > 3)
        self.assertIs(Function[u64, B](S), S)  # χ : u64 → 𝔹
        self.assertEqual(ap(S, 4), True)
        self.assertIsInstance(K3 & (π > F), Function[K3, B])  # decidable over K₃
        self.assertIsInstance(identity(K3), Function[K3, K3])  # K₃-valued over K₃
        with self.assertRaises(TypeError):
            Function[u64, K3](S)  # its χ is 𝔹-valued
        with self.assertRaises(TypeError):
            Function[𝔽ₚ, B](S)
        # The universes are sets too: χ constant ⊤.
        self.assertIsInstance(U, Function[u64, B])
        self.assertIsInstance(UF, Function[𝔽ₚ, B])


class Relations(unittest.TestCase):
    def test_a_relation_of_its_factors(self) -> None:
        R = U * U & (π_1 < π_2)
        self.assertIs(Relation[u64, u64](R), R)
        self.assertIsInstance(UF * UF & (π_2 == π_1 * 3), Relation[𝔽ₚ, 𝔽ₚ])
        with self.assertRaises(TypeError):
            Relation[𝔽ₚ, 𝔽ₚ](R)
        with self.assertRaises(TypeError):
            Relation[u64, u64](U & (π > 3))  # a set, not a relation
        # The unrestricted products are relations too.
        self.assertIsInstance(U * U, Relation[u64, u64])
        self.assertIsInstance(UF * UF, Relation[𝔽ₚ, 𝔽ₚ])
        self.assertNotIsInstance(U * U, Set[u64])

    def test_a_graph_atom_is_a_function(self) -> None:
        f = Function[u64, u64](U * U & (π_2 == π_1 * 3 + 1))
        self.assertEqual(ap(f, 4), 13)  # applied to a point: t(x)
        self.assertIs(ap(f, (4, 13)), True)  # applied to a pair: χ
        self.assertEqual(ap(Function[u64, u64](U * U & (π_1 + 1 == π_2)), 2**64 - 1), 0)  # it wraps
        third = Function[𝔽ₚ, 𝔽ₚ](UF * UF & (π_2 == π_1 / 3))
        self.assertEqual(ap(third, 3), 𝔽ₚ(1))

    def test_mccarthys_conditional_is_a_function(self) -> None:
        step = Function[u64, u64](U * U & (π_2 == cond(π_1 % 2 == 0, π_1 // 2, π_1 * 3 + 1)))
        self.assertEqual([ap(step, n) for n in (6, 7, 22)], [3, 22, 11])

    def test_what_is_not_certified_a_function(self) -> None:
        for R in (
            U * U & (π_1 < π_2),  # not functional
            U * U & (π_2 == π_2 + π_1),  # the term mentions π_2
            U * U & ((π_2 == π_1 + 1) & (π_1 < 5)),  # not entire: restricted
        ):
            self.assertNotIsInstance(R, Function[u64, u64])
            with self.assertRaises(TypeError):
                ap(R, 3)  # no t to apply
        with self.assertRaises(TypeError):
            Function[u64, 𝔽ₚ](U * U & (π_2 == π_1))  # between carriers: many-sorted terms


if __name__ == "__main__":
    unittest.main()
