"""Sets over the truth chains, per the paper's Lwv grammar (#975, PR two).

Every handle is a real C++ set and every query is decided in C++ by exhausting
the chain; the Python side only spells the grammar.  Queries answer in the
set's own species, so on a K₃-valued set ``==`` may be ``UNKNOWN``.
"""

import unittest

from dedekind.pst import 𝔸, Ø, η, π, χ, 𝔹, K3, Ternary, Boole, Kleene
from dedekind.pst import exists, forall, runs, lift
from dedekind import pst

A, B = 𝔸, 𝔹  # the written forms are the names, under NFKC
F, U, T = Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE


class GrammarOverK3Test(unittest.TestCase):
    def test_former_and_membership(self) -> None:
        S = A(K3) | (π > F)
        self.assertIn(U, S)
        self.assertIn(T, S)
        self.assertNotIn(F, S)
        self.assertIs(S(U), True)
        self.assertIs(S(F), False)
        self.assertIs((A(K3) | (χ == U))(U), True)  # χ is a second spelling of π

    def test_equality_is_extensional_and_decided(self) -> None:
        self.assertIs((A(K3) | (π > F)) == (A(K3) | (π >= U)), True)
        self.assertIs((A(K3) | (π > F)) == A(K3), False)
        self.assertIs((A(K3) | (π > F)) != A(K3), True)
        self.assertIs(η(U) == (A(K3) | (π == U)), True)
        self.assertIs((A(K3) | (π > T)) == Ø(K3), True)  # {x > ⊤} is empty

    def test_lattice(self) -> None:
        S = A(K3) | (π > F)
        self.assertIs((S | ~S) == A(K3), True)
        self.assertIs((S & ~S) == Ø(K3), True)
        self.assertIs((S ^ ~S) == A(K3), True)
        self.assertIs(S <= A(K3), True)
        self.assertIs(A(K3) <= S, False)
        self.assertIs(η(T) <= S, True)

    def test_quantifiers_and_aliases(self) -> None:
        self.assertIs(exists(A(K3), π > F), True)
        self.assertIs(forall(A(K3), π > F), False)
        self.assertIs(forall(A(K3), π >= F), True)
        self.assertIs(exists(Ø(K3), π >= F), False)
        self.assertIs(pst.any(A(K3), π == U), True)
        self.assertIs(pst.all(A(K3) | (π > F), π >= U), True)

    def test_runs_are_the_normal_form(self) -> None:
        S = A(K3) | (π > F)
        self.assertEqual(runs(S), [(U, T)])
        self.assertEqual(S.runs(), [(U, T)])
        self.assertEqual(repr(S), "[U, ⊤]")
        self.assertEqual(repr(η(U)), "{U}")
        self.assertEqual(repr(Ø(K3)), "Ø")
        self.assertEqual(repr(η(F) | η(T)), "{⊥} ∪ {⊤}")
        self.assertEqual(repr(A(K3)), "[⊥, ⊤]")

    def test_sugar_on_the_chain(self) -> None:
        self.assertIs(K3.above(F) == (A(K3) | (π > F)), True)
        self.assertIs(K3.at_most(U) == ~K3.above(U), True)
        self.assertIs(K3.point(U) == η(U), True)
        self.assertIs(K3.all == A(K3), True)
        self.assertIs(K3.none == Ø(K3), True)


class GrammarOverBTest(unittest.TestCase):
    def test_the_two_chain(self) -> None:
        top = A(B) | (π == True)
        self.assertEqual(runs(top), [(True, True)])
        self.assertIs(exists(A(B), π == True), True)
        self.assertIs(forall(A(B), π == True), False)
        self.assertIs(top == η(True), True)
        self.assertIs((top | ~top) == A(B), True)
        self.assertEqual(repr(top), "{⊤}")


class KleeneValuedTest(unittest.TestCase):
    def test_verdicts_in_k3(self) -> None:
        H = K3.identity  # χ(x) = x, valued in K₃
        self.assertFalse(H.is_decidable)
        self.assertEqual(H(U), U)
        self.assertIn(T, H)  # `in` is the decided membership χ(x) = ⊤
        self.assertNotIn(U, H)
        self.assertEqual(exists(H, π >= F), T)  # ⋁ χ
        self.assertEqual(forall(H, π >= F), F)  # ⋀ χ: ⊥ at ⊥
        # Equality is the internal biconditional: reflexive only up to the
        # excluded middle, so H agrees with itself to degree U where it is U.
        self.assertEqual(H == H, U)
        # The excluded middle itself, to degree U: 𝔸 = H ∨ ¬H is Unknown.
        self.assertEqual(A(K3, Kleene) == (H | ~H), U)
        self.assertEqual(A(K3, Kleene) == A(K3, Kleene), T)

    def test_cuts_and_fibres_are_decidable_sets(self) -> None:
        H = K3.identity
        self.assertTrue(H.cut(U).is_decidable)
        self.assertIs(H.cut(U) == (A(K3) | (π >= U)), True)
        self.assertIs(H.cut(T) == η(T), True)
        self.assertIs(H.fibre(U) == η(U), True)
        self.assertEqual(runs(H.cut(U)), [(U, T)])
        self.assertEqual(repr(H), "{χ ≥ ⊤}: {⊤}; {χ ≥ U}: [U, ⊤]")
        with self.assertRaises(TypeError):
            runs(H)  # an L-valued set is read through its cuts

    def test_lift(self) -> None:
        S = A(K3) | (π > F)
        L = lift(S)
        self.assertFalse(L.is_decidable)
        self.assertEqual(L(U), T)
        self.assertEqual(L == lift(A(K3) | (π >= U)), T)
        self.assertEqual(L.cut(U) == S, True)


class RefusalsTest(unittest.TestCase):
    def test_species_and_carriers_do_not_mix(self) -> None:
        with self.assertRaises(TypeError):
            A(B, Kleene)
        with self.assertRaises(TypeError):
            A(K3) & A(B)  # different chains: no overload
        with self.assertRaises(TypeError):
            A(K3) | (π > True)  # a 𝔹 datum on K₃: no overload
        with self.assertRaises(TypeError):
            exists(A(K3), η(U))  # a set is not a where-clause
