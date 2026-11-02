"""Sets over the truth chains, per the paper's Lwv grammar (#975, PR two).

Every handle is a run list, the C++ normal form of these sets, and every query
is decided in C++ on it; the Python side only spells the grammar.  Queries answer in the
set's own species, so on a K₃-valued set ``==`` may be ``UNKNOWN``.
"""

import unittest

from pynzli.pst import (
    𝔸, Ø, η, π, 𝔹, K3, Ternary, Boole, Kleene, ap, canonical, exists, forall, lift,
    codomain, identity, is_decidable,
    any as any_, all as all_,  # the aliases, imported under names that do not shadow the builtins
)

A, B = 𝔸, 𝔹  # the written forms are the names, under NFKC
F, U, T = Ternary.FALSE, Ternary.UNKNOWN, Ternary.TRUE


class GrammarOverK3Test(unittest.TestCase):
    def test_former_and_membership(self) -> None:
        S = K3 & (π > F)
        self.assertIn(U, S)
        self.assertIn(T, S)
        self.assertNotIn(F, S)
        self.assertIs(ap(S, U), True)
        self.assertIs(ap(S, F), False)

    def test_equality_is_extensional_and_decided(self) -> None:
        self.assertIs((K3 & (π > F)) == (K3 & (π >= U)), True)
        self.assertIs((K3 & (π > F)) == K3, False)
        self.assertIs((K3 & (π > F)) != K3, True)
        self.assertIs(η(U) == (K3 & (π == U)), True)
        self.assertIs((K3 & (π > T)) == Ø(K3), True)  # {x > ⊤} is empty

    def test_lattice(self) -> None:
        S = K3 & (π > F)
        self.assertIs((S | ~S) == K3, True)
        self.assertIs((S & ~S) == Ø(K3), True)
        self.assertIs((S ^ ~S) == K3, True)
        self.assertIs(S <= K3, True)
        self.assertIs(K3 <= S, False)
        self.assertIs(η(T) <= S, True)

    def test_quantifiers_and_aliases(self) -> None:
        self.assertIs(exists(K3, π > F), True)
        self.assertIs(forall(K3, π > F), False)
        self.assertIs(forall(K3, π >= F), True)
        self.assertIs(exists(Ø(K3), π >= F), False)
        self.assertIs(any_(K3, π == U), True)
        self.assertIs(all_(K3 & (π > F), π >= U), True)
        self.assertIs(forall(K3 & (π > F), π == T), False)
        self.assertIs(forall(Ø(K3), π == T), True)  # vacuous

    def test_the_run_list_is_the_canonical_form(self) -> None:
        S = K3 & (π > F)
        self.assertIs(canonical(S) == S, True)  # held as its run list
        self.assertEqual(repr(S), "[U, ⊤]")
        self.assertEqual(repr(η(U)), "{U}")
        self.assertEqual(repr(Ø(K3)), "Ø")
        self.assertEqual(repr(η(F) | η(T)), "{⊥} ∪ {⊤}")
        self.assertEqual(repr(K3), "K₃")  # the universe
        self.assertEqual(repr(~Ø(K3)), "[⊥, ⊤]")  # its full run list

    def test_sugar_on_the_chain(self) -> None:
        # The universe is a set: the former, the lattice, membership.
        self.assertIs((K3 & (π <= U)) == ~(K3 & (π > U)), True)
        self.assertIs((K3 & (K3 & (π > F))) == (K3 & (π > F)), True)
        self.assertIs(~K3 == Ø(K3), True)
        self.assertIs(Ø(K3) == Ø(Ternary), True)
        self.assertIn(U, K3)
        self.assertNotIn(5, K3)
        self.assertEqual(A(Ternary), K3)


class GrammarOverBTest(unittest.TestCase):
    def test_the_two_chain(self) -> None:
        top = B & (π == True)
        self.assertEqual(repr(top), "{⊤}")
        self.assertIs(exists(B, π == True), True)
        self.assertIs(forall(B, π == True), False)
        self.assertIs(top == η(True), True)
        self.assertIs((top | ~top) == B, True)
        self.assertEqual(repr(top), "{⊤}")


class KleeneValuedTest(unittest.TestCase):
    def test_verdicts_in_k3(self) -> None:
        H = identity(K3)  # χ(x) = x, valued in K₃
        self.assertFalse(is_decidable(H))
        self.assertEqual(ap(H, U), U)
        self.assertIn(T, H)  # `in` is the decided membership χ(x) = ⊤
        self.assertNotIn(U, H)
        self.assertEqual(exists(H, π >= F), T)  # ⋁ (χ ∧ P)
        with self.assertRaises(TypeError):
            forall(H, π >= F)  # a bounded ∀ over an L-valued set awaits #980
        self.assertIs(forall(K3 & (ap(H, π) >= U), π >= U), True)  # over its cut
        # Equality is the internal biconditional: reflexive only up to the
        # excluded middle, so H agrees with itself to degree U where it is U.
        self.assertEqual(H == H, U)
        # The excluded middle itself, to degree U: 𝔸 = H ∨ ¬H is Unknown.
        self.assertEqual(A(Ternary, Kleene) == (H | ~H), U)
        self.assertEqual(A(Ternary, Kleene) == A(Ternary, Kleene), T)

    def test_cuts_and_fibres_are_decidable_sets(self) -> None:
        H = identity(K3)
        cut_U = K3 & (ap(H, π) >= U)  # χ applied to π, compared on the chain
        self.assertTrue(is_decidable(cut_U))
        self.assertIs(cut_U == (K3 & (π >= U)), True)
        self.assertIs((K3 & (ap(H, π) >= T)) == η(T), True)
        self.assertIs((K3 & (ap(H, π) == U)) == η(U), True)
        self.assertEqual(repr(cut_U), "[U, ⊤]")
        self.assertIs((K3 & (ap(H, π) == T)) == η(T), True)  # cond(χ ≥ ⊤, ⊤, …) == ⊤
        self.assertEqual(repr(H), "{χ ≥ ⊤}: {⊤}; {χ ≥ U}: [U, ⊤]")

    def test_a_set_applied_to_a_term_is_its_arrow(self) -> None:
        # ap(S, t) is χ_S as a term of sort codomain(S): a decidable table's
        # run predicate as a conditional into 𝔹.
        S = K3 & (π > F)
        self.assertEqual(repr(ap(S, π)), f"cond(π >= {U!r}, True, False)")
        self.assertIs((K3 & (ap(S, π) == True)) == S, True)  # noqa: E712
        self.assertIs((K3 & (ap(S, π) == False)) == ~S, True)  # noqa: E712
        top = B & (π == True)  # noqa: E712
        self.assertIs((B & (ap(top, π) == True)) == top, True)  # noqa: E712
        # An L-valued table's χ is a conditional over its cuts, of sort K₃;
        # a term is not a predicate on any universe: compare it.
        H = identity(K3)
        for universe, term in ((K3, ap(H, π)), (A(Ternary, Kleene), ap(H, π)),
                               (K3, ap(S, π)), (B, π * 1)):
            with self.assertRaises(TypeError):
                universe & term

    def test_the_dominance_is_a_coercion(self) -> None:
        # 𝔹 ↪ K₃: a value of 𝔹 is read in K₃ wherever the two meet, the one
        # coercion of the grammar; any other two carriers meet through ap.
        S = K3 & (π > F)  # χ into 𝔹
        H = identity(K3)  # χ into K₃
        self.assertEqual(K3 & (ap(H, π) == ap(S, π)), η(F) | η(T))  # U ≠ ⊤
        self.assertEqual(K3 & (ap(H, π) >= True), η(T))  # True is ⊤
        self.assertEqual(B & (π == T), B & (π == True))  # noqa: E712
        # A table over K₃ read at the points of 𝔹, through the dominance.
        self.assertEqual(B & (ap(S, π) == True), B & (π == True))  # noqa: E712
        self.assertEqual(B & (ap(H, π) == True), B & (π == True))  # noqa: E712
        with self.assertRaises(TypeError):
            B & (ap(S, π) == 1)  # a numeral is no value of 𝔹
        with self.assertRaises(TypeError):
            bool(π > F)  # a predicate is bound, not tested

    def test_lift(self) -> None:
        S = K3 & (π > F)
        L = lift(S)
        self.assertFalse(is_decidable(L))
        self.assertEqual(ap(L, U), T)
        self.assertEqual(L == lift(K3 & (π >= U)), T)
        self.assertEqual((K3 & (ap(L, π) >= U)) == S, True)


class MixedSpeciesTest(unittest.TestCase):
    """A Boolean set and a K₃-valued set over one chain mix in their join, K₃."""

    def test_lattice_in_the_join(self) -> None:
        S = K3 & (π > F)  # Boolean: {U, ⊤}
        H = identity(K3)  # K₃-valued: χ(x) = x
        M = S & H
        self.assertFalse(is_decidable(M))
        self.assertEqual(ap(M, U), U)  # ⊤ ∧ U
        self.assertEqual(ap(M, T), T)
        self.assertEqual(ap(M, F), F)
        self.assertEqual(ap(S | H, F), F)
        self.assertEqual(ap(H | S, U), T)  # U ∨ ⊤
        self.assertEqual(lift(S) == (S & A(Ternary, Kleene)), T)
        # ^ across species: (⊤ ⊕ U) = U at U, ⊤ ⊕ ⊤ = ⊥ at ⊤, in K₃ either way.
        for X in (S ^ H, H ^ S):
            self.assertFalse(is_decidable(X))
            self.assertEqual((ap(X, F), ap(X, U), ap(X, T)), (F, U, F))

    def test_queries_in_the_join(self) -> None:
        S = K3 & (π > F)
        H = identity(K3)
        # ⋀ (χ_S ⇔ χ_H): agree at ⊥ and ⊤, to degree U at U.
        self.assertEqual(S == H, U)
        self.assertEqual(H == S, U)
        self.assertEqual(η(T) == H, U)  # ⊥ against U at U is only U
        self.assertEqual(η(F) == H, F)  # ⊤ against ⊥ at ⊥: differ outright
        self.assertEqual(S <= H, U)
        self.assertEqual(H <= S, U)
        self.assertEqual(S != H, U)
        # >= on unequal sets, both orders: 𝔸 ⊇ S holds, S ⊇ 𝔸 fails at ⊥.
        self.assertEqual(A(Ternary, Kleene) >= S, T)
        self.assertEqual(S >= A(Ternary, Kleene), F)


class RefusalsTest(unittest.TestCase):
    def test_species_and_carriers_do_not_mix(self) -> None:
        with self.assertRaises(TypeError):
            A(bool, Kleene)
        self.assertTrue(repr(K3 & B).endswith("& ⊥"))  # across carriers: Ø of the sum (#1039)
        with self.assertRaises(TypeError):
            K3 & (π > 1)  # a numeral on K₃: refused when bound
        self.assertEqual(K3 & (π > True), Ø(K3))  # a bool is read in K₃: True is ⊤
        with self.assertRaises(TypeError):
            exists(K3, η(U))  # a set is not a where-clause
