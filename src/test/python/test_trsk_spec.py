"""The Python face of the relational grammar, as #1006 states it.

Each test encodes one claim from #1006's prose (and the operator surface of
#1018, the Pst definition on #981).  A test runs as soon as the names it needs
are bound and is skipped until then, so this file is the acceptance spec of
the binding: the claims are stated now, and switch on as the binding lands.
"""

import importlib
import unittest
from fractions import Fraction


def _bound(module: str, *names: str) -> bool:
    """Whether ``module`` imports and binds every one of ``names``."""
    try:
        mod = importlib.import_module(module)
    except ImportError:
        return False
    return all(hasattr(mod, name) for name in names)


def _needs(module: str, *names: str, exhibit: str):
    return unittest.skipUnless(
        _bound(module, *names), f"#1006 {exhibit}: not bound yet"
    )


class PathHandles(unittest.TestCase):
    """Exhibit 1: a Path is an iterable with positional slicing."""

    @_needs("dedekind.sequences", "N", exhibit="exhibit 1")
    def test_positional_slicing_is_range(self) -> None:
        from dedekind.sequences import N

        self.assertEqual(list(N[3:8]), list(range(3, 8)))
        self.assertEqual(N[5], 5)

    @_needs("dedekind.sequences", "N", exhibit="exhibit 1")
    def test_the_successor_orbit_is_not_absorptive(self) -> None:
        from dedekind.sequences import N

        # ℵ₀ is its supremum, not one of its terms.
        self.assertFalse(N.is_absorptive)


class CutsPullBackAlongAdjoints(unittest.TestCase):
    """Exhibit 2: the preimage of a ray under a monotone injection is a prefix,
    computed by the adjoint, not by scanning."""

    @_needs("dedekind.sequences", "N", "preimage", "below", exhibit="exhibit 2")
    def test_preimage_of_a_rational_ray_is_a_prefix(self) -> None:
        from dedekind.sequences import below, preimage

        def r(n: int) -> Fraction:  # n ↦ n / (n + 1), monotone ℕ → ℚ ∩ [0, 1)
            return Fraction(n, n + 1)

        # n / (n + 1) < 3/4  ⟺  n < 3
        self.assertEqual(list(preimage(r, below(Fraction(3, 4)))), [0, 1, 2])


class RelationAsSymbolicFunction(unittest.TestCase):
    """Exhibit 3: a relation functional in one projection solves to a path."""

    @_needs("dedekind.pst", "N", "π1", "π2", "π3", exhibit="exhibit 3")
    def test_the_fibonacci_law_is_a_function_and_solves_to_a_path(self) -> None:
        from dedekind.pst import N, π1, π2, π3

        law = N * N * N | (π3 == π1 + π2)
        self.assertTrue(law.is_function)  # functional in π3
        fib = law.solve(0, 1)
        self.assertEqual(list(fib[:8]), [0, 1, 1, 2, 3, 5, 8, 13])
        self.assertEqual(list(fib[3:][:5]), [2, 3, 5, 8, 13])


class ExtToTheHost(unittest.TestCase):
    """Exhibit 4: ext lowers a finite relation to host data, one row per
    element of the domain when the relation is a function."""

    @_needs("dedekind.pst", "N", "π1", "π2", "ext", exhibit="exhibit 4")
    def test_a_functional_relation_is_one_row_per_domain_element(self) -> None:
        from dedekind.pst import N, ext, π1, π2

        doubling = (N[:5] * N) | (π2 == π1 * 2)
        self.assertEqual(ext(doubling), [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)])


class TheTotalOperatorSurface(unittest.TestCase):
    """#1018 / #981: a term operator is admitted where it is total on the
    carrier, or divides by a nonzero constant."""

    @_needs("dedekind.pst", "N", "π", "π1", "π2", exhibit="exhibit 3 (terms)")
    def test_on_the_naturals(self) -> None:
        from dedekind.pst import N, π, π1, π2

        # + and · are total on ℕ (they saturate at ℵ₀); % by a constant is too.
        N * N | (π2 == π1 + 1)
        evens = N | (π % 2 == 0)
        self.assertIn(4, evens)
        self.assertNotIn(5, evens)
        # ℕ has no total −, and a term divisor could be 0: refused.
        with self.assertRaises(TypeError):
            N * N | (π2 == π1 - 1)
        with self.assertRaises(TypeError):
            N * N | (π2 == π2 // π1)
        # A constant divisor is checked when the term is built.
        with self.assertRaises(ValueError):
            π % 0


if __name__ == "__main__":
    unittest.main()
