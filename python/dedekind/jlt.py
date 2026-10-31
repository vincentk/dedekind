"""Jlt: the arrow DSL (#961) — involutive endomorphisms over two objects.

A fluent, point-free calculus over the *real* category arrows, exposed as
duck-typed handles: an "arrow" is the protocol ``__call__`` / ``__rshift__``
plus the free functions ``dom`` / ``cod``, not a base class.  The objects are
the two carriers ``bool`` and ``int``; the primitive arrows on a carrier ``T``
are ``id(T)`` (identity), ``refl(T)`` (:logic's reflection involution
``logic_complement``: ``not`` (¬) on ``bool``, the order-reversing ``~`` on the
``int`` chain), and the NNO step ``succ(T)`` / ``pred(T)`` (#1001), total and
saturating at the chain's bounds: ``succ(bool)(True) is True``.

    from dedekind.jlt import id, refl, dom, cod

    (id(bool) >> refl(bool))(False)   # True   -- compose, then apply
    (refl(int) >> refl(int))(5)       # 5      -- refl is an involution: r∘r = id
    dom(id(int)) is int               # True   -- dom/cod are type objects
    id(bool) >> refl(int)             # TypeError -- not composable (bool ≠ int)

The tensor exhibit (#1107) adds handles to ``:periodic``'s tensors: an
``alphabet("ACGT")``, its ``power(a, n)`` (the words of length n, never built),
``free_monoid(a, L)`` (the words shorter than L), and ``symmetric(p, sigma, tau)``
(the words fixed by a ``reversal()`` or ``rotation(d)`` of positions through a
``letter_map`` on letters).  ``count_factors`` gives a count as factors, each a
popcount; ``count`` its residue modulo 2**64; ``exact_count`` the count or
``None`` where no word holds it, so a genome of 2**200 words is refused as a
sequence and counted as a tensor.  ``*`` is the outer product.

Arrows are **extensional** (functions): ``id(T) >> f`` and ``f`` are equal on
every input.  Composability (``cod(f) == dom(g)``) is enforced structurally --
composing across objects raises.  Structural reduction (``simplify`` / ``cata``)
is *intensional* and lives on the C++ side; it is vacuous here.  ``id`` /
``refl`` are the real ``:morphism`` / involution arrows, witnessing ``IsArrow``
(and ``IsEndomorphism`` / ``IsInvolution``) in C++.
"""

# `_jlt` is the exhibit's own private native extension module (dedekind._jlt),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._jlt import alphabet
from ._jlt import cod
from ._jlt import count
from ._jlt import count_factors
from ._jlt import dom
from ._jlt import exact_count
from ._jlt import free_monoid
from ._jlt import id
from ._jlt import is_square
from ._jlt import letter_map
from ._jlt import pack
from ._jlt import power
from ._jlt import pred
from ._jlt import refl
from ._jlt import reversal
from ._jlt import rotation
from ._jlt import succ
from ._jlt import symmetric

__all__ = [
    "alphabet",
    "cod",
    "count",
    "count_factors",
    "dom",
    "exact_count",
    "free_monoid",
    "id",
    "is_square",
    "letter_map",
    "pack",
    "power",
    "pred",
    "refl",
    "reversal",
    "rotation",
    "succ",
    "symmetric",
]
