"""Jlt: the arrow DSL (#961) — involutive endomorphisms over two objects.

A fluent, point-free calculus over the *real* category arrows, exposed as
duck-typed handles: an "arrow" is the protocol ``__call__`` / ``__rshift__``
plus the free functions ``dom`` / ``cod``, not a base class.  The objects are
the two carriers ``bool`` and ``int``; the primitive arrows on a carrier ``T``
are ``id(T)`` (identity) and ``refl(T)`` (the reflection involution: ``not`` on
``bool``, negation on ``int``).

    from dedekind.jlt import id, refl, dom, cod

    (id(bool) >> refl(bool))(False)   # True   -- compose, then apply
    (refl(int) >> refl(int))(5)       # 5      -- refl is an involution: r∘r = id
    dom(id(int)) is int               # True   -- dom/cod are type objects
    id(bool) >> refl(int)             # TypeError -- not composable (bool ≠ int)

Arrows are **extensional** (functions): ``id(T) >> f`` and ``f`` are equal on
every input.  Composability (``cod(f) == dom(g)``) is enforced structurally --
composing across objects raises.  Structural reduction (``simplify`` / ``cata``)
is *intensional* and lives on the C++ side; it is vacuous here.  ``id`` /
``refl`` are the real ``:morphism`` / involution arrows, witnessing ``IsArrow``
(and ``IsEndomorphism`` / ``IsInvolution``) in C++.
"""

# `_jlt` is the exhibit's own private native extension module (dedekind._jlt),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._jlt import cod
from ._jlt import dom
from ._jlt import id
from ._jlt import refl

__all__ = ["cod", "dom", "id", "refl"]
