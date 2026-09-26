"""Jlt: the arrow DSL (#961), first iteration — the unary boolean operations.

A fluent, point-free calculus over the *real* category arrows, exposed as
duck-typed handles: an "arrow" is the protocol ``__call__`` / ``__rshift__`` /
``dom`` / ``cod``, not a base class.  The objects are the booleans; the
primitive arrows are ``id`` and ``not_`` (``not`` is a Python keyword).

    from dedekind.jlt import id, not_

    (id >> not_)(False)      # True   -- compose (apply id, then not), then apply
    (not_ >> not_)(True)     # True   -- not is an involution: not∘not = id
    dom(id) is bool          # True   -- dom/cod are free fns returning a type
    isinstance(id >> not_, Morphism)   # composition type-erases to a Morphism

Arrows are **extensional** (functions): ``id >> not_`` and ``not_`` are equal on
every input.  Structural reduction (``simplify`` / ``cata``) is *intensional*
and lives on the C++ side (the type-level ``:f_algebra`` reducer); it is vacuous
on extensional arrows, so it is deliberately absent here.  ``Identity`` and
``Morphism`` are the real ``:morphism`` arrow types, both witnessing ``IsArrow``
in C++.
"""

# `_jlt` is the exhibit's own private native extension module (dedekind._jlt),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._jlt import Identity
from ._jlt import Morphism
from ._jlt import cod
from ._jlt import dom
from ._jlt import id
from ._jlt import not_

__all__ = ["Identity", "Morphism", "cod", "dom", "id", "not_"]
