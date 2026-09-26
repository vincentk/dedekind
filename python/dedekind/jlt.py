"""Jlt: the composition-term exhibit (#961), first iteration.

A tiny point-free calculus of the unary boolean operations, exposed as *handles*
over the C++ core.  The objects are ``True`` / ``False``; the primitive arrows
are ``id`` and ``not_`` (``not`` is a Python keyword).  The reducer lives in C++
(value-first): ``simplify`` calls into ``ArrowTerm::reduce``; Python only
composes handles and asks C++ to normalise.  This mirrors the type-level
``cata`` in ``:f_algebra`` at runtime.

    from dedekind.jlt import id, not_, simplify

    simplify(id >> id) == id           # True:  id ∘ id = id
    simplify(not_ >> id) == not_       # True:  not ∘ id = not
    simplify(id >> not_) == not_       # True:  id ∘ not = not
    repr(simplify(not_ >> not_))       # '(>> not not)': inert (unit law only)
    not_(False)                        # True: apply to a boolean object

Scope (first iteration, #961): the monoid UNIT law only.  ``not`` is still
treated as an opaque non-identity arrow, so ``not ∘ not → id`` (the involution
law, using :involution's witness for ``std::logical_not``) is the next slice.
"""

# `_jlt` is the exhibit's own private native extension module (dedekind._jlt),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._jlt import Arrow
from ._jlt import id
from ._jlt import not_
from ._jlt import simplify

__all__ = ["Arrow", "id", "not_", "simplify"]
