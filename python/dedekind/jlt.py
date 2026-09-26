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

# `jlt` is a native submodule of the `_dedekind` extension (def_submodule), so
# it is reachable as an attribute of the base module; access it that way rather
# than `from ._dedekind.jlt import ...` (which would rely on sys.modules
# registration).
from ._dedekind import jlt as _jlt

Arrow = _jlt.Arrow
id = _jlt.id
not_ = _jlt.not_
simplify = _jlt.simplify

__all__ = ["Arrow", "id", "not_", "simplify"]
