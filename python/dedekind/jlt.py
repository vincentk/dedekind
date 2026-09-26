"""Jlt: the composition-term exhibit (#961).

A tiny point-free arrow calculus over ``int`` endo-maps, exposed as *handles*
over the C++ core.  The reducer lives in C++ (value-first): ``simplify`` calls
into ``ArrowTerm::reduce``; Python only composes handles and asks C++ to
normalise.  This mirrors the type-level ``cata`` in ``:f_algebra`` at runtime.

    from dedekind.jlt import id, atom, simplify

    simplify(id >> id) == id          # True:  id ∘ id = id
    f = atom("f", lambda x: x + 1)
    simplify(f >> id) == f            # True:  f ∘ id = f
    simplify(id >> f) == f            # True:  id ∘ f = f
    repr(simplify(f >> f))            # '(>> f f)': two non-units stay inert
    (id >> f)(41)                     # 42: apply runs the wrapped map

Scope (first slice, #961): the monoid UNIT law only.  Atoms are opaque, so
there is no inverse-cancellation / involution law yet (that is the next slice).
"""

from ._dedekind import Arrow
from ._dedekind import atom
from ._dedekind import id
from ._dedekind import simplify

__all__ = ["Arrow", "atom", "id", "simplify"]
