"""Lwv: the scalar set-comprehension language (#965) -- the SET twin of ``jlt``.

Paper §3, ``Lwv := Jlt ∩ Set``: sets defined *intensionally*, by a membership
test ``{x ∈ S | P(x)}`` rather than by listing elements.  A halfspace is the
principal filter/ideal of the carrier order; the README exhibit is the two
overlapping halfspaces collapsing to a singleton::

    from dedekind.lwv import gt_3, lt_5

    gt_3(4)              # True   -- 4 ∈ {x > 3}
    4 in lt_5            # True   -- 4 ∈ {x < 5}
    collapse = gt_3 & lt_5   # {x>3} ∩ {x<5}, through the :order reducer
    collapse(4)          # True   -- the only inhabitant
    collapse(3)          # False
    collapse.cardinality # 1      -- |{4}| = 1
    repr(collapse)       # '{4}'

The intersection ``&`` dispatches to the **real** C++ ``:order`` reducer
(``structured_and``, the value-first crossing law ``↑a ∩ ↓b = [a,b]``), which
collapses ``{x>3} ∩ {x<5}`` to the singleton ``{4}`` by cardinality analysis --
the exact collapse the compile-time ``static_assert`` exhibit folds, now
observed at Python runtime.  There is no Python-side reducer (handle-only): the
handle *dispatches* to the C++ one, as ``jlt``'s ``>>`` dispatches to ``cata``.

This is a first iteration.  The pivots are compile-time, so it binds the curated
README sets -- exactly as ``jlt`` binds the fixed generators ``id`` / ``refl``.
A general fluent constructor over *runtime* pivots awaits the value-first
subobject reducer's leaf-combine leg (#922 slice 2): a runtime pivot cannot
index a compile-time halfspace type.
"""

# `_lwv` is the exhibit's own private native extension module (dedekind._lwv),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._lwv import gt_3
from ._lwv import lt_5

__all__ = ["gt_3", "lt_5"]
