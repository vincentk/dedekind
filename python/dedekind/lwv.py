"""Lwv: the scalar set-comprehension language (#965) -- the SET twin of ``jlt``.

Paper §3, ``Lwv := Jlt ∩ Set``: sets defined *intensionally*, by a membership
test ``{x ∈ S | P(x)}`` rather than by listing elements.  Iteration 2 is
**value-based**: a halfspace is a point plus a direction, and the pivot rides as
a runtime *value*, so a single constructor covers every pivot (unlike the
iteration-1 one-constant-per-pivot stopgap)::

    from dedekind.lwv import above, below, singleton

    gt = above(3)                # {x | x > 3}   -- runtime pivot
    4 in gt                      # True
    region = above(3) & below(5) # {x>3} ∩ {x<5}, through the lattice reducer
    region                       # {4}
    region == singleton(4)       # True
    region.cardinality           # 1

Constructors: ``above(k)`` / ``at_least(k)`` (``>`` / ``>=``), ``below(k)`` /
``at_most(k)`` (``<`` / ``<=``), ``singleton(k)`` (the point ``η(k)``, the
value-based atom), and ``everything()`` / ``nothing()`` (``𝔸`` / ``Ø``).
Membership is ``s(x)`` / ``x in s``; the meet, join and complement are
``a & b``, ``a | b`` and ``~a``.  A set is a run list, finitely many disjoint
intervals in order (``s.runs()``), the normal form of unions of intervals, so
``~singleton(5)`` and ``at_least(0) & ~singleton(5)`` are sets like any other.

Every operator folds through the **same** lattice reducer (``category::Reduce``
under ``cata``) the compile-time ``static_assert`` exhibits fold: its leaf
combine is the interval meet, so ``above(3) & below(5)`` runs the crossing law
``↑a ∩ ↓b = [a,b]`` (collapsing to the singleton ``{4}`` by integer
cardinality), and unions and complements are run lists, computed on the
run-time pivots.  No Python-side reducer.  This
is the compile-time / runtime *optionality*: one law, either phase, chosen by
the evaluation context.  Value-oriented relational grounding (the pivot never
lives in a type): ``{x | x ≤ p} == preimage₁( S × η(p) | (χ/π₁ ≤ π₂) )``.

"""

# `_lwv` is the exhibit's own private native extension module (dedekind._lwv),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._lwv import Set
from ._lwv import above
from ._lwv import image
from ._lwv import preimage
from ._lwv import at_least
from ._lwv import at_most
from ._lwv import below
from ._lwv import everything
from ._lwv import nothing
from ._lwv import singleton

__all__ = [
    "Set",
    "above",
    "image",
    "preimage",
    "at_least",
    "at_most",
    "below",
    "everything",
    "nothing",
    "singleton",
]
