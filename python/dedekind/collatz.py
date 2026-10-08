"""The bounded Collatz exhibit (#861), built from the library's arrows.

The rule is a term in the arrows on ℕ's machine shadow: ``π`` is the identity
arrow, ``π % 2 == 0`` a Boolean test, ``π // 2`` and ``3*π + 1`` arrows after
it, and ``cond(p, f, g)`` McCarthy's conditional::

    from dedekind.collatz import π, cond, iterate, Σ, reaches_within, forall
    from dedekind.collatz import at_least, below, everything, Ternary
    U, T = Ternary.UNKNOWN, Ternary.TRUE

    step = cond(π % 2 == 0, π // 2, 3*π + 1)   # the rule; collatz_step is the library's own
    step(27)                                  # 82
    o = iterate(step, 27)                     # the orbit, a lazy path
    o[:6]                                     # [27, 82, 41, 124, 62, 31]
    o.first_where(π == 1, 120)                # 111: the bounded search; None at budget 50
    Σ(True), Σ(False)                         # TRUE, UNKNOWN: a search read in K₃, never FALSE
    P = reaches_within(step, 120)             # Σ ∘ first_where(π == 1, 120) ∘ iterate(step)
    P(27)                                     # TRUE; reaches_within(step, 50)(27) is UNKNOWN

    W = at_least(1) & below(1000)             # the window [1, 1000)
    forall(W, reaches_within(step, 177))      # UNKNOWN: 871 needs 178 steps
    forall(W, reaches_within(step, 178))      # TRUE: the window is decided
    forall(everything(), reaches_within(step, 300))   # TypeError: no window

Change one constant and the computation changes with it:
``cond(π % 2 == 0, π // 2, 5*π + 1)`` is the 5n+1 rule, whose window ∀ stays
UNKNOWN at every budget.  Handles only: the arrows are the library's, the
iterate is ``sequences::iterate``, the search ``first_where``, Σ the dominance,
and the ∀ the meet along the window.
"""

# Kleene's three values are bound by the Pst module; importing it first
# registers the type the native module below returns.
from .pst import Ternary
from .lwv import Set
from .lwv import above
from .lwv import at_least
from .lwv import at_most
from .lwv import below
from .lwv import everything
from .lwv import nothing
from .lwv import singleton
from ._collatz import ArrowN
from ._collatz import PathN
from ._collatz import PredN
from ._collatz import ReachesWithin
from ._collatz import collatz_step
from ._collatz import cond
from ._collatz import forall
from ._collatz import identity
from ._collatz import iterate
from ._collatz import reaches_within
from ._collatz import sigma
from ._collatz import Σ
from ._collatz import π

__all__ = [
    "ArrowN",
    "PathN",
    "PredN",
    "ReachesWithin",
    "Set",
    "Ternary",
    "above",
    "at_least",
    "at_most",
    "below",
    "collatz_step",
    "cond",
    "everything",
    "forall",
    "identity",
    "iterate",
    "nothing",
    "reaches_within",
    "sigma",
    "singleton",
    "Σ",
    "π",
]
