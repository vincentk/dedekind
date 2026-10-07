"""The bounded Collatz exhibit (#861): the conjecture, undecided in general and
decided on every finite prefix with a budget.

``T(n) = n/2`` if ``n`` is even, ``3n+1`` if odd.  ``{n | the orbit reaches 1}``
is de facto undecidable: universality is the open Collatz conjecture, the
generalised problem is provably undecidable (Conway, 1972), and the library has
no former for the unbounded closure, so no object exists to ask.  What exists is
the budgeted verdict, valued in K₃, and the orbit itself::

    from dedekind.collatz import collatz, reaches_within, forall, at_least, below, everything
    from dedekind.collatz import Ternary
    U, T = Ternary.UNKNOWN, Ternary.TRUE

    o = collatz(27)                  # the orbit, a lazy path
    o[:6]                            # [27, 82, 41, 124, 62, 31]
    o.reach_time(120)                # 111; o.reach_time(50) is None
    reaches_within(50)(27)           # UNKNOWN: not yet
    reaches_within(120)(27)          # TRUE: seen to arrive; never FALSE

    W = at_least(1) & below(1000)    # the window [1, 1000)
    forall(W, reaches_within(177))   # UNKNOWN (871 needs 178 steps)
    forall(W, reaches_within(178))   # TRUE: the window is decided
    forall(everything(), reaches_within(300))   # TypeError: no window

The ∀ answers in K₃ by construction.  TRUE on a window means the verdict there
takes only the values ⊥ and ⊤, so the set has collapsed along 𝔹 ↪ K₃ into a
Boolean one: decidability on the prefix, read off the value.  UNKNOWN means the
budget has not decided it yet.  Nothing answers FALSE, and nothing quantifies
over all of ℕ: the conjecture is the quantifier swap ∀n ∃B that no window
performs.

Handles only: the orbit is the C++ ``iterate``, the verdict is
``numbers:collatz``'s, the ∀ is the meet along the ``lwv`` window.
"""

# The verdicts are Kleene's three values, bound by the Pst module; importing it
# first registers the type the native module below returns.
from .pst import Ternary
from .lwv import Set
from .lwv import at_least
from .lwv import at_most
from .lwv import above
from .lwv import below
from .lwv import everything
from .lwv import nothing
from .lwv import singleton
from ._collatz import Orbit
from ._collatz import ReachesWithin
from ._collatz import collatz
from ._collatz import forall
from ._collatz import reaches_within

__all__ = [
    "Orbit",
    "ReachesWithin",
    "Set",
    "Ternary",
    "above",
    "at_least",
    "at_most",
    "below",
    "collatz",
    "everything",
    "forall",
    "nothing",
    "reaches_within",
    "singleton",
]
