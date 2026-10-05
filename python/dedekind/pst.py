"""Pst: the bounded chains the sets are valued in (#1001, paper §3).

``Pst := Jlt ∩ Chain``: the truth objects ``B`` (𝔹 = {False < True}) and ``K3``
(Kleene's {FALSE < UNKNOWN < TRUE}), and ``N``, ℕ's proxy, a bounded chain of
the same shape whose top is ``aleph0`` (the memory boundary), not a truth
object.  A chain is a duck-typed handle over the real C++ carrier: its
endpoints ``bottom`` / ``top``, its step read twice -- total and saturating
(``succ`` / ``pred``, the algebra side) and partial (``cover``, ``None`` at ⊤,
the coalgebra side: ``N → 1 + N``) -- its order ``le``, and its classification
as the C++ concepts decide it (``is_bounded``, ``is_truth_object``,
``is_dense``, ``saturates``, ``cardinality``).

    from dedekind.pst import B, K3, N, Ternary, aleph0

    list(B)                      # [False, True]       -- unfold ⊥ by the cover
    list(K3)                     # [FALSE, UNKNOWN, TRUE]
    K3.cover(Ternary.TRUE)       # None                -- ⊤ has no cover
    K3.succ(Ternary.TRUE)        # TRUE                -- the total step saturates
    N.succ(41)                   # 42
    N.succ(aleph0)               # ℵ₀                  -- saturating at the top
    N.pred(0)                    # 0                   -- the monus
    itertools.islice(N, 5)       # 0, 1, 2, 3, 4       -- ℕ's ⊤ is a limit: iteration never reaches it
    N.is_truth_object            # False               -- a chain, not a truth object
    B.is_dense                   # False               -- a chain with the step is discrete

Iterating a chain is the unfold of its bottom by the cover and stops where the
cover stops; ``StopIteration`` is the Python spelling of the cover's ``None``.
Reduction stays in C++; Python holds handles.
"""

# `_pst` is the exhibit's own private native extension module (dedekind._pst),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._pst import B
from ._pst import K3
from ._pst import N
from ._pst import Ternary
from ._pst import aleph0

__all__ = ["B", "K3", "N", "Ternary", "aleph0"]
