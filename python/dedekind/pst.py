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

    from dedekind.pst import 𝔹, K₃, ℕ, ℵ₀, Ternary

    list(𝔹)                      # [False, True]       -- unfold ⊥ by the cover
    list(K₃)                     # [FALSE, UNKNOWN, TRUE]
    K₃.cover(Ternary.TRUE)       # None                -- ⊤ has no cover
    K₃.succ(Ternary.TRUE)        # TRUE                -- the total step saturates
    ℕ.succ(41)                   # 42
    ℕ.succ(ℵ₀)                   # ℵ₀                  -- saturating at the top
    ℕ.pred(0)                    # 0                   -- the monus
    itertools.islice(ℕ, 5)       # 0, 1, 2, 3, 4       -- ℕ's ⊤ is a limit: iteration never reaches it
    ℕ.is_truth_object            # False               -- a chain, not a truth object
    𝔹.is_dense                   # False               -- a chain with the step is discrete

Iterating a chain is the unfold of its bottom by the cover and stops where the
cover stops; ``StopIteration`` is the Python spelling of the cover's ``None``.
Reduction stays in C++; Python holds handles.

Spelling note (NFKC): Python normalises identifiers, so ``𝔹``, ``ℕ`` and
``K₃`` in source *are* the names ``B``, ``N`` and ``K3``; ``ℵ₀`` normalises to
the Hebrew-letter spelling, which is why it is bound below under its own
written form as well as the ASCII ``aleph0``.  Either spelling reaches the same
object.
"""

import unicodedata as _unicodedata

# `_pst` is the exhibit's own private native extension module (dedekind._pst),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._pst import B
from ._pst import K3
from ._pst import N
from ._pst import Ternary
from ._pst import aleph0

ℵ₀ = aleph0  # the written form; the identifier is its NFKC normalisation

__all__ = [
    "B",
    "K3",
    "N",
    "Ternary",
    "aleph0",
    _unicodedata.normalize("NFKC", "ℵ₀"),
]
