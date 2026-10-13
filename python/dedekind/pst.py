"""Pst: the bounded chains the sets are valued in, and the sets over them.

``Pst := Jlt ∩ Chain`` (paper §3).  Three chains, handles over the real C++
carriers: ``B`` (𝔹 = {False < True}) and ``K3`` (Kleene's
{FALSE < UNKNOWN < TRUE}) are truth objects; ``N`` is ℕ's proxy, a bounded
chain of the same shape whose top is ``ℵ_0``.  A chain exposes its ends
``bottom`` / ``top``, its step read twice (the total, saturating ``succ`` /
``pred``; the partial ``cover``, ``None`` at ⊤), its order ``le``, its
classification as the C++ concepts decide it, and iterates from ⊥ by the cover.

Sets over 𝔹 and K₃ are the chain fragment of the paper's Lwv grammar, spelled
as the paper spells it (scalar carriers; products and composed predicates are
not bound here)::

    from dedekind.pst import 𝔸, Ø, η, π, K3, Ternary, Kleene, exists, forall
    U, T, F = Ternary.UNKNOWN, Ternary.TRUE, Ternary.FALSE

    S = 𝔸(K3) | (π > F)          # generator | pred      -> {U, ⊤}
    S == (𝔸(K3) | (π >= U))      # True: one normal form, the run list [U, ⊤]
    ~S & η(T) | Ø(K3)            # ~ & | ^ on sets
    U in S;  S(U)                # membership, χ
    exists(𝔸(K3), π > F)         # True;  forall(𝔸(K3), π > F) is False
    S.runs();  repr(S)           # [(UNKNOWN, TRUE)]; "[U, ⊤]"   the normal form

    H = K3.identity              # χ(x) = x, valued in K₃
    H == H                       # Ternary.UNKNOWN: reflexive only up to the
                                 # excluded middle, where the set is U
    H.cut(U) == (𝔸(K3) | (π >= U))   # the α-cut, a decidable set: True

Every handle is a run list, the library's normal form of these sets, valued in
its species (a K₃-valued set is a step function on the chain); every operator
and every query is the library's, on it.  Reduction stays in C++.

The canonical ring and field, each as large as a machine word allows::

    from dedekind.pst import 𝔸, u64, 𝔽ₚ   # 𝔽ₚ is the name Fp (NFKC)

    u64(2**64 - 1) + 1          # u64(0): the ring ℤ/2⁶⁴ wraps
    u64(7) // 2;  u64(7) % 2    # by a nonzero word; u64(7) / 2 is a TypeError
    𝔽ₚ(1) / 3 * 3               # 𝔽ₚ(1): p = 2⁶⁴ − 59, the largest prime < 2⁶⁴
    𝔽ₚ(-1);  int(𝔽ₚ(-1))        # 𝔽ₚ(-1); -1: the symmetric representative
    5 in 𝔸(u64);  -1 in 𝔸(𝔽ₚ)   # True; True: ints that are representatives
    u64.is_field;  𝔽ₚ.is_field  # False; True: the C++ concepts' verdict

Spelling: Python normalises identifiers (NFKC), so ``𝔸`` *is* the name ``A``
and ``𝔹`` / ``ℕ`` are ``B`` / ``N``; ``Ø``, ``η``, ``π``, ``χ`` are names as
written.  Subscript digits are not identifier characters, so ``K₃`` is ``K3``
and ``ℵ₀`` is ``ℵ_0`` (the C++ spelling; ``aleph0`` remains an alias).  ``∃`` / ``∀`` are not identifiers either: the
grammar's words ``exists`` / ``forall``, with ``any`` / ``all`` as aliases
(importable by name; not star-exported, since they shadow the builtins).
"""

# `_pst` is the private native extension module (dedekind._pst), NumPy-style;
# this pure-Python facade re-exports it under the public name.
from ._pst import A
from ._pst import B
from ._pst import Fp
from ._pst import Boole
from ._pst import K3
from ._pst import Kleene
from ._pst import N
from ._pst import Ternary
from ._pst import aleph0
from ._pst import exists
from ._pst import forall
from ._pst import lift
from ._pst import runs
from ._pst import u64
from ._pst import Ø
from ._pst import η
from ._pst import π
from ._pst import χ

# ℵ₀ under its own symbol: ℵ_0 is an identifier (NFKC: א_0), the subscript
# digit is not, so the C++ spelling ℵ_0 is the Python one too.
ℵ_0 = aleph0

# The aliases: deliberately not in __all__ (they shadow the builtins when
# star-imported); `from dedekind.pst import any, all` is the explicit opt-in.
any = exists  # noqa: A001
all = forall  # noqa: A001

__all__ = [
    "A", "B", "K3", "N", "Ternary", "u64", "Fp", "ℵ_0", "aleph0", "Boole", "Kleene",
    "Ø", "η", "π", "χ", "exists", "forall", "runs", "lift",
]
