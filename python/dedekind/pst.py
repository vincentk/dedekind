"""Pst: the bounded chains the sets are valued in, and the sets over them.

``Pst := Jlt ∩ Chain`` (paper §3).  Three chains, handles over the real C++
carriers: ``B`` (𝔹 = {False < True}) and ``K3`` (Kleene's
{FALSE < UNKNOWN < TRUE}) are truth objects; ``N`` is ℕ's proxy, a bounded
chain of the same shape whose top is ``ℵ_0``.  A chain exposes its ends
``bottom`` / ``top``, its step read twice (the total, saturating ``succ`` /
``pred``; the partial ``cover``, ``None`` at ⊤), its order ``le``, its
classification as the C++ concepts decide it, and iterates from ⊥ by the cover.

Sets over 𝔹 and K₃ are the chain fragment of the paper's Lwv grammar, spelled
as the paper spells it::

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

Every handle over a truth chain is a run list, the library's normal form of
these sets, valued in its species (a K₃-valued set is a step function on the
chain); every operator and every query is the library's, on it.  The sets and
relations the term surface below cuts out of ``u64`` and ``𝔽ₚ`` hold the bound
term.  The laws decide the syntax: a carrier's certified varieties supply the
operators a term may use and the free algebra its normal form lives in, and
equality of terms is equality there; a chain's is read as run lists, a ring's
polynomials are #1023.  A term outside that catalogue evaluates but does not
compare.  Reduction stays in C++.

The canonical ring and field, each as large as a machine word allows::

    from dedekind.pst import 𝔸, u64, 𝔽ₚ   # 𝔽ₚ is the name Fp (NFKC)

    u64(2**64 - 1) + 1          # u64(0): the ring ℤ/2⁶⁴ wraps
    u64(7) // 2;  u64(7) % 2    # by a nonzero word; u64(7) / 2 is a TypeError
    𝔽ₚ(1) / 3 * 3               # 𝔽ₚ(1): p = 2⁶⁴ − 59, the largest prime < 2⁶⁴
    𝔽ₚ(-1);  int(𝔽ₚ(-1))        # 𝔽ₚ(-1); -1: the symmetric representative
    5 in 𝔸(u64);  -1 in 𝔸(𝔽ₚ)   # True; True: ints that are representatives
    𝔽ₚ(-1) == -1                # True, and 𝔽ₚ(-1) != p - 1: equal to the
                                # int it represents, so hashing agrees
    u64.is_field;  𝔽ₚ.is_field  # False; True: the C++ concepts' verdict

The term surface (#1006, exhibit 3): a term names no carrier, and the former
``A(carrier) | term`` binds it, casting every constant into the carrier and
admitting every operator there or refusing it (``TypeError``), all when the
set is built::

    from dedekind.pst import A, u64, 𝔽ₚ, B, π, π1, π2

    U = A(u64)
    evens = U | (π % 2 == 0)            # + − · wrap; // and % by a nonzero word
    4 in evens                          # True
    U * U | (π2 == π1 + 1)              # a relation on the product
    U * U | (π2 == π2 // π1)            # TypeError: a term divisor could be 0
    U | (π > -1)                        # ValueError: the word cannot hold -1
    π % 0                               # ValueError, when the term is written
    F = A(𝔽ₚ)
    third = F * F | (π2 == π1 / 3)      # / by a nonzero constant: a field's
    third((3, 1))                       # True: 3 · 3⁻¹ = 1
    F | (π < 3)                         # TypeError: 𝔽ₚ has no order
    A(u64) | (π / 2 == 1)               # TypeError: 2 is not a unit of ℤ/2⁶⁴
    A(B) | (π + 1 == True)              # TypeError: 1 is not a bool (𝔹's + is ∨)

A set over ``u64`` built from ``π ⋈ k`` atoms under ``&`` and ``|`` is read as
its run list, so equality, ``runs()``, ``len``, iteration and slicing by value
are the library's; ``image`` / ``preimage`` under ``succ`` / ``pred`` / ``id``
(from ``dedekind.jlt``) substitute the variable, and the run list translates,
wrapping at the word's end::

    from dedekind.jlt import succ
    (U | (π > 3)) & (U | (π < 5)) == U | (π == 4)    # True: one normal form
    (~(U | (π == 5))).runs()                         # [(0, 4), (6, 2**64 - 1)]
    list(U[3:8]);  len(U | (π < 3))                  # [3, 4, 5, 6, 7]; 3
    image(succ, U | (π > 5)) == U | ((π > 6) | (π == 0))   # True: it wraps

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
from ._pst import Pred
from ._pst import Term
from ._pst import Boole
from ._pst import K3
from ._pst import Kleene
from ._pst import N
from ._pst import Ternary
from ._pst import aleph0
from ._pst import exists
from ._pst import forall
from ._pst import image
from ._pst import lift
from ._pst import preimage
from ._pst import runs
from ._pst import u64
from ._pst import Ø
from ._pst import η
from ._pst import π
from ._pst import π1
from ._pst import π2
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
    "Ø", "η", "π", "π1", "π2", "χ", "Term", "Pred", "exists", "forall", "runs",
    "lift", "image", "preimage",
]
