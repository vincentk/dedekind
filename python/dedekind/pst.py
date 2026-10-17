"""Pst: the universes over the finite carriers, and the sets over them.

``Pst := Jlt ∩ Chain`` (paper §3).  A universe is ``A(T)``, the set of every
value of a carrier type ``T``, its sets valued in 𝔹; the four carriers have
names, the same as in C++:

    B    = A(bool)      𝔹, the two-element truth chain {False < True}
    K3   = A(Ternary)   Kleene's chain {FALSE < UNKNOWN < TRUE}
    U64  = A(u64)       the ring ℤ/2⁶⁴, the machine word
    GF_p = A(𝔽ₚ)        the field GF(p), p = 2⁶⁴ − 59

``A(Ternary, Kleene)`` is the universe whose sets are valued in K₃.  The truth
chains' universes carry their order: the ends ``bottom`` / ``top``, the step
read twice (the total, saturating ``succ`` / ``pred``; the partial ``cover``,
``None`` at ⊤), the order ``le``, the classification as the C++ concepts decide
it, and iteration from ⊥ by the cover.

Sets over 𝔹 and K₃ are the chain fragment of the paper's Lwv grammar, spelled
as the paper spells it::

    from dedekind.pst import 𝔸, Ø, η, π, K3, Ternary, Kleene, ap, canonical, exists, forall
    U, T, F = Ternary.UNKNOWN, Ternary.TRUE, Ternary.FALSE

    S = K3 & (π > F)          # generator & pred      -> {U, ⊤}
    S == (K3 & (π >= U))      # True: one normal form, the run list [U, ⊤]
    ~S & η(T) | Ø(K3)            # ~ & | ^ on sets
    U in S;  ap(S, U)            # membership, χ: ap applies an arrow
    exists(K3, π > F)         # True;  forall(K3, π > F) is False
    canonical(S) == S;  repr(S)  # True; "[U, ⊤]": held as its run list

    H = K3.identity              # χ(x) = x, valued in K₃
    H == H                       # Ternary.UNKNOWN: reflexive only up to the
                                 # excluded middle, where the set is U
    K3 & (ap(H, π) >= U)      # the α-cut: χ applied to π, compared on the chain

Every handle over a truth chain is a run list, the library's normal form of
these sets, valued in its species (a K₃-valued set is a step function on the
chain); every operator and every query is the library's, on it.  The sets and
relations the term surface below cuts out of ``u64`` and ``𝔽ₚ`` hold the bound
term.  The laws decide the syntax: a carrier's certified varieties supply the
operators a term may use and the free algebra its normal form lives in, and
equality of terms is equality there; a chain's is read as run lists, a ring's
as polynomials (``canonical(S)``, a set in, the same set out, its equations
``p == 0`` with ``p`` a polynomial).  Equal normal forms cut out equal sets;
different ones decide nothing, so ``==`` answers ``True`` or raises.  A term
outside that catalogue evaluates but does not compare.  Reduction stays in C++.

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
``A(carrier) & term`` binds it, casting every constant into the carrier and
admitting every operator there or refusing it (``TypeError``), all when the
set is built.  The universes multiply into products of any arity, each column
its own sort (a relation is a table: one class, its schema as data); 𝔹 ↪ K₃
is the one coercion, any other two carriers meet through ``ap``::

    from dedekind.pst import A, U64, u64, 𝔽ₚ, B, ap, π, π_1, π_2, π_3
    from dedekind.trsk import Function        # the relational surface

    U = A(u64)
    evens = U & (π % 2 == 0)            # + − · wrap; // and % by a nonzero word
    4 in evens                          # True
    U * U & (π_2 == π_1 + 1)              # a relation on the product
    U * U & (π_2 == π_2 // π_1)            # TypeError: a term divisor could be 0
    U & (π > -1)                        # ValueError: the word cannot hold -1
    π % 0                               # ValueError, when the term is written
    F = A(𝔽ₚ)
    third = F * F & (π_2 == π_1 / 3)      # / by a nonzero constant: a field's
    ap(third, (3, 1))                   # True: 3 · 3⁻¹ = 1
    Function[u64, B](U64 * B & (π_2 == ap(U64 & (π > 5), π_1)))   # χ as a function
    fib = U64 * U64 * U64 & (π_3 == π_1 + π_2)    # Function[u64, u64, u64]
    ap(fib, (3, 5))                               # 8
    F & (π < 3)                         # TypeError: 𝔽ₚ has no order
    A(u64) & (π / 2 == 1)               # TypeError: 2 is not a unit of ℤ/2⁶⁴
    B & (π + True == True)              # TypeError: 𝔹's rig is (∨, ∧), it has no +

A set over ``u64`` built from ``π ⋈ k`` atoms under ``&`` and ``|`` is read as
its run list, so equality, ``canonical``, ``len``, iteration and slicing by value
are the library's; ``image`` / ``preimage`` under ``succ`` / ``pred`` / ``id``
(from ``dedekind.jlt``) substitute the variable, and the run list translates,
wrapping at the word's end::

    from dedekind.jlt import succ
    (U & (π > 3)) & (U & (π < 5)) == U & (π == 4)    # True: one normal form
    canonical(~(U & (π == 5)))     # 𝔸(u64) & ((π <= 4) | (π >= 6)): the runs, written
    list(U[3:8]);  len(U & (π < 3))                  # [3, 4, 5, 6, 7]; 3
    image(succ(int), U & (π > 5)) == U & ((π > 6) | (π == 0))   # True: wraps

Spelling: Python normalises identifiers (NFKC), so ``𝔸`` *is* the name ``A``
and ``𝔹`` is ``B``; ``Ø``, ``η``, ``π`` are names as
written.  Subscript digits and parentheses are not identifier characters, so
``K₃`` is ``K3``
and ``GF(p)`` is ``GF_p``.  ``∃`` / ``∀`` are not identifiers either: the
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
from ._pst import Ternary
from ._pst import exists
from ._pst import forall
from ._pst import image
from ._pst import lift
from ._pst import ap
from ._pst import canonical
from ._pst import cond
from ._pst import preimage
from ._pst import u64
from ._pst import U64
from ._pst import GF_p
from ._pst import Ø
from ._pst import η
from ._pst import π
from ._pst import π_1
from ._pst import π_2
from ._pst import π_3


class _Annotation:
    """A type annotation applied: a check at construction, which returns the
    value unchanged or raises TypeError, and an ``isinstance`` test.  The
    verdicts are the library's (a carrier, a codomain, a function's
    certificate); nothing is decided here."""

    __slots__ = ("_name", "_holds")

    def __init__(self, name, holds):
        self._name, self._holds = name, holds

    def __call__(self, x):
        if not self._holds(x):
            raise TypeError(f"{x!r} is not a {self._name}")
        return x

    def __instancecheck__(self, x):
        return self._holds(x)

    def __repr__(self):
        return self._name


def _named(c):
    return getattr(c, "__name__", None) or repr(c)


def _attr(x, name):
    try:
        return getattr(x, name)
    except Exception:  # noqa: BLE001 (a value without the attribute is not one)
        return None


def _is_set_on(x, c):
    return _attr(x, "factors") is None and _attr(x, "carrier") is not None and _attr(x, "carrier") == c


class Set:
    """``Set[C]``: a set whose carrier is ``C``."""

    def __class_getitem__(cls, c):
        return _Annotation(f"Set[{_named(c)}]", lambda x: _is_set_on(x, c))


# The aliases: deliberately not in __all__ (they shadow the builtins when
# star-imported); `from dedekind.pst import any, all` is the explicit opt-in.
any = exists  # noqa: A001
all = forall  # noqa: A001

__all__ = [
    "A", "B", "K3", "U64", "GF_p", "Ternary", "u64", "Fp", "Boole", "Kleene",
    "Ø", "η", "π", "π_1", "π_2", "π_3", "Term", "Pred", "exists", "forall", "ap", "canonical", "cond", "Set",
    "lift", "image", "preimage",
]
