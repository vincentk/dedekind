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
as the paper spells it.  A set is held as its run list, its normal form, and
``ext`` (from ``pynzli``) materialises it, a ``frozenset``; a K₃-valued set is
its χ, a read-only map:

>>> from pynzli import ext
>>> from pynzli.pst import 𝔸, Ø, η, π, K3, Ternary, Kleene, ap, canonical, exists, forall, identity
>>> U, T, F = Ternary.UNKNOWN, Ternary.TRUE, Ternary.FALSE
>>> S = K3 & (π > F)  # generator & pred
>>> S  # the author wrote π > F; the set holds its run list
[U, ⊤]
>>> S == (K3 & (π >= U)), canonical(S) == S  # one normal form
(True, True)
>>> ~S, ~S | η(T), ~S & η(T) | Ø(K3)  # ~ & | ^ on sets
({⊥}, {⊥} ∪ {⊤}, Ø)
>>> U in S, ap(S, U), exists(K3, π > F), forall(K3, π > F)  # χ: ap applies an arrow
(True, True, True, False)
>>> ext(S) == {U, T}, type(ext(S)).__name__
(True, 'frozenset')
>>> H = identity(K3)  # χ(x) = x, valued in K₃
>>> H
{χ ≥ ⊤}: {⊤}; {χ ≥ U}: [U, ⊤]
>>> (H == H) == U  # reflexive only up to the excluded middle, where the set is U
True
>>> K3 & (ap(H, π) >= U)  # the α-cut: χ applied to π, compared on the chain
[U, ⊤]
>>> ext(H) == {F: F, U: U, T: T}, type(ext(H)).__name__
(True, 'mappingproxy')

A set, a chain and a carrier are read by free functions, not properties
(#1054): ``carrier(S)``, ``universe(S)``, ``codomain(S)``,
``cardinality(S)``, ``is_decidable(S)``; on a truth chain ``bottom``, ``top``,
``identity``, ``name`` and the verdicts ``is_bounded``, ``is_dense``,
``is_truth_object``, ``saturates``; on a carrier ``is_ring``, ``is_field``,
``modulus``.

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

The canonical ring and field, each as large as a machine word allows:

>>> from pynzli.pst import 𝔸, is_field, modulus, u64, 𝔽ₚ  # 𝔽ₚ is the name Fp (NFKC)
>>> u64(2**64 - 1) + 1  # the ring ℤ/2⁶⁴ wraps
u64(0)
>>> u64(7) // 2, u64(7) % 2  # by a nonzero word
(u64(3), u64(1))
>>> u64(7) / 2  # doctest: +IGNORE_EXCEPTION_DETAIL
Traceback (most recent call last):
TypeError: a word divides by // and %
>>> 𝔽ₚ(1) / 3 * 3  # p = 2⁶⁴ − 59, the largest prime < 2⁶⁴
𝔽ₚ(1)
>>> 𝔽ₚ(-1), int(𝔽ₚ(-1))  # the symmetric representative
(𝔽ₚ(-1), -1)
>>> 5 in 𝔸(u64), -1 in 𝔸(𝔽ₚ)  # ints that are representatives
(True, True)
>>> p = modulus(𝔽ₚ)
>>> 𝔽ₚ(-1) == -1, 𝔽ₚ(-1) != p - 1  # equal to the int it represents, so hashing agrees
(True, True)
>>> is_field(u64), is_field(𝔽ₚ)  # the C++ concepts' verdict
(False, True)

The term surface (#1006, exhibit 3): a term names no carrier, and the former
``A(carrier) & term`` binds it, casting every constant into the carrier and
admitting every operator there or refusing it (``TypeError``), all when the
set is built.  The universes multiply into products of any arity, each column
its own sort (a relation is a table: one class, its schema as data); 𝔹 ↪ K₃
is the one coercion, any other two carriers meet through ``ap``.  ``ext``
materialises what fits, a function as a read-only map, and refuses what does
not, naming its size:

>>> from pynzli.pst import A, U64, GF_p, u64, 𝔽ₚ, B, ap, cond, π, π_1, π_2, π_3
>>> from pynzli.trsk import Function  # the relational surface
>>> U = A(u64)
>>> evens = U & (π % 2 == 0)  # + − · wrap; // and % by a nonzero word
>>> 4 in evens, 5 in evens
(True, False)
>>> step = U * U & (π_2 == π_1 + 1)  # a relation on the product
>>> (3, 4) in step, (3, 5) in step
(True, False)
>>> U * U & (π_2 == π_2 // π_1)  # doctest: +IGNORE_EXCEPTION_DETAIL
Traceback (most recent call last):
TypeError: a term divisor could be 0
>>> U & (π > -1)  # doctest: +IGNORE_EXCEPTION_DETAIL
Traceback (most recent call last):
ValueError: the word cannot hold -1
>>> π % 0  # doctest: +IGNORE_EXCEPTION_DETAIL
Traceback (most recent call last):
ValueError: a division by 0, when the term is written
>>> F = A(𝔽ₚ)
>>> third = F * F & (π_2 == π_1 / 3)  # / by a nonzero constant: a field's
>>> ap(third, (3, 1))  # 3 · 3⁻¹ = 1
True
>>> chi = Function[u64, B](U64 * B & (π_2 == ap(U64 & (π > 5), π_1)))  # χ as a function
>>> ap(chi, 7), ap(chi, 2)
(True, False)
>>> fib = U64 * U64 * U64 & (π_3 == π_1 + π_2)  # Function[u64, u64, u64]
>>> ap(fib, (3, 5))
8
>>> one_hot = B * GF_p & (π_2 == cond(π_1 == True, 1, 0))  # a function from 𝔹
>>> ext(one_hot) == {False: 0, True: 1}, type(ext(one_hot)).__name__
(True, 'mappingproxy')
>>> sorted(ext(U & (π < 3)))
[0, 1, 2]
>>> ext(U)
Traceback (most recent call last):
ValueError: ext: 18446744073709551616 elements, past the limit 1000 (pynzli.options.limit)
>>> ext(evens)  # not counted: its universe bounds it
Traceback (most recent call last):
ValueError: ext: 18446744073709551616 elements in its universe, past the limit 1000 (pynzli.options.limit)
>>> F & (π < 3)  # doctest: +IGNORE_EXCEPTION_DETAIL
Traceback (most recent call last):
TypeError: 𝔽ₚ has no order
>>> A(u64) & (π / 2 == 1)  # doctest: +IGNORE_EXCEPTION_DETAIL
Traceback (most recent call last):
TypeError: 2 is not a unit of ℤ/2⁶⁴
>>> B & (π + 1 == True)  # doctest: +IGNORE_EXCEPTION_DETAIL
Traceback (most recent call last):
TypeError: 𝔹's rig is (∨, ∧), it has no +

A set over ``u64`` built from ``π ⋈ k`` atoms under ``&`` and ``|`` is read as
its run list, so equality, ``canonical``, ``len``, iteration and slicing by value
are the library's; ``image`` / ``preimage`` under ``succ`` / ``pred`` / ``id``
(from ``pynzli.jlt``) substitute the variable, and the run list translates,
wrapping at the word's end:

>>> from pynzli.jlt import succ
>>> (U & (π > 3)) & (U & (π < 5)) == U & (π == 4)  # one normal form
True
>>> canonical(~(U & (π == 5)))  # the author wrote ~(π == 5); the runs, written
𝔸(u64) & ((π <= 4) | (π >= 6))
>>> everything = U & (π >= 0)
>>> list(everything[3:8]), len(U & (π < 3))
([3, 4, 5, 6, 7], 3)
>>> image(succ(int), U & (π > 5)) == U & ((π > 6) | (π == 0))  # wraps
True

Spelling: Python normalises identifiers (NFKC), so ``𝔸`` *is* the name ``A``
and ``𝔹`` is ``B``; ``Ø``, ``η``, ``π`` are names as
written.  Subscript digits and parentheses are not identifier characters, so
``K₃`` is ``K3``
and ``GF(p)`` is ``GF_p``.  ``∃`` / ``∀`` are not identifiers either: the
grammar's words ``exists`` / ``forall``, with ``any`` / ``all`` as aliases
(importable by name; not star-exported, since they shadow the builtins).
"""

from array import array
from collections.abc import Sequence
from functools import cache
from operator import index

# `_pst` is the private native extension module (pynzli._pst), NumPy-style;
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
from ._pst import bottom
from ._pst import cardinality
from ._pst import carrier
from ._pst import codomain
from ._pst import identity
from ._pst import is_bounded
from ._pst import is_decidable
from ._pst import is_dense
from ._pst import is_field
from ._pst import is_ring
from ._pst import is_truth_object
from ._pst import modulus
from ._pst import name
from ._pst import saturates
from ._pst import top
from ._pst import universe


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
    return _attr(x, "_factors") is None and _attr(x, "_carrier") is not None and _attr(x, "_carrier") == c


class Set:
    """``Set[C]``: a set whose carrier is ``C``."""

    def __class_getitem__(cls, c):
        return _Annotation(f"Set[{_named(c)}]", lambda x: _is_set_on(x, c))


class Seq(Sequence):
    """``Seq[C](x, …)``: a finite sequence over the carrier ``C``, immutable.

    Every element is checked a value of ``C`` when the sequence is built, and
    held as ``C`` makes it; a word is held as its int.  Slicing gives a
    ``Seq[C]``, ``==`` compares the values, and the sequence hashes.

    >>> s = Seq[u64](3, 4, 5)
    >>> s
    Seq[u64](3, 4, 5)
    >>> s[1:], s[0], 5 in s, s == Seq[u64](3, 4, 5)
    (Seq[u64](4, 5), 3, True, True)
    >>> Seq[u64](-1)
    Traceback (most recent call last):
    TypeError: -1 is not a u64
    """

    __slots__ = ("_items",)
    _carrier = None

    def __class_getitem__(cls, c):
        return _seq_over(c)

    def __new__(cls, *xs):
        if cls._carrier is None:
            raise TypeError("Seq[C](x, …): the carrier is stated")
        return cls._of(_held(cls._carrier, xs))

    @classmethod
    def _of(cls, items):
        s = object.__new__(cls)
        object.__setattr__(s, "_items", items)
        return s

    def __setattr__(self, name, value):
        raise AttributeError("a Seq is immutable")

    def __len__(self):
        return len(self._items)

    def __iter__(self):
        return iter(self._items)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return self._of(self._items[i])
        return self._items[i]

    def __eq__(self, other):
        if not isinstance(other, Seq):
            return NotImplemented
        return type(self) is type(other) and self._items == other._items

    def __hash__(self):
        return hash((self._carrier, tuple(self._items)))

    def __repr__(self):
        return f"{type(self).__name__}({', '.join(map(repr, self))})"


@cache
def _seq_over(c):
    """The class ``Seq[C]``, one per carrier; ``A(c)`` refuses a non-carrier."""
    A(c)
    return type(f"Seq[{_named(c)}]", (Seq,), {"__slots__": (), "_carrier": c})


def _held(c, xs):
    """The elements as ``Seq[C]`` holds them: a word's in an ``array('Q')``,
    every other carrier's in a tuple of its values."""
    universe = A(c)
    for x in xs:
        if x not in universe:
            raise TypeError(f"{x!r} is not a {_named(c)}")
    if c is u64:
        return array("Q", map(index, xs))
    return tuple(map(c, xs))


# The aliases: deliberately not in __all__ (they shadow the builtins when
# star-imported); `from pynzli.pst import any, all` is the explicit opt-in.
any = exists  # noqa: A001
all = forall  # noqa: A001

__all__ = [
    "A", "B", "K3", "U64", "GF_p", "Ternary", "u64", "Fp", "Boole", "Kleene",
    "Ø", "η", "π", "π_1", "π_2", "π_3", "Term", "Pred", "exists", "forall", "ap", "canonical", "cond", "Set", "Seq",
    "lift", "image", "preimage",
    # the readings: free functions, one spelling each (#1054)
    "bottom", "cardinality", "carrier", "codomain", "identity", "is_bounded", "is_decidable", "is_dense",
    "is_field", "is_ring", "is_truth_object", "modulus", "name", "saturates", "top", "universe",
]
