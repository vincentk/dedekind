"""pynzli: Python handles over the dedekind library's C++ core.

The mathematics lives in C++; Python holds handles to it.  Three modules:

``pynzli.jlt``
    The arrows: ``id``, ``refl``, ``succ`` and ``pred`` over ``bool`` and
    ``int`` (the word u64), composed with ``>>``.
``pynzli.pst``
    The finite carriers and the sets over them: the truth chains ``B`` and
    ``K3``, the canonical ring ``u64`` and field ``𝔽ₚ`` (universes ``U64``, ``GF_p``), and
    the term surface ``A(carrier) & term``, checked when it is built.
``pynzli.trsk``
    The relations between products of those universes: Kleisli composition
    ``>>``, ``diag`` (η), the Frobenius spiders ``copy`` / ``merge``, the
    converse, and the annotations ``Relation[…]``, ``Function[…]``.

A term may use the operators the carrier's certified algebras have (the laws
decide the syntax), and equality of terms is equality in the free algebra
they generate where it has a normal form (a chain's run lists); see the
``sets:terms`` header in the C++ sources.

The project: https://github.com/vincentk/dedekind#readme

The C++ API reference: https://vincentk.github.io/dedekind/

The Jlt paper: https://vincentk.github.io/dedekind/jlt.pdf
"""

from itertools import product
from math import prod
from types import MappingProxyType

from ._jlt import MatchQuery, RandomText, WindowQuery, collect, count
from ._version import __version__
from .pst import A, K3, Seq, ap, cardinality, carrier, codomain, u64
from .pst import _attr
from .trsk import arrow, dom, factors, function


def ext(S, *, limit=1 << 20):
    """``ext(S)``: the extension of an intensional value, immutable, its shape
    the source's type's:

    ================================  =====================================
    the source                        its extension
    ================================  =====================================
    a set                             ``frozenset`` of its elements
    a set valued in K₃                its χ, a read-only map to K₃
    a relation                        ``frozenset`` of its rows, ``tuple``
    a relation valued in K₃           its χ, a read-only map from rows to K₃
    a relation certified a function   a read-only map, argument to value
    a text of letters                 ``bytes`` (``to_string``: a ``str``)
    the positions of a query          ``Seq[u64]``
    ================================  =====================================

    A source of more than ``limit`` points raises ``ValueError``, the count
    named: never truncated.  Where the library does not count a set, its
    universe's count bounds it.

    >>> from pynzli.jlt import abc, fixed_by, len as length, random_text, rev, words
    >>> pali6 = words(abc("ACGT")) & length(6) & fixed_by(~rev)
    >>> ext(b"GAATTCGAGCTC" >> pali6)  # where each site begins
    Seq[u64](0, 6)
    >>> text = ext(random_text(abc("ACGT"), 8))
    >>> type(text).__name__, len(text), set(text) <= set(b"ACGT")
    ('bytes', 8, True)
    >>> ext(random_text(abc("ACGT"), 10**9))
    Traceback (most recent call last):
    ValueError: ext: 1000000000 letters, past the limit 1048576
    """
    if isinstance(S, RandomText):
        _within(len(S), "letters", limit)
        return bytes(S)
    if isinstance(S, (MatchQuery, WindowQuery)):
        _within(count(S), "positions", limit)
        return Seq[u64](*collect(S))
    if _attr(S, "_factors") is not None:
        return _relation(S, limit)
    if _attr(S, "_carrier") is not None:
        return _set(S, limit)
    raise TypeError(f"ext({S!r}): no set, relation, function or sequence of pynzli")


def to_string(letters):
    """``to_string(letters)``: a text of letters as a ``str``, each letter its
    ASCII character.  ``ext`` gives letters as ``bytes``; a ``str`` is asked
    for, never guessed.  A byte past ASCII raises ``ValueError``, named.

    >>> to_string(b"GAATTC")
    'GAATTC'
    >>> to_string(bytes([71, 65, 255, 67]))
    Traceback (most recent call last):
    ValueError: to_string: byte 255 at 2 is no ASCII letter
    """
    if not isinstance(letters, bytes):
        raise TypeError(f"to_string({letters!r}): letters are bytes, from ext")
    try:
        return letters.decode("ascii")
    except UnicodeDecodeError as e:
        raise ValueError(
            f"to_string: byte {letters[e.start]} at {e.start} is no ASCII letter"
        ) from None


def _within(n, what, limit):
    """Refuse ``n`` points past ``limit``, naming ``n``."""
    if n > limit:
        raise ValueError(f"ext: {n} {what}, past the limit {limit}")


def _universe(c):
    """The universe over a column's carrier: ``A(c)`` of a carrier type, a
    truth chain its own universe."""
    return A(c) if isinstance(c, type) else c


def _count(columns):
    return prod(cardinality(_universe(c)) for c in columns)


def _arguments(columns):
    """The arguments of a function, as ``ap`` takes them: a value for one
    column, a tuple for several."""
    if len(columns) == 1:
        return iter(_universe(columns[0]))
    return product(*map(_universe, columns))


def _set(S, limit):
    """A set over one carrier: its elements, or its χ where valued in K₃."""
    U = _universe(carrier(S))
    if codomain(S) is K3:
        _within(cardinality(U), "elements in its universe", limit)
        return MappingProxyType({x: ap(S, x) for x in U})
    n = _attr(S, "_cardinality")
    if n is not None:
        _within(n, "elements", limit)
        return frozenset(S)
    _within(cardinality(U), "elements in its universe", limit)
    return frozenset(x for x in U if x in S)


def _relation(R, limit):
    """A relation: a function's graph as a map, else its rows, or its χ
    where valued in K₃."""
    if R in function:
        columns = dom(arrow(R))
        _within(_count(columns), "arguments in its domain", limit)
        return MappingProxyType({x: ap(R, x) for x in _arguments(columns)})
    columns = factors(R)
    _within(_count(columns), "rows in its product", limit)
    rows = product(*map(_universe, columns))
    if codomain(R) is K3:
        return MappingProxyType({r: ap(R, r) for r in rows})
    return frozenset(r for r in rows if r in R)


__all__ = ["__version__", "ext", "jlt", "pst", "to_string", "trsk"]
