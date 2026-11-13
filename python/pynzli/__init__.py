"""pynzli: Python handles over the dedekind library's C++ core.

The mathematics lives in C++; Python holds handles to it.  Four modules:

``pynzli.jlt``
    The arrows: ``id``, ``refl``, ``succ`` and ``pred`` over ``bool`` and
    ``int`` (the word u64), composed with ``>>``.
``pynzli.pst``
    The finite carriers and the sets over them: the truth chains ``B`` and
    ``K3``, the canonical ring ``u64`` and field ``𝔽ₚ`` (universes ``U64``, ``GF_p``), and
    the term surface ``A(carrier) & term``, checked when it is built.
``pynzli.q``
    ``Q``, the rationals as terms of ℚ, exact: from an ``int``, a
    ``Fraction`` or a finite ``float``; ``==`` and the order by canonical form.
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

from itertools import islice
from math import prod
from types import MappingProxyType

from ._jlt import MatchQuery, RandomText, WindowQuery, collect, count
from ._pst import UniverseFp, UniverseU64
from ._version import __version__
from .pst import A, K3, Fp, Seq, ap, cardinality, carrier, codomain, u64
from .pst import _attr
from .q import Q
from .trsk import arrow, dom, factors, function, kind, operands


def ext(S, *, limit=1000):
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

    A source of more than ``limit`` points raises ``ValueError``, the count named: never truncated; ``take`` previews
    it.  Where the library does not count a set, its universe's count bounds it.

    >>> from pynzli.jlt import abc, fixed_by, len as length, random_text, rev, words
    >>> pali6 = words(abc("ACGT")) & length(6) & fixed_by(~rev)
    >>> ext(b"GAATTCGAGCTC" >> pali6)  # where each site begins
    Seq[u64](0, 6)
    >>> text = ext(random_text(abc("ACGT"), 8))
    >>> type(text).__name__, len(text), set(text) <= set(b"ACGT")
    ('bytes', 8, True)
    >>> ext(random_text(abc("ACGT"), 10**9))
    Traceback (most recent call last):
    ValueError: ext: 1000000000 letters, past the limit 1000 (pass limit=)
    """
    if isinstance(S, RandomText):
        _within(S.length, "letters", limit)
        return bytes(S)
    if isinstance(S, (MatchQuery, WindowQuery)):
        _within(count(S), "positions", limit)
        return Seq[u64](*collect(S))
    points, keep, shape, size, what = _points(S, "ext")
    _within(size, what, limit)
    return shape(filter(keep, points) if keep else points)


def take(S, n=5, *, limit=1000):
    """``take(S, n)``: a preview, the first ``n`` points of a set or relation
    in its universe's order, shaped as ``ext`` shapes the whole, as a data
    frame's ``head`` is.  It needs no count, so it reaches what ``ext``
    refuses; it scans at most ``limit`` points and returns what it found
    there.

    >>> from pynzli.pst import A, u64, π
    >>> U = A(u64)
    >>> take(U & (π % 2 == 0), 3)
    frozenset({0, 2, 4})
    >>> take(U & (π % 1000 == 999))  # within the first 1000: to see more, raise the limit
    frozenset({999})
    >>> from pynzli.pst import B, U64
    >>> take(U64 | B)
    Traceback (most recent call last):
    TypeError: take(𝔸(u64) | 𝔹): a sum's points are tagged, inl / inr; not enumerated yet
    >>> from pynzli.pst import GF_p
    >>> len(take(GF_p, 3))  # the field's first residues, its universe never read whole
    3
    """
    points, keep, shape, _, _ = _points(S, "take")
    scanned = islice(points, limit)
    return shape(islice(filter(keep, scanned) if keep else scanned, n))


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
        raise ValueError(f"ext: {n} {what}, past the limit {limit} (pass limit=)")


def _universe(c):
    """The universe over a column's carrier: ``A(c)`` of a carrier type, a
    truth chain its own universe."""
    return A(c) if isinstance(c, type) else c


def _count(columns):
    return prod(cardinality(_universe(c)) for c in columns)


def _elements(U):
    """A universe's elements in its order, lazily, read only when asked for:
    the word's are 0 to 2⁶⁴ − 1, the field's their residues, a chain's its own."""
    if isinstance(U, UniverseU64):
        yield from range(cardinality(U))
    elif isinstance(U, UniverseFp):
        yield from map(Fp, range(cardinality(U)))
    else:
        yield from U


def _rows(columns):
    """A product's rows in order, lazily: the first column slowest."""
    if not columns:
        yield ()
        return
    for x in _elements(_universe(columns[0])):
        for rest in _rows(columns[1:]):
            yield (x, *rest)


def _arguments(columns):
    """The arguments of a function, as ``ap`` takes them: a value for one
    column, a tuple for several."""
    if len(columns) == 1:
        return _elements(_universe(columns[0]))
    return _rows(columns)


def _read_only(pairs):
    """A map from pairs, read-only."""
    return MappingProxyType(dict(pairs))


def _kind(x):
    """``kind(x)`` where x is a set, relation or object of pynzli; else None."""
    try:
        return kind(x)
    except TypeError:
        return None


def _has_sum(x):
    """Whether a sum occurs in ``x``'s object, at the top or nested in a
    product: its points are tagged, ``inl(x)`` / ``inr(y)``."""
    k = _kind(x)
    if k == "sum":
        return True
    if k in (None, "leaf", "unit"):
        return False
    return any(map(_has_sum, operands(x)))


def _points(S, caller):
    """How a set or relation is read: its candidate points in the universe's
    order, which of them are in its extension (``None``: all), the shape of
    the result, how many candidates, and what they are called.

    A function is its graph, a map from each argument; a set or relation
    valued in K₃ is its χ, a map from each point; else the points it holds.
    """
    if _has_sum(S):
        raise TypeError(f"{caller}({S!r}): a sum's points are tagged, inl / inr; not enumerated yet")
    if _attr(S, "_factors") is not None:
        if S in function:
            columns = dom(arrow(S))
            pairs = ((x, ap(S, x)) for x in _arguments(columns))
            return pairs, None, _read_only, _count(columns), "arguments in its domain"
        columns = factors(S)
        rows = _rows(columns)
        if codomain(S) is K3:
            pairs = ((r, ap(S, r)) for r in rows)
            return pairs, None, _read_only, _count(columns), "rows in its product"
        return rows, S.__contains__, frozenset, _count(columns), "rows in its product"
    if _attr(S, "_carrier") is not None:
        U = _universe(carrier(S))
        if codomain(S) is K3:
            pairs = ((x, ap(S, x)) for x in _elements(U))
            return pairs, None, _read_only, cardinality(U), "elements in its universe"
        n = _attr(S, "_cardinality")
        if n is not None:
            return _elements(S), None, frozenset, n, "elements"
        return _elements(U), S.__contains__, frozenset, cardinality(U), "elements in its universe"
    raise TypeError(f"{caller}({S!r}): no set, relation, function or sequence of pynzli")


__all__ = ["Q", "__version__", "ext", "jlt", "pst", "q", "take", "to_string", "trsk"]
