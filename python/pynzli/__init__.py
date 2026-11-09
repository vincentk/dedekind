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

from itertools import islice, product
from warnings import warn
from math import prod
from types import MappingProxyType, SimpleNamespace

from ._jlt import MatchQuery, RandomText, WindowQuery, collect, count
from ._version import __version__
from .pst import A, K3, Seq, ap, cardinality, carrier, codomain, u64
from .pst import _attr
from .trsk import arrow, dom, factors, function


options = SimpleNamespace(limit=1000)
"""``pynzli.options``: settings read at each call.  ``options.limit``, default
1000, is how many points ``ext`` materialises and ``take`` scans before it
refuses; set it once (``pynzli.options.limit = 10**6``), or pass ``limit=`` to
one call."""


def ext(S, *, limit=None):
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

    A source of more than ``limit`` points (``options.limit`` unless given)
    raises ``ValueError``, the count named: never truncated; ``take`` previews
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
    ValueError: ext: 1000000000 letters, past the limit 1000 (pynzli.options.limit)
    """
    limit = options.limit if limit is None else limit
    if isinstance(S, RandomText):
        _within(len(S), "letters", limit)
        return bytes(S)
    if isinstance(S, (MatchQuery, WindowQuery)):
        _within(count(S), "positions", limit)
        return Seq[u64](*collect(S))
    points, keep, shape, size, what = _points(S, "ext")
    _within(size, what, limit)
    return shape(filter(keep, points) if keep else points)


def take(S, n=5, *, limit=None):
    """``take(S, n)``: a preview, the first ``n`` points of a set or relation
    in its universe's order, shaped as ``ext`` shapes the whole, as a data
    frame's ``head`` is.  It needs no count, so it reaches what ``ext``
    refuses; it scans at most ``limit`` points (``options.limit`` unless
    given); where that leaves it short it returns what it found, with a
    warning saying how to see more.

    >>> from pynzli.pst import A, u64, π
    >>> U = A(u64)
    >>> take(U & (π % 2 == 0), 3)
    frozenset({0, 2, 4})
    >>> take(U & (π % 1000 == 999))  # warns: 1 of 5, raise pynzli.options.limit
    frozenset({999})
    """
    limit = options.limit if limit is None else limit
    points, keep, shape, size, what = _points(S, "take")
    scanned = islice(points, limit)
    found = list(islice(filter(keep, scanned) if keep else scanned, n))
    if len(found) < n and size > limit:
        warn(
            f"take: {len(found)} of {n} among the first {limit} {what}; "
            "to see more, raise pynzli.options.limit",
            stacklevel=2,
        )
    return shape(found)


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
        raise ValueError(f"ext: {n} {what}, past the limit {limit} (pynzli.options.limit)")


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


def _read_only(pairs):
    """A map from pairs, read-only."""
    return MappingProxyType(dict(pairs))


def _points(S, caller):
    """How a set or relation is read: its candidate points in the universe's
    order, which of them are in its extension (``None``: all), the shape of
    the result, how many candidates, and what they are called.

    A function is its graph, a map from each argument; a set or relation
    valued in K₃ is its χ, a map from each point; else the points it holds.
    """
    if _attr(S, "_factors") is not None:
        if S in function:
            columns = dom(arrow(S))
            pairs = ((x, ap(S, x)) for x in _arguments(columns))
            return pairs, None, _read_only, _count(columns), "arguments in its domain"
        columns = factors(S)
        rows = product(*map(_universe, columns))
        if codomain(S) is K3:
            pairs = ((r, ap(S, r)) for r in rows)
            return pairs, None, _read_only, _count(columns), "rows in its product"
        return rows, S.__contains__, frozenset, _count(columns), "rows in its product"
    if _attr(S, "_carrier") is not None:
        U = _universe(carrier(S))
        if codomain(S) is K3:
            pairs = ((x, ap(S, x)) for x in U)
            return pairs, None, _read_only, cardinality(U), "elements in its universe"
        n = _attr(S, "_cardinality")
        if n is not None:
            return iter(S), None, frozenset, n, "elements"
        return iter(U), S.__contains__, frozenset, cardinality(U), "elements in its universe"
    raise TypeError(f"{caller}({S!r}): no set, relation, function or sequence of pynzli")


__all__ = ["__version__", "ext", "jlt", "options", "pst", "take", "to_string", "trsk"]
