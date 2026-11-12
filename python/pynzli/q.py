"""ℚ: the rationals as terms, exact (#1035).

A ``Q`` holds a term of ℚ, the library's ``RatExpr<>``: ``+ − ·`` and ``/``
build terms, and ``==`` and the order are those of the canonical form, the
reduced fraction, decided in C++.  An ``int`` and a ``Fraction`` enter
exactly; a ``float`` enters as the dyadic rational its bits spell, so
``Q(0.1)`` is not one tenth.  ``Fraction(q)`` is exact, and ``repr`` shows the
canonical form.

>>> from fractions import Fraction
>>> from pynzli import Q
>>> Q(1) / 3 + Q(1) / 6
Q(1/2)
>>> Q(0.1) == Fraction(1, 10), Q(0.1) == Fraction(0.1)
(False, True)
>>> Q(2**64) * Q(2**64) == 2**128
True
>>> Fraction(Q(-6) / 4), Q(1) < Fraction(3, 2) <= Q(3) / 2
(Fraction(-3, 2), True)

What is not a rational is refused: a division by zero, NaN and ±∞.  A term
has a declared bound, 64 nodes: past it, ``OverflowError``, and
``canonical()`` brings a term back to its normal form.

>>> Q(1) / 0
Traceback (most recent call last):
ZeroDivisionError: Q: division by zero
>>> Q(float("nan"))
Traceback (most recent call last):
ValueError: Q: nan is not a rational
>>> x = Q(1)
>>> for _ in range(10):
...     x = x + x
Traceback (most recent call last):
OverflowError: Q: the term passes its bound of 64 nodes; canonical() normalises it
>>> y = Q(1)
>>> for _ in range(10):
...     y = (y + y).canonical()
>>> y
Q(1024)
"""

import math
from fractions import Fraction
from numbers import Rational

from ._pst import RatTerm

_WORD = 1 << 64


def _natural(n):
    """The term of a natural: one atom where it fits a word, else its limbs
    in Horner form over 2⁶⁴."""
    if n < _WORD:
        return RatTerm.atom(n)
    limbs = []
    while n:
        limbs.append(n & (_WORD - 1))
        n >>= 64
    base = RatTerm.atom(1 << 32).mul(RatTerm.atom(1 << 32))
    acc = RatTerm.atom(limbs[-1])
    for limb in reversed(limbs[:-1]):
        acc = acc.mul(base).add(RatTerm.atom(limb))
    return acc


def _integer(n):
    t = _natural(abs(n))
    return t.neg() if n < 0 else t


def _exact(x):
    """The term of an exact operand, a ``Q``, an ``int`` or a rational; else
    ``None``."""
    if isinstance(x, Q):
        return x._t
    if isinstance(x, int):
        return _integer(x)
    if isinstance(x, Rational):
        return _integer(x.numerator).div(_integer(x.denominator))
    return None


def _value(t):
    """The value of a term, read off its nodes."""
    values = []
    for node in t.nodes():
        kind, *args = node
        if kind == "atom":
            values.append(Fraction(args[0]))
        elif kind == "add":
            values.append(values[args[0]] + values[args[1]])
        elif kind == "mul":
            values.append(values[args[0]] * values[args[1]])
        elif kind == "neg":
            values.append(-values[args[0]])
        else:
            values.append(values[args[0]] / values[args[1]])
    return values[-1]


class Q:
    """A rational, held as a term of ℚ.  ``Q(x)`` for an ``int``, a
    ``Fraction`` or a ``Q`` (exact), or a finite ``float`` (its dyadic
    value)."""

    __slots__ = ("_t",)

    def __init__(self, x=0):
        if isinstance(x, float):
            if not math.isfinite(x):
                raise ValueError(f"Q: {x} is not a rational")
            self._t = RatTerm.from_float(x)
            return
        t = _exact(x)
        if t is None:
            raise TypeError(f"Q({x!r}): an int, a Fraction, a Q or a float")
        self._t = t

    @classmethod
    def _of(cls, t):
        q = cls.__new__(cls)
        q._t = t
        return q

    def canonical(self):
        """The same rational, its term in canonical form."""
        return Q._of(self._t.canonical())

    @property
    def numerator(self):
        return _value(self._t.canonical()).numerator

    @property
    def denominator(self):
        return _value(self._t.canonical()).denominator

    def __add__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(self._t.add(t))

    def __radd__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(t.add(self._t))

    def __sub__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(self._t.add(t.neg()))

    def __rsub__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(t.add(self._t.neg()))

    def __mul__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(self._t.mul(t))

    def __rmul__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(t.mul(self._t))

    def __truediv__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(self._t.div(t))

    def __rtruediv__(self, other):
        t = _exact(other)
        return NotImplemented if t is None else Q._of(t.div(self._t))

    def __neg__(self):
        return Q._of(self._t.neg())

    def __pos__(self):
        return self

    def _compare(self, other):
        t = _exact(other)
        return None if t is None else self._t.compare(t)

    def __eq__(self, other):
        c = self._compare(other)
        return NotImplemented if c is None else c == 0

    def __lt__(self, other):
        c = self._compare(other)
        return NotImplemented if c is None else c < 0

    def __le__(self, other):
        c = self._compare(other)
        return NotImplemented if c is None else c <= 0

    def __gt__(self, other):
        c = self._compare(other)
        return NotImplemented if c is None else c > 0

    def __ge__(self, other):
        c = self._compare(other)
        return NotImplemented if c is None else c >= 0

    def __hash__(self):
        return hash(_value(self._t.canonical()))

    def __repr__(self):
        v = _value(self._t.canonical())
        return f"Q({v.numerator})" if v.denominator == 1 else f"Q({v})"


Rational.register(Q)

__all__ = ["Q"]
