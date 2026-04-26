"""Python mirror of ``dedekind::algebra::RigPolynomial<R>``.

Mirrors the C++ template at
``src/main/modules/dedekind/algebra/polynomial.cppm``.  Coefficients
are represented as a list (least-significant first) with operations
defined by the ring ``R``:

  * ``ring`` defaults to ``int`` for ℕ-flavoured polynomials.
  * For F₂[x] (the natural sibling to the ``F2 = bool`` carrier from
    ``dedekind.numbers.boolean``), pass ``ring=bool``: addition
    becomes ``^`` (XOR) and multiplication becomes ``&`` (AND), which
    is exactly the ``static_assert(IsField<𝔹, std::bit_xor<𝔹>,
    std::bit_and<𝔹>>)`` C++ witness made executable.

Operations exposed (the minimum-viable surface):

  * ``+``, ``*`` — ring operations on the coefficient lists.
  * ``__eq__``, ``__repr__``.
  * ``degree`` — the index of the highest non-zero coefficient.

Out of scope for this initial mirror (defer to a follow-up issue):

  * Euclidean GCD / division-with-remainder.
  * Irreducibility checks over arbitrary rings.
  * Factoring / GF(p^n) construction via irreducible quotients.
"""

from __future__ import annotations

from typing import Iterable


def _ring_zero(ring) -> object:
    """The additive identity in the ring (``ring()`` for the default)."""
    if ring is bool:
        return False
    return ring(0) if ring is int else ring()


def _ring_one(ring) -> object:
    """The multiplicative identity in the ring."""
    if ring is bool:
        return True
    return ring(1) if ring is int else ring(1)


def _ring_add(ring, a, b):
    """Ring addition.  XOR for F₂ (``ring=bool``); ``+`` otherwise."""
    return (a ^ b) if ring is bool else (a + b)


def _ring_mul(ring, a, b):
    """Ring multiplication.  AND for F₂ (``ring=bool``); ``*`` otherwise."""
    return (a & b) if ring is bool else (a * b)


def _trim(coeffs: list, ring) -> list:
    """Drop trailing zero coefficients to keep ``degree`` honest."""
    zero = _ring_zero(ring)
    out = list(coeffs)
    while len(out) > 0 and out[-1] == zero:
        out.pop()
    return out


class RigPolynomial:
    """Polynomial over a rig (semiring without additive inverse) ``R``.

    Coefficients are stored least-significant first: ``coeffs[i]`` is
    the coefficient of ``x^i``.  Mirrors the C++
    ``RigPolynomial<R>`` template's coefficient layout.
    """

    __slots__ = ("ring", "_coeffs")

    def __init__(self, coeffs: Iterable, ring=int):
        self.ring = ring
        self._coeffs = _trim(list(coeffs), ring)

    @property
    def coeffs(self) -> list:
        return list(self._coeffs)

    @property
    def degree(self) -> int:
        """Degree of the polynomial; ``-1`` for the zero polynomial."""
        return len(self._coeffs) - 1

    def __eq__(self, other) -> bool:
        if not isinstance(other, RigPolynomial):
            return NotImplemented
        return self.ring is other.ring and self._coeffs == other._coeffs

    def __add__(self, other: "RigPolynomial") -> "RigPolynomial":
        if not isinstance(other, RigPolynomial):
            return NotImplemented
        if self.ring is not other.ring:
            raise TypeError(
                f"Cannot add polynomials over different rings: "
                f"{self.ring.__name__} vs {other.ring.__name__}"
            )
        n = max(len(self._coeffs), len(other._coeffs))
        zero = _ring_zero(self.ring)
        out = []
        for i in range(n):
            a = self._coeffs[i] if i < len(self._coeffs) else zero
            b = other._coeffs[i] if i < len(other._coeffs) else zero
            out.append(_ring_add(self.ring, a, b))
        return RigPolynomial(out, ring=self.ring)

    def __mul__(self, other: "RigPolynomial") -> "RigPolynomial":
        if not isinstance(other, RigPolynomial):
            return NotImplemented
        if self.ring is not other.ring:
            raise TypeError(
                f"Cannot multiply polynomials over different rings: "
                f"{self.ring.__name__} vs {other.ring.__name__}"
            )
        if not self._coeffs or not other._coeffs:
            return RigPolynomial([], ring=self.ring)
        zero = _ring_zero(self.ring)
        out = [zero] * (len(self._coeffs) + len(other._coeffs) - 1)
        for i, a in enumerate(self._coeffs):
            for j, b in enumerate(other._coeffs):
                out[i + j] = _ring_add(self.ring, out[i + j],
                                       _ring_mul(self.ring, a, b))
        return RigPolynomial(out, ring=self.ring)

    def __repr__(self) -> str:
        return f"RigPolynomial({self._coeffs!r}, ring={self.ring.__name__})"

    @staticmethod
    def zero(ring=int) -> "RigPolynomial":
        return RigPolynomial([], ring=ring)

    @staticmethod
    def one(ring=int) -> "RigPolynomial":
        return RigPolynomial([_ring_one(ring)], ring=ring)

    @staticmethod
    def x(ring=int) -> "RigPolynomial":
        """The indeterminate ``x`` as a polynomial: ``[0, 1]``."""
        return RigPolynomial([_ring_zero(ring), _ring_one(ring)], ring=ring)


__all__ = ["RigPolynomial"]
