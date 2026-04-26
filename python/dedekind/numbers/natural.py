"""Python mirror of ``dedekind::sets::ℕ`` and ``dedekind::numbers::naturals``.

Per #401 (carrier-type migration), the canonical Natural-numbers species
symbol ``ℕ`` names the carrier itself.  The C++ side picks
``unsigned int`` (machine-ℕ; the modular ring ℤ/2^wℤ where ``w`` is
the carrier's value-bit width — typically 32 on current targets, but
not guaranteed by the standard).  On the Python side the analog is
``int`` — Python's ``int`` is unbounded, which is a happy accident of
the language boundary: it matches the *conceptual* ℕ ("the infinite
number line") more faithfully than C++ unsigned ever can.  The exact-ℕ
sibling ``ExtensionalCardinal<>`` (saturating to ℵ₀) on the C++ side
maps onto plain ``int`` in Python by the same logic.

Operator convention (matches the C++ side and mirrors
``dedekind.numbers.boolean``):

  * ``var(ℕ)`` is a ``Variable`` over the carrier (re-exported from
    ``dedekind.sets`` since the machinery is domain-agnostic).
  * ``b == rhs`` / ``b != rhs`` / ``b < rhs`` etc. on a ``Variable`` build
    callable predicates.
  * ``successor(n) = n + 1`` is the Peano-flavoured carrier morphism;
    available as a free function for callsite ergonomics.

The module is intentionally minimal — just enough to act as the
external-API contract test for the #401 carrier migration.
"""

from ..sets import Variable, var

# Carrier symbol.  ``ℕ`` is the Unicode form; ``N_`` is an ASCII alias.
# This module exports only the carrier reading for natural numbers in
# the Python API; it does not define a separate value-level ``N``
# predicate-set classifier (the C++ side has one in
# ``dedekind::sets::N``, but the Python mirror is intentionally minimal
# — Set-builder DSL parity is out of scope for the #401 mirror and
# tracked under #408 / future per-symbol PRs).
ℕ = int
N_ = int


def successor(n: int) -> int:
    """Peano successor on ℕ: ``s(n) = n + 1``.

    Mirrors the carrier-level successor reading of ℕ as the inductively
    defined natural-number type.  Total on Python ``int`` (no overflow
    at the language level — Python int is unbounded), which is closer
    to the conceptual ℕ than the C++ machine carrier (where overflow
    wraps modulo 2^w, for ``w`` = the carrier's value-bit width).
    """
    if not isinstance(n, int) or isinstance(n, bool):
        raise TypeError(f"successor() requires an int; got {type(n).__name__!r}")
    if n < 0:
        raise ValueError(f"successor() is defined on ℕ; got n = {n} < 0")
    return n + 1


__all__ = ["ℕ", "N_", "Variable", "var", "successor"]
