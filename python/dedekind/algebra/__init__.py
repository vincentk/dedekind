"""Python mirror of the C++ ``dedekind.algebra`` module.

External-API surface for algebraic structures.  Currently mirrored
child modules:

  * ``polynomial`` — ``RigPolynomial(coeffs, ring)`` mirroring
    ``dedekind::algebra::RigPolynomial<R>``.  The F₂ specialisation
    (``RigPolynomial(coeffs, ring=bool)``) is the natural sibling to
    the ``F2 = bool`` carrier exposed in ``dedekind.numbers.boolean``;
    walks the textbook ``(x+1)² = x²+1 over F₂`` witness in tests.

Filed under the cross-issue note on #401 (PR #407 #400 follow-up): the
ℕ carrier migration carries the F₂[x] / RigPolynomial<bool> Python
mirror as its sibling exposition, since the polynomial ring story is
told once across both ℕ-coefficient and F₂-coefficient at the same
time.
"""

from . import polynomial  # noqa: F401

__all__ = ["polynomial"]
