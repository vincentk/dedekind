"""Python mirror of ``dedekind::numbers::booleans`` (and ``dedekind::algebra::boolean``).

Per #400 (carrier-type migration), the canonical Boolean species symbol
``𝔹`` names the carrier itself.  In Python the carrier is ``bool``;
this module exposes the symbol plus the symbolic-scout factory ``var``
(re-exported from ``dedekind.sets``) so set-builder DSL expressions
read the way they read in C++::

    from dedekind.numbers.boolean import 𝔹, B, var

    b = var(𝔹)
    truthy = (b == True)               # callable predicate: lambda v: v == True
    falsy  = b.not_()                  # bool-domain shorthand for "b is False"

Operator convention (matches the C++ side):

  * ``b == rhs`` / ``b != rhs`` / ``b <`` ... build callable predicates.
  * ``b.not_()`` is the bool-domain identity-negation shorthand;
    ``~`` is reserved for set complement and is intentionally not
    overloaded on ``Variable``.

The Python surface is intentionally minimal — just enough for an
external-API test that the C++ migration's "plain math" reading
survives across the language boundary.  The generic ``Variable`` /
``var`` machinery lives in ``dedekind.sets`` (it is not boolean-
specific); this module re-exports it for callsite ergonomics.
"""

from ..sets import Variable, var

# Carrier symbols.  ``𝔹`` is the canonical Unicode form; ``B`` is the
# ASCII alias for environments where Unicode input is inconvenient.
𝔹 = bool
B = bool


__all__ = ["𝔹", "B", "Variable", "var"]
