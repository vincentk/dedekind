"""Python mirror of the C++ ``dedekind.numbers`` module.

External-API surface for the canonical species spine: each child module
(``boolean``, ...) exports the carrier symbol so ``var(carrier)`` and
predicate-style set construction read the same way they read in C++.

Per #400 (𝔹 carrier migration), the species symbols name the carrier
type itself (``𝔹 = bool``, ``ℕ = int``, ...).  This package is the
external-test contract: any change to the C++ side that breaks the
"plain math" reading of the symbols should also break a Python test
here.
"""

from . import boolean  # noqa: F401

__all__ = ["boolean"]
