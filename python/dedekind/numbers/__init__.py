"""Python mirror of the C++ ``dedekind.numbers`` module.

External-API surface for the canonical species spine.  The currently
mirrored child module is ``boolean``; it exports the carrier symbol
so ``var(carrier)`` and predicate-style set construction read the
same way they read in C++.

Per #400 (𝔹 carrier migration), the currently mirrored species symbol
names the carrier type itself (``𝔹 = bool``).  This package is the
external-test contract: any change to the C++ side that breaks the
"plain math" reading of the symbol should also break a Python test
here.  Future child modules (``natural``, ``integer``, ``rational``,
``real``, ``complex``, ``dual``) are tracked under #401–#405.
"""

from . import boolean  # noqa: F401

__all__ = ["boolean"]
