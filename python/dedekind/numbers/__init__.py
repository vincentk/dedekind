"""Python mirror of the C++ ``dedekind.numbers`` module.

External-API surface for the canonical species spine.  Currently
mirrored child modules:

  * ``boolean`` — ``𝔹 = bool``, plus the Galois-field-of-order-2 reading
    via ``F2`` (#400, PR #407).
  * ``natural`` — ``ℕ = int`` (Python int is unbounded, closer to the
    conceptual ℕ than C++ unsigned int's modular ring) (#401).

Per #399 / #400 / #401, each species symbol names the carrier type
itself.  This package is the external-test contract: any change to
the C++ side that breaks the "plain math" reading of a symbol should
also break a Python test here.  Future child modules (``integer``,
``rational``, ``real``, ``complex``, ``dual``) are tracked under
#402–#405.
"""

from . import boolean  # noqa: F401
from . import natural  # noqa: F401

__all__ = ["boolean", "natural"]
