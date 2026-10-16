"""dedekind: Python handles over the C++ core.

The mathematics lives in C++; Python holds handles to it.  Two modules:

``dedekind.jlt``
    The arrows: ``id``, ``refl``, ``succ`` and ``pred`` over ``bool`` and
    ``int`` (the word u64), composed with ``>>``.
``dedekind.pst``
    The finite carriers and the sets over them: the truth chains ``B`` and
    ``K3``, the canonical ring ``u64`` and field ``𝔽ₚ`` (universes ``U64``, ``GF_p``), and
    the term surface ``A(carrier) | term``, checked when it is built.

A term may use the operators the carrier's certified algebras have (the laws
decide the syntax), and equality of terms is equality in the free algebra
they generate where it has a normal form (a chain's run lists); see the
``sets:terms`` header in the C++ sources.
"""

__all__ = ["jlt", "pst"]
