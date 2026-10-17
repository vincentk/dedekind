"""Trsk: relations, the arrows between products of the Pst universes.

A relation ``A ⇸ B`` is a term over relations (C++ ``relational:rows``): its
domain and codomain are products of the universes (``U64``, ``GF_p``, ``B``,
``K3``, from ``dedekind.pst``), and its predicate a reading of the term.  A
set ``U | pred`` is a relation out of the unit; ``U * V`` multiplies.

Relations are the Kleisli arrows of the powerset monad, composed
monadically, and form a cartesian bicategory::

    from dedekind.pst import U64, π_1, π_2, canonical
    from dedekind.trsk import Function, copy, diag, merge

    succ = U64 * U64 | (π_2 == π_1 + 1)
    two = succ >> succ                     # Kleisli composition: ∃ over the middle
    two == (U64 * U64 | (π_2 == π_1 + 2))  # True: canonical eliminates the ∃
    diag(U64) >> succ == succ              # True: η, the unit of >>
    copy(U64) >> (succ.arrow * succ.arrow) >> merge(U64)   # succ ∧ succ: a spider
    succ.converse                          # the dagger
    Function[u64, u64](succ)               # the graph of a function, checked

``copy`` and ``merge`` are the Frobenius spiders, ``diag`` is η, ``>>`` the
Kleisli composition (its ∃ the monad's μ), ``R.converse`` the dagger.  Where
the ∃ cannot be eliminated, membership searches within a budget, valued in Σ
(⊤ or U) for ∃ and Π (⊥ or U) for ∀.
"""

from ._pst import copy
from ._pst import diag
from ._pst import merge
from .pst import _Annotation, _attr, _is_set_on, _named


class Relation:
    """``Relation[A, B, …]``: a relation on ``A × B × …``, one column per
    factor."""

    def __class_getitem__(cls, factors):
        fs = factors if isinstance(factors, tuple) else (factors,)
        return _Annotation(f"Relation[{', '.join(map(_named, fs))}]",
                           lambda x: _attr(x, "factors") == fs)


class Function:
    """``Function[A, …, B]``: a total, single-valued arrow from ``A × …`` to
    ``B``.  A set over ``A`` valued in ``B`` is one (its χ); so is a relation
    on ``A × … × B`` certified the graph of a function of its first columns,
    applied as ``ap(R, x)``."""

    def __class_getitem__(cls, factors):
        fs = factors if isinstance(factors, tuple) else (factors,)

        def holds(x):
            if len(fs) == 2 and _is_set_on(x, fs[0]):
                return _attr(x, "codomain") == fs[1]
            return _attr(x, "factors") == fs and _attr(x, "is_function") is True

        return _Annotation(f"Function[{', '.join(map(_named, fs))}]", holds)


__all__ = ["Relation", "Function", "copy", "diag", "merge"]
