"""Trsk: relations, the arrows between products of the Pst universes.

A relation ``A ⇸ B`` is a term over relations (C++ ``relational:rows``): its
domain and codomain are products of the universes (``U64``, ``GF_p``, ``B``,
``K3``, from ``pynzli.pst``), and its predicate a reading of the term.  A
set ``U & pred`` is a relation out of the unit; ``U * V`` multiplies.

Relations are the Kleisli arrows of the powerset monad, composed
monadically, and form a cartesian bicategory::

    from pynzli.pst import U64, π_1, π_2, canonical
    from pynzli.trsk import Function, arrow, converse, copy, diag, merge

    succ = U64 * U64 & (π_2 == π_1 + 1)
    two = succ >> succ                     # Kleisli composition: ∃ over the middle
    two == (U64 * U64 & (π_2 == π_1 + 2))  # True: canonical eliminates the ∃
    diag(U64) >> succ == succ              # True: η, the unit of >>
    copy(U64) >> (arrow(succ) * arrow(succ)) >> merge(U64) # succ ∧ succ: a spider
    converse(succ)                         # the dagger
    Function[u64, u64](succ)               # the graph of a function, checked

``copy`` and ``merge`` are the Frobenius spiders, ``diag`` is η, ``>>`` the
Kleisli composition (its ∃ the monad's μ), ``converse(R)`` the dagger.  Where
the ∃ cannot be eliminated, membership searches within a budget, valued in Σ
(⊤ or U) for ∃ and Π (⊥ or U) for ∀.

A relation is its term, inspected by free functions, not methods (#1054):
``dom(R)``, ``cod(R)``, ``factors(R)``, ``classifier(R)``, ``arrow(R)``,
``converse(R)``; ``kind(R)`` names the root node (universe, comprehension, meet, join, not,
converse, tensor, compose) and ``operands(R)`` its children; a domain or
codomain is an ``Object``, a product of carriers (``kind`` unit, leaf or
product, ``operands`` the binary reading ``(first, rest)``).  ``repr(R)`` is
the Trsk set expression, ``repr(canonical(R))`` its normal form; ``==`` and
the inclusion ``<=`` / ``>=`` are decided by normal forms (``TypeError`` where
they differ, which decides nothing).

Across carriers, ``|`` is the sum (#1039): ``U64 | B`` is a set over the
object ``(𝔸(u64) + 𝔹)``, its points written explicitly ``inl(x)`` / ``inr(y)``
(never inferred: ``bool`` is an ``int``); ``&`` across carriers is the empty
set of the sum; a summand with no member leaves the other, over its own
carrier.
"""

from ._pst import copy
from ._pst import diag
from ._pst import merge
from ._pst import Object
from ._pst import inl
from ._pst import inr
from ._pst import kind
from ._pst import operands
from ._pst import arrow
from ._pst import classifier
from ._pst import cod
from ._pst import converse
from ._pst import dom
from ._pst import factors
from ._pst import total
from ._pst import univalent
from ._pst import injective
from ._pst import surjective
from .pst import _Annotation, _attr, _is_set_on, _named

#: The functions: the relations that are total and univalent, read as an
#: arrow (a table of n columns as the arrow of its first n − 1).  A set of
#: relations like the four it meets: ``R in function``, ``ap(function, R)``;
#: closed under ``>>`` as both of them are.
function = total & univalent


class Relation:
    """``Relation[A, B, …]``: a relation on ``A × B × …``, one column per
    factor."""

    def __class_getitem__(cls, factors):
        fs = factors if isinstance(factors, tuple) else (factors,)
        return _Annotation(f"Relation[{', '.join(map(_named, fs))}]",
                           lambda x: _attr(x, "_factors") == fs)


class Function:
    """``Function[A, …, B]``: a total, single-valued arrow from ``A × …`` to
    ``B``.  A set over ``A`` valued in ``B`` is one (its χ); so is a relation
    on ``A × … × B`` certified the graph of a function of its first columns,
    applied as ``ap(R, x)``."""

    def __class_getitem__(cls, factors):
        fs = factors if isinstance(factors, tuple) else (factors,)

        def holds(x):
            if len(fs) == 2 and _is_set_on(x, fs[0]):
                return _attr(x, "_codomain") == fs[1]
            # The certificate is the arrow's own split; the annotation promises
            # one output, the last factor, so the split must be that one.
            return (_attr(x, "_factors") == fs and x in function
                    and _attr(_attr(x, "_arrow"), "_cod") == fs[-1:])

        return _Annotation(f"Function[{', '.join(map(_named, fs))}]", holds)


__all__ = ["Function", "Object", "Relation", "arrow", "classifier", "cod", "converse", "copy",
           "diag", "dom", "factors", "function", "injective", "inl", "inr", "kind", "merge",
           "operands", "surjective", "total", "univalent"]
