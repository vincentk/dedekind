"""Trsk: relations, the arrows between products of the Pst universes.

A relation ``A ⇸ B`` is a term over relations (C++ ``relational:rows``): its
domain and codomain are products of the universes (``U64``, ``GF_p``, ``B``,
``K3``, from ``pynzli.pst``), and its predicate a reading of the term.  A
set ``U & pred`` is a relation out of the unit; ``U * V`` multiplies.

Relations are the Kleisli arrows of the powerset monad, composed
monadically, and form a cartesian bicategory:

>>> from pynzli.pst import U64, ap, u64, π_1, π_2, canonical
>>> from pynzli.trsk import Function, arrow, classifier, converse, copy, diag, merge
>>> succ = U64 * U64 & (π_2 == π_1 + 1)
>>> two = succ >> succ  # Kleisli composition: ∃ over the middle
>>> two == (U64 * U64 & (π_2 == π_1 + 2))  # canonical eliminates the ∃
True
>>> (diag(U64) >> succ) == succ  # η, the unit of >>
True
>>> spider = copy(U64) >> (arrow(succ) * arrow(succ)) >> merge(U64)
>>> spider == (succ & succ)  # succ ∧ succ: a spider
True
>>> converse(succ)  # the dagger
(𝔸(u64) × 𝔸(u64) & (π_2 == (π_1 + 1)))°
>>> Function[u64, u64](succ)  # the graph of a function, checked
𝔸(u64) × 𝔸(u64) & (π_2 == (π_1 + 1))
>>> ap(two, 3)  # a function applied
5

A relation holds the term its author wrote; ``classifier`` reads it, and
``canonical`` is its normal form, the ∃ eliminated where an atom defines the
column:

>>> two  # as written
(𝔸(u64) × 𝔸(u64) & (π_2 == (π_1 + 1))) >> (𝔸(u64) × 𝔸(u64) & (π_2 == (π_1 + 1)))
>>> classifier(two)  # read: the ∃ over the middle
∃π_3 ∈ 𝔸(u64). (π_3 == (π_1 + 1)) & (π_2 == (π_3 + 1))
>>> succ.project(1)  # R >> π, the structural projection, as written
(𝔸(u64) × 𝔸(u64) & (π_2 == (π_1 + 1))) >> (𝔸(u64) × 𝔸(u64) × 𝔸(u64) & (π_3 == π_1))
>>> canonical(succ.project(1))  # both ∃ eliminated: the domain is entire
𝔸(u64)

``ext`` (from ``pynzli``) materialises a relation whose product is small: a
function as a read-only map from its arguments, any other relation as the
``frozenset`` of its rows, each a ``tuple``.  The author's spelling is gone,
only the extension remains; past the limit, ``ext`` refuses, naming the size:

>>> from pynzli import ext
>>> from pynzli.pst import B, K3, Ternary
>>> ext(diag(B))  # η on 𝔹, a function
mappingproxy({False: False, True: True})
>>> rows = K3 * B & (π_2 == π_1)  # a relation: U has no image in 𝔹
>>> ext(rows) == {(Ternary.FALSE, False), (Ternary.TRUE, True)}
True
>>> ext(B * K3 & (π_2 == π_1)) == {False: Ternary.FALSE, True: Ternary.TRUE}  # 𝔹 ↪ K₃
True
>>> ext(succ)
Traceback (most recent call last):
ValueError: ext: 18446744073709551616 arguments in its domain, past the limit 1000 (pass limit=)
>>> ext(U64 * U64 & (π_2 * π_2 == π_1))  # a root: no function
Traceback (most recent call last):
ValueError: ext: 340282366920938463463374607431768211456 rows in its product, past the limit 1000 (pass limit=)

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
