"""Python ↔ C++ extensional sets mapping.

This module re-exports the C++ ``dedekind.sets`` bindings for set algebra over
finite extensional sets, and adds pure-Python helpers that mirror the C++
DSL machinery in ``dedekind.sets:expressions``:

  * ``Variable`` / ``var(domain)`` — symbolic-scout factory (mirrors
    ``dedekind::sets::Variable<S>`` / ``var<S>`` in ``expressions.cppm``).
    Lives here, not in ``dedekind.numbers.boolean``, because the
    machinery is domain-agnostic — anything with a sensible carrier
    type can be the symbolic-scout's domain.
  * ``power_set`` / ``𝔓`` — power-set generator over a finite carrier.
    Mirrors ``dedekind::sets::power_set<T, L, P>`` (and the C++
    fraktur-P alias ``𝔓``) from ``expressions.cppm``.

Operator convention (matching the C++ side):

  * ``b == rhs`` / ``b != rhs`` etc. on a ``Variable`` build callable
    predicates.
  * ``b.not_()`` is the bool-domain identity-negation shorthand
    (mirrors C++ ``operator!`` on ``Variable<bool>``).
  * ``~`` is reserved for set complement; do not overload it on
    ``Variable``.
  * ``|`` and ``&`` are the bitwise-lattice operators, used for
    set union and intersection (the C++ side already does this on
    ``Set``; the Python side will follow once a ``Set`` class lands).
"""

from itertools import combinations

from ._dedekind import set_cardinality, set_difference, set_intersection, set_union


class Variable:
    """The symbolic scout (``id_S``) of a carrier type.

    Mirrors ``dedekind::sets::Variable<S>`` from
    ``src/main/modules/dedekind/sets/expressions.cppm``.  Holds the
    carrier domain and supports basic comparison / logical operators
    that build callable predicates.
    """

    __slots__ = ("domain",)

    def __init__(self, domain):
        self.domain = domain

    def __eq__(self, rhs):
        return lambda v: v == rhs

    def __ne__(self, rhs):
        return lambda v: v != rhs

    def __lt__(self, rhs):
        return lambda v: v < rhs

    def __le__(self, rhs):
        return lambda v: v <= rhs

    def __gt__(self, rhs):
        return lambda v: v > rhs

    def __ge__(self, rhs):
        return lambda v: v >= rhs

    def not_(self):
        """``b.not_()`` ≡ "b is False" predicate (bool-domain only).

        Mirrors C++ ``operator!`` on ``Variable<S>`` constrained to
        ``element_of_t<S> == bool``.  ``~`` is intentionally NOT
        overloaded here — the project reserves ``~`` for set
        complement.
        """
        if self.domain is not bool:
            raise TypeError(
                f"Variable.not_() requires a bool domain; got {self.domain!r}"
            )
        return self == False  # noqa: E712 — predicate-builder, not a comparison

    def __repr__(self):
        return f"var({self.domain.__name__})"


def var(domain):
    """Symbolic-scout factory: ``var(𝔹)`` ranges over ``bool``.

    Mirrors ``dedekind::sets::var<S>`` (the ``inline constexpr
    Variable<S> var{}`` template variable).  In Python ``var`` is a
    function rather than a template variable, so the C++ ``var<𝔹>``
    becomes ``var(𝔹)`` — the carrier is the runtime argument.
    """
    return Variable(domain)


def power_set(carrier):
    """Yield every subset of ``carrier`` as ``frozenset`` elements (lazy).

    Mirrors ``dedekind::sets::power_set<T, L, P>`` from ``expressions.cppm``.
    The carrier must be finite and its elements must be hashable; the
    iterable is materialised once at call time (to walk
    ``itertools.combinations``), but the subsets themselves are yielded
    lazily — appropriate for ``2**n`` growth.

    Mathematically the power set is unordered with no duplicates, so the
    helper deduplicates the carrier internally before enumerating.
    Callers that want a concrete set-of-subsets can wrap with
    ``set(power_set(...))`` (the yielded ``frozenset`` elements are
    hashable).
    """
    items = list(dict.fromkeys(carrier))  # dedup, preserve insertion order
    for size in range(len(items) + 1):
        for subset in combinations(items, size):
            yield frozenset(subset)


# Textbook fraktur-P alias.  ``𝔓(A)`` reads as "power set of A" the way it
# reads in standard set-theory texts (Halmos / Munkres / Lambek--Scott).
𝔓 = power_set


__all__ = [
    "set_union",
    "set_intersection",
    "set_difference",
    "set_cardinality",
    "Variable",
    "var",
    "power_set",
    "𝔓",
]
