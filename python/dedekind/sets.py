"""Python ↔ C++ extensional sets mapping.

This module re-exports the C++ `dedekind.sets` bindings for set algebra over
finite extensional sets.

All operations work with Python `set` objects containing homogeneous elements:
bool, int, float, or str. Nanobind dispatches at call time based on element type.

The pure-Python ``power_set`` / ``𝔓`` helpers below mirror the C++
``dedekind::sets::power_set`` template (see
``src/main/modules/dedekind/sets/expressions.cppm``); they enumerate every
subset of a finite iterable and return them as a list of ``frozenset``
elements, suitable for set-builder DSL use.  ``𝔓`` is the textbook
fraktur-P alias for ``power_set``.
"""

from itertools import combinations

from ._dedekind import set_cardinality, set_difference, set_intersection, set_union


def power_set(carrier):
    """Yield every subset of ``carrier`` as a list of ``frozenset`` elements.

    Mirrors ``dedekind::sets::power_set<T, L, P>`` from
    ``expressions.cppm``.  For ``carrier`` of cardinality ``n`` the returned
    list has cardinality ``2 ** n``.  The carrier must be finite and its
    elements must be hashable (Python ``set`` requirement).
    """
    items = list(carrier)
    return [
        frozenset(subset)
        for size in range(len(items) + 1)
        for subset in combinations(items, size)
    ]


# Textbook fraktur-P alias.  ``𝔓(A)`` reads as "power set of A" the way it
# reads in standard set-theory texts (Halmos / Munkres / Lambek--Scott).
𝔓 = power_set


__all__ = [
    "set_union",
    "set_intersection",
    "set_difference",
    "set_cardinality",
    "power_set",
    "𝔓",
]
