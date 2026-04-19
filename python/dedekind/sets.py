"""Python ↔ C++ extensional sets mapping.

This module re-exports the C++ `dedekind.sets` bindings for set algebra over
finite extensional sets.

All operations work with Python `set` objects containing homogeneous elements:
bool, int, float, or str. Nanobind dispatches at call time based on element type.
"""

from ._dedekind import set_cardinality, set_difference, set_intersection, set_union

__all__ = [
    "set_union",
    "set_intersection",
    "set_difference",
    "set_cardinality",
]
