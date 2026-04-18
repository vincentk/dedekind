"""Minimal Python MVP facade for dedekind."""

from ._dedekind import ordered_set_roundtrip
from ._dedekind import path_from_range
from ._dedekind import unordered_set_roundtrip
from .dsl import Ensemble, SetDef, pivot_table, unpivot_table

__all__ = [
    "ordered_set_roundtrip",
    "path_from_range",
    "unordered_set_roundtrip",
    "SetDef",
    "Ensemble",
    "pivot_table",
    "unpivot_table",
]