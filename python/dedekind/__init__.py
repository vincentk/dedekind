"""Minimal Python MVP facade for dedekind."""

from ._dedekind import ordered_set_roundtrip
from ._dedekind import path_from_range
from ._dedekind import unordered_set_roundtrip

__all__ = [
    "ordered_set_roundtrip",
    "path_from_range",
    "unordered_set_roundtrip",
]