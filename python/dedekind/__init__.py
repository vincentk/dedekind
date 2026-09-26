"""Minimal Python facade for dedekind: native (``_dedekind``) bindings only.

The pure-Python prototype modules (``dsl`` / ``sequences`` / ``sets`` / ``lwv`` /
``numbers`` / ``algebra``) were removed as pre-target-architecture technical
debt (#886). The target operating model is handle-only over the native core:
all predicate structure and reduction stay on the C++ side, and Python holds
handles plus accessibility bits. The native handle surface (e.g. the canonical
sets) is reached through ``dedekind._dedekind``.
"""

try:
    from ._dedekind import ordered_set_roundtrip
    from ._dedekind import path_from_array
    from ._dedekind import path_from_range
    from ._dedekind import unordered_set_roundtrip
    from ._dedekind import set_union
    from ._dedekind import set_intersection
    from ._dedekind import set_difference
    from ._dedekind import set_cardinality
    # Canonical native sets (#886).  Python NFKC-normalises identifiers, so the
    # double-struck attrs `B` / `N` are equally reachable as `𝔹` / `ℕ`; the
    # discriminating ℕ⊂ℤ classifier is keyed `Nat` to avoid the `ℕ` → "N"
    # collision.  `ext` is the native materialise retraction μ: Int ⇀ Ext.
    from ._dedekind import B
    from ._dedekind import N
    from ._dedekind import Nat
    from ._dedekind import ext
except ModuleNotFoundError as _exc:
    raise ImportError(
        "The dedekind C++ extension (_dedekind) is not available. "
        "Build it with `cmake --build build` (or `make`) from the repository "
        "root, then install with `pip install -e .`."
    ) from _exc

__all__ = [
    "ordered_set_roundtrip",
    "path_from_array",
    "path_from_range",
    "unordered_set_roundtrip",
    "set_union",
    "set_intersection",
    "set_difference",
    "set_cardinality",
    "B",
    "N",
    "Nat",
    "ext",
]
