"""Minimal Python MVP facade for dedekind."""

try:
    from ._dedekind import ordered_set_roundtrip
    from ._dedekind import path_from_array
    from ._dedekind import path_from_range
    from ._dedekind import unordered_set_roundtrip
    from ._dedekind import set_union
    from ._dedekind import set_intersection
    from ._dedekind import set_difference
    from ._dedekind import set_cardinality
    from ._dedekind import Complex
    from ._dedekind import Dual
except ModuleNotFoundError as _exc:
    raise ImportError(
        "The dedekind C++ extension (_dedekind) is not available. "
        "Build it with `cmake --build build` (or `make`) from the repository "
        "root, then install with `pip install -e .`."
    ) from _exc

# Lazy-load submodules on first access to avoid circular imports
def __getattr__(name):
    """Lazy-load submodules (sequences, sets) on demand."""
    import importlib

    if name in {"sequences", "sets"}:
        module_name = f"{__name__}.{name}"
        try:
            return importlib.import_module(f".{name}", __name__)
        except ModuleNotFoundError as exc:
            # Keep hasattr/getattr semantics: missing optional submodules
            # should surface as AttributeError, not ModuleNotFoundError.
            if exc.name == module_name:
                raise AttributeError(
                    f"module {__name__!r} has no attribute {name!r}"
                ) from None
            raise
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")

# Top-level utility: frame_to_paths convenience alias
# (imported after dsl to avoid circular dependency with frame_to_paths)
try:
    from .sequences import frame_to_paths
except ImportError:
    frame_to_paths = None

from .dsl import (
    Activity,
    AnalystFrame,
    Ensemble,
    LinearChoice,
    SetDef,
    align_pivot_table,
    analyst_sales_quality_lift_report,
    critical_path_schedule,
    dual_derivative,
    monthly_category_report,
    pivot_table,
    pivot_mean_absolute_error,
    pivot_quality_report,
    repair_missing_by_group_ratio,
    repair_missing_by_product,
    solve_mixed_integer_plan,
    smart_join,
    smart_pivot,
    table_quality_metrics,
    table,
    unpivot_table,
)

__all__ = [
    # C++ bindings (backward compat)
    "ordered_set_roundtrip",
    "path_from_array",
    "path_from_range",
    "unordered_set_roundtrip",
    "set_union",
    "set_intersection",
    "set_difference",
    "set_cardinality",
    "Complex",
    "Dual",
    # Submodules
    "sets",
    "sequences",
    # Top-level convenience
    "frame_to_paths",
    # DSL classes
    "SetDef",
    "Ensemble",
    "AnalystFrame",
    "LinearChoice",
    "Activity",
    "table",
    "smart_join",
    "smart_pivot",
    "pivot_table",
    "unpivot_table",
    "monthly_category_report",
    "repair_missing_by_group_ratio",
    "repair_missing_by_product",
    "table_quality_metrics",
    "analyst_sales_quality_lift_report",
    "align_pivot_table",
    "pivot_mean_absolute_error",
    "pivot_quality_report",
    "dual_derivative",
    "solve_mixed_integer_plan",
    "critical_path_schedule",
]
