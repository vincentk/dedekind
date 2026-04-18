"""Minimal Python MVP facade for dedekind."""

try:
    from ._dedekind import ordered_set_roundtrip
    from ._dedekind import path_from_range
    from ._dedekind import unordered_set_roundtrip
except ModuleNotFoundError:
    def _require_iterable(values, *, function_name):
        try:
            items = list(values)
        except TypeError as exc:
            raise TypeError(f"{function_name} expects an iterable") from exc
        return items

    def _is_integral(val):
        """Check if value is an integral number (not bool)."""
        from numbers import Integral
        return isinstance(val, Integral) and not isinstance(val, bool)

    def _ordered_unique(items):
        # Validate all items are integral (match C++ API contract)
        for item in items:
            if not _is_integral(item):
                raise TypeError(
                    f"ordered_set_roundtrip expects integral items; got {type(item).__name__}"
                )
        return sorted(set(items))

    def ordered_set_roundtrip(values):
        items = _require_iterable(values, function_name="ordered_set_roundtrip")
        return _ordered_unique(items)

    def unordered_set_roundtrip(values):
        items = _require_iterable(values, function_name="unordered_set_roundtrip")
        return _ordered_unique(items)

    def path_from_range(values):
        return _require_iterable(values, function_name="path_from_range")
from .dsl import (
    Activity,
    AnalystFrame,
    Dual,
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
    "ordered_set_roundtrip",
    "path_from_range",
    "unordered_set_roundtrip",
    "SetDef",
    "Ensemble",
    "AnalystFrame",
    "Dual",
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