"""Python ↔ C++ sequences (finite paths) mapping.

This module re-exports the C++ `dedekind.sequences` bindings and provides
utilities for converting pandas DataFrames to products of sequences.

Sequence operations are zero-copy where possible (NumPy arrays via buffer
protocol). String sequences fall back to Python iteration.
"""

from ._dedekind import (
    ordered_set_roundtrip,
    path_from_range,
    unordered_set_roundtrip,
)

try:
    from ._dedekind import path_from_array
except ImportError:
    # Fallback for incomplete builds
    path_from_array = None

try:
    import numpy as np
    _HAS_NUMPY = True
except ImportError:
    _HAS_NUMPY = False

try:
    import pandas as pd

    _HAS_PANDAS = True
except ImportError:
    _HAS_PANDAS = False


__all__ = [
    "ordered_set_roundtrip",
    "unordered_set_roundtrip",
    "path_from_range",
    "path_from_array",
    "frame_to_paths",
]


def frame_to_paths(df):
    """Convert a pandas DataFrame to a product of sequences (finite paths).

    Each column becomes a FinitePath over its value domain. Numeric columns
    use zero-copy NumPy array binding; string columns iterate Python objects.

    Args:
        df: pandas.DataFrame

    Returns:
        dict[str, list]: mapping column name → materialized path (list)

    Raises:
        ImportError: if pandas is not available
        TypeError: if column dtype is not bool, int, float, or object (str)
    """
    if not _HAS_PANDAS:
        raise ImportError("pandas is required for frame_to_paths")
    if not _HAS_NUMPY:
        raise ImportError("numpy is required for frame_to_paths (should be auto-installed with pandas)")

    result = {}
    for col in df.columns:
        series = df[col]
        dtype_name = str(series.dtype)

        # Normalize and dispatch based on dtype
        if dtype_name == "bool" or dtype_name == "boolean":
            # BooleanDtype (nullable) → bool array with NA fill
            arr = series.to_numpy(dtype=bool, na_value=False)
            arr = np.ascontiguousarray(arr)
            result[col] = path_from_array(arr) if path_from_array else path_from_range(arr.tolist())

        elif pd.api.types.is_integer_dtype(series):
            # Integer dtype → int64 array (zero-copy)
            arr = series.to_numpy(dtype="int64")
            arr = np.ascontiguousarray(arr)
            result[col] = path_from_array(arr) if path_from_array else path_from_range(arr.tolist())

        elif pd.api.types.is_float_dtype(series):
            # Float dtype → float64 array (zero-copy)
            arr = series.to_numpy(dtype="float64")
            arr = np.ascontiguousarray(arr)
            result[col] = path_from_array(arr) if path_from_array else path_from_range(arr.tolist())

        else:
            # String / object dtype → Python sequence fallback
            result[col] = path_from_array(series.tolist())

    return result
