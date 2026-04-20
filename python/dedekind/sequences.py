"""Python ↔ C++ sequences (finite paths) mapping.

This module re-exports the C++ `dedekind.sequences` bindings and provides
utilities for converting pandas DataFrames to products of sequences.

Sequence operations prefer contiguous typed arrays for efficient transfer to
the C++ bindings. String sequences fall back to Python iteration.
"""

from ._dedekind import (
    ordered_set_roundtrip,
    path_from_bool_array,
    path_from_float64_array,
    path_from_int64_array,
    path_from_range,
    path_from_str_seq,
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
    "path_from_bool_array",
    "path_from_int64_array",
    "path_from_float64_array",
    "path_from_str_seq",
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
            arr = np.ascontiguousarray(
                series.to_numpy(dtype=np.bool_, na_value=False, copy=False),
                dtype=np.bool_,
            ).reshape(-1)
            if path_from_array:
                try:
                    result[col] = path_from_bool_array(arr)
                except TypeError:
                    # Some platforms expose NumPy bool arrays in a way that can
                    # fail strict nanobind signature matching; keep a safe fallback.
                    result[col] = arr.tolist()
            else:
                result[col] = arr.tolist()

        elif pd.api.types.is_integer_dtype(series):
            # Integer dtype → int64 array (contiguous where needed)
            arr = np.ascontiguousarray(
                series.to_numpy(dtype=np.int64, copy=False),
                dtype=np.int64,
            ).reshape(-1)
            if path_from_array:
                try:
                    result[col] = path_from_int64_array(arr)
                except TypeError:
                    # Some platforms expose NumPy int64 arrays in a way that can
                    # fail strict nanobind signature matching; keep a safe fallback.
                    result[col] = path_from_range(arr.tolist())
            else:
                result[col] = path_from_range(arr.tolist())

        elif pd.api.types.is_float_dtype(series):
            # Float dtype → float64 array (contiguous where needed)
            arr = np.ascontiguousarray(
                series.to_numpy(dtype=np.float64, copy=False),
                dtype=np.float64,
            ).reshape(-1)
            if path_from_array:
                try:
                    result[col] = path_from_float64_array(arr)
                except TypeError:
                    # Some platforms expose NumPy float64 arrays in a way that can
                    # fail strict nanobind signature matching; keep a safe fallback.
                    result[col] = arr.tolist()
            else:
                result[col] = arr.tolist()

        else:
            # String / object dtype → Python sequence fallback
            result[col] = path_from_str_seq(series.tolist())

    return result
