import unittest

import dedekind

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


class DedekindPythonSmokeTest(unittest.TestCase):
    def test_ordered_set_roundtrip(self) -> None:
        self.assertEqual(dedekind.ordered_set_roundtrip([3, 1, 2, 2]), [1, 2, 3])

    def test_unordered_set_roundtrip(self) -> None:
        self.assertEqual(
            dedekind.unordered_set_roundtrip([4, 2, 4, 1]),
            [1, 2, 4],
        )

    def test_path_from_range(self) -> None:
        self.assertEqual(dedekind.path_from_range([2, 4, 6, 8]), [2, 4, 6, 8])

    def test_ordered_set_roundtrip_rejects_non_integral_items(self) -> None:
        with self.assertRaises(TypeError):
            dedekind.ordered_set_roundtrip([1, "x", 3])

    def test_unordered_set_roundtrip_rejects_non_iterable(self) -> None:
        with self.assertRaises(TypeError):
            dedekind.unordered_set_roundtrip(42)

    def test_path_from_range_rejects_non_integral_items(self) -> None:
        with self.assertRaises(TypeError):
            dedekind.path_from_range([1, None, 3])


class DedekindSetAlgebraTest(unittest.TestCase):
    """Smoke tests for the extensional set-algebra bindings (dedekind.sets)."""

    # ── int overload ──────────────────────────────────────────────────────
    def test_set_union_ints(self) -> None:
        self.assertEqual(dedekind.set_union({1, 2, 3}, {2, 3, 4}), {1, 2, 3, 4})

    def test_set_intersection_ints(self) -> None:
        self.assertEqual(dedekind.set_intersection({1, 2, 3}, {2, 3, 4}), {2, 3})

    def test_set_difference_ints(self) -> None:
        self.assertEqual(dedekind.set_difference({1, 2, 3}, {2, 3, 4}), {1})

    def test_set_cardinality_ints(self) -> None:
        self.assertEqual(dedekind.set_cardinality({7, 8, 9}), 3)

    # ── bool overload ─────────────────────────────────────────────────────
    def test_set_union_bools(self) -> None:
        self.assertEqual(dedekind.set_union({True}, {False}), {True, False})

    def test_set_intersection_bools(self) -> None:
        self.assertEqual(dedekind.set_intersection({True, False}, {False}), {False})

    def test_set_difference_bools(self) -> None:
        self.assertEqual(dedekind.set_difference({True, False}, {False}), {True})

    def test_set_cardinality_bools(self) -> None:
        self.assertEqual(dedekind.set_cardinality({True, False}), 2)

    # ── float (double) overload ───────────────────────────────────────────
    def test_set_union_floats(self) -> None:
        self.assertEqual(dedekind.set_union({1.0, 2.0}, {2.0, 3.0}), {1.0, 2.0, 3.0})

    def test_set_intersection_floats(self) -> None:
        self.assertEqual(dedekind.set_intersection({1.0, 2.0}, {2.0, 3.0}), {2.0})

    def test_set_difference_floats(self) -> None:
        self.assertEqual(dedekind.set_difference({1.0, 2.0}, {2.0, 3.0}), {1.0})

    def test_set_cardinality_floats(self) -> None:
        self.assertEqual(dedekind.set_cardinality({1.5, 2.5, 3.5}), 3)

    # ── str overload ──────────────────────────────────────────────────────
    def test_set_union_strs(self) -> None:
        self.assertEqual(
            dedekind.set_union({"a", "b"}, {"b", "c"}), {"a", "b", "c"}
        )

    def test_set_intersection_strs(self) -> None:
        self.assertEqual(dedekind.set_intersection({"a", "b"}, {"b", "c"}), {"b"})

    def test_set_difference_strs(self) -> None:
        self.assertEqual(dedekind.set_difference({"a", "b"}, {"b", "c"}), {"a"})

    def test_set_cardinality_strs(self) -> None:
        self.assertEqual(dedekind.set_cardinality({"x", "y"}), 2)

    # ── identity / empty-set properties ──────────────────────────────────
    def test_union_with_empty(self) -> None:
        self.assertEqual(dedekind.set_union({1, 2}, set()), {1, 2})

    def test_intersection_with_empty(self) -> None:
        self.assertEqual(dedekind.set_intersection({1, 2}, set()), set())

    def test_difference_with_self(self) -> None:
        self.assertEqual(dedekind.set_difference({1, 2}, {1, 2}), set())

    def test_cardinality_empty(self) -> None:
        self.assertEqual(dedekind.set_cardinality(set()), 0)


@unittest.skipUnless(_HAS_NUMPY, "numpy required for path_from_array tests")
class DedekindArrayBindingsTest(unittest.TestCase):
    """Test zero-copy NumPy ndarray path_from_array bindings."""

    def test_path_from_array_bool(self) -> None:
        arr = np.array([True, False, True], dtype=bool)
        result = dedekind.path_from_array(arr)
        self.assertEqual(result, [True, False, True])

    def test_path_from_array_int64(self) -> None:
        arr = np.array([10, 20, 30], dtype=np.int64)
        result = dedekind.path_from_array(arr)
        self.assertEqual(result, [10, 20, 30])

    def test_path_from_array_float64(self) -> None:
        arr = np.array([1.5, 2.5, 3.5], dtype=np.float64)
        result = dedekind.path_from_array(arr)
        self.assertEqual(result, [1.5, 2.5, 3.5])

    def test_path_from_array_empty(self) -> None:
        arr = np.array([], dtype=np.int64)
        result = dedekind.path_from_array(arr)
        self.assertEqual(result, [])

    def test_path_from_array_preserves_order(self) -> None:
        arr = np.array([5, 3, 9, 1], dtype=np.int64)
        result = dedekind.path_from_array(arr)
        self.assertEqual(result, [5, 3, 9, 1])


@unittest.skipUnless(_HAS_PANDAS, "pandas required for frame_to_paths tests")
class DedekindFramePathsTest(unittest.TestCase):
    """Test pandas DataFrame → product-of-sequences conversion."""

    def test_frame_to_paths_int_columns(self) -> None:
        df = pd.DataFrame({"a": [1, 2, 3], "b": [10, 20, 30]})
        paths = dedekind.frame_to_paths(df)
        self.assertEqual(paths["a"], [1, 2, 3])
        self.assertEqual(paths["b"], [10, 20, 30])

    def test_frame_to_paths_mixed_types(self) -> None:
        df = pd.DataFrame({
            "ints": [1, 2, 3],
            "floats": [1.1, 2.2, 3.3],
            "bools": [True, False, True],
        })
        paths = dedekind.frame_to_paths(df)
        self.assertEqual(paths["ints"], [1, 2, 3])
        self.assertEqual(paths["floats"], [1.1, 2.2, 3.3])
        self.assertEqual(paths["bools"], [True, False, True])

    def test_frame_to_paths_string_column(self) -> None:
        df = pd.DataFrame({"labels": ["a", "b", "c"]})
        paths = dedekind.frame_to_paths(df)
        self.assertEqual(paths["labels"], ["a", "b", "c"])

    def test_frame_to_paths_empty_dataframe(self) -> None:
        df = pd.DataFrame()
        paths = dedekind.frame_to_paths(df)
        self.assertEqual(paths, {})

    def test_frame_to_paths_single_row(self) -> None:
        df = pd.DataFrame({"x": [42], "y": [3.14]})
        paths = dedekind.frame_to_paths(df)
        self.assertEqual(paths["x"], [42])
        self.assertEqual(paths["y"], [3.14])


if __name__ == "__main__":
    unittest.main()