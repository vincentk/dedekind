import unittest

import dedekind

try:
    import pandas as pd
except ModuleNotFoundError:  # pragma: no cover - guarded for environments without pandas
    pd = None


@unittest.skipIf(pd is None, "pandas is required for DSL smart_pivot tests")
class DedekindDslSmartPivotTest(unittest.TestCase):
    def test_smart_pivot_infers_axes_and_compresses_categories(self) -> None:
        df = pd.DataFrame(
            [
                {"month": "2026-01", "category": "hardware", "revenue": 120.0},
                {"month": "2026-01", "category": "software", "revenue": 200.0},
                {"month": "2026-01", "category": "accessories", "revenue": 80.0},
                {"month": "2026-01", "category": "services", "revenue": 30.0},
                {"month": "2026-01", "category": "education", "revenue": 10.0},
                {"month": "2026-01", "category": "support", "revenue": 8.0},
                {"month": "2026-02", "category": "hardware", "revenue": 130.0},
                {"month": "2026-02", "category": "software", "revenue": 210.0},
                {"month": "2026-02", "category": "accessories", "revenue": 90.0},
                {"month": "2026-02", "category": "services", "revenue": 35.0},
                {"month": "2026-02", "category": "education", "revenue": 9.0},
                {"month": "2026-02", "category": "support", "revenue": 7.0},
            ]
        )

        pivoted = dedekind.smart_pivot(
            df,
            max_columns=4,
            min_coverage=0.85,
            other_label="__other__",
        )

        realized = pivoted.realize()
        diagnostics = pivoted._smart_pivot_diagnostics

        self.assertEqual(diagnostics["index"], "month")
        self.assertEqual(diagnostics["columns"], "category")
        self.assertEqual(diagnostics["values"], "revenue")
        self.assertTrue(diagnostics["compressed"])
        self.assertLessEqual(diagnostics["columns_after"], 4)
        self.assertIn("__other__", realized.columns)
        self.assertGreater(diagnostics["cell_compression_ratio"], 0)

    def test_smart_pivot_respects_explicit_axes_without_compression(self) -> None:
        df = pd.DataFrame(
            [
                {"month": "2026-01", "segment": "A", "revenue": 100.0},
                {"month": "2026-01", "segment": "B", "revenue": 60.0},
                {"month": "2026-02", "segment": "A", "revenue": 110.0},
                {"month": "2026-02", "segment": "B", "revenue": 70.0},
            ]
        )

        pivoted = dedekind.table(df).smart_pivot(
            index="month",
            columns="segment",
            values="revenue",
            max_columns=None,
        )
        realized = pivoted.realize()
        diagnostics = pivoted._smart_pivot_diagnostics

        self.assertFalse(diagnostics["compressed"])
        self.assertEqual(realized.to_dict(orient="records"), [
            {"month": "2026-01", "A": 100.0, "B": 60.0},
            {"month": "2026-02", "A": 110.0, "B": 70.0},
        ])


if __name__ == "__main__":
    unittest.main()
