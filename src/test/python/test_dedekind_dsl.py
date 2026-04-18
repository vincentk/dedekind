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

    def test_monthly_report_and_repair_helpers(self) -> None:
        sales = pd.DataFrame(
            [
                {"date": "2026-01-05", "category": "hardware", "units": 10.0, "revenue": 100.0},
                {"date": "2026-01-07", "category": "software", "units": 5.0, "revenue": 250.0},
                {"date": "2026-02-03", "category": "hardware", "units": 8.0, "revenue": 80.0},
                {"date": "2026-02-11", "category": "accessories", "units": 10.0, "revenue": None},
            ]
        )
        price_reference = pd.DataFrame(
            [
                {"category": "hardware", "unit_price": 10.0},
                {"category": "software", "unit_price": 50.0},
                {"category": "accessories", "unit_price": 20.0},
            ]
        )

        joined = dedekind.table(sales).join(dedekind.table(price_reference), on="category", how="left")
        repaired = dedekind.repair_missing_by_product(
            joined,
            value_column="revenue",
            factor_columns=["units", "unit_price"],
        )
        report = dedekind.monthly_category_report(repaired, measures={"revenue": ("revenue", "sum")})
        pivoted = report.pivot(index="month", columns="category", values="revenue", fill_value=0).realize()

        self.assertEqual(pivoted.to_dict(orient="records"), [
            {"month": "2026-01", "accessories": 0.0, "hardware": 100.0, "software": 250.0},
            {"month": "2026-02", "accessories": 200.0, "hardware": 80.0, "software": 0.0},
        ])

    def test_pivot_quality_report(self) -> None:
        reference = pd.DataFrame(
            [
                {"month": "2026-01", "hardware": 100.0, "software": 250.0},
                {"month": "2026-02", "hardware": 80.0, "software": 200.0},
            ]
        )
        candidate = pd.DataFrame(
            [
                {"month": "2026-01", "hardware": 90.0, "software": 250.0},
                {"month": "2026-02", "hardware": 80.0, "software": 180.0},
            ]
        )

        quality = dedekind.pivot_quality_report(
            reference,
            repaired=candidate,
            value_columns=["hardware", "software"],
        )
        self.assertEqual(list(quality["stage"]), ["repaired"])
        self.assertGreater(float(quality.loc[0, "pivot_mae"]), 0.0)

    def test_to_set_supports_string_values_without_native_extension(self) -> None:
        frame = dedekind.table(pd.DataFrame([
            {"region": "south"},
            {"region": "north"},
            {"region": "south"},
        ]))

        self.assertEqual(frame.to_set("region").realize(), ["north", "south"])

    def test_analyst_sales_quality_lift_report(self) -> None:
        sales = pd.DataFrame(
            [
                {"date": "2026-01-05", "product_id": " P1 ", "region": "North", "units": "10", "revenue": "100"},
                {"date": "2026-01-07", "product_id": "p2", "region": "NORTH", "units": "5", "revenue": "$250"},
                {"date": "2026-01-09", "product_id": "p3", "region": "south", "units": 7, "revenue": "140.0"},
                {"date": "2026-02-03", "product_id": "p1", "region": " SOUTH ", "units": 8, "revenue": "80"},
                {"date": "2026-02-10", "product_id": "p2", "region": "north", "units": "4x", "revenue": "200"},
                {"date": "2026-02-11", "product_id": "p3", "region": "south", "units": 10, "revenue": "oops"},
            ]
        )
        products = pd.DataFrame(
            [
                {"product_id": "p1", "category": " hardware "},
                {"product_id": "p2", "category": "SOFTWARE"},
                {"product_id": "p3", "category": "Accessories"},
            ]
        )
        regions = pd.DataFrame(
            [
                {"region": "north", "segment": "Enterprise"},
                {"region": "south", "segment": "SMB"},
            ]
        )
        product_price_hq = pd.DataFrame(
            [
                {"product_id": "p1", "unit_price_hq": 10.0},
                {"product_id": "p2", "unit_price_hq": 50.0},
                {"product_id": "p3", "unit_price_hq": 20.0},
            ]
        )
        reference_pivot = pd.DataFrame(
            [
                {"month": "2026-01", "accessories": 140.0, "hardware": 100.0, "software": 250.0},
                {"month": "2026-02", "accessories": 200.0, "hardware": 80.0, "software": 200.0},
            ]
        )

        report = dedekind.analyst_sales_quality_lift_report(
            dedekind.table("sales", sales),
            dedekind.table("products", products),
            dedekind.table("regions", regions),
            product_price_hq=dedekind.table("product_price_hq", product_price_hq),
            reference_pivot=reference_pivot,
        )

        self.assertEqual(sorted(report["input_quality"]["table"].tolist()), [
            "table_a_sales",
            "table_b_products",
            "table_regions",
        ])
        self.assertEqual(
            report["smart_pivot"].to_dict(orient="records"),
            [
                {"month": "2026-01", "accessories": 140.0, "hardware": 100.0, "software": 250.0},
                {"month": "2026-02", "accessories": 200.0, "hardware": 80.0, "software": 200.0},
            ],
        )
        self.assertGreater(float(report["quality_delta"].loc[0, "improvement"]), 0.0)


@unittest.skipIf(pd is None, "pandas is required for formal DSL middleware tests")
class DedekindDslOptimizationTest(unittest.TestCase):
    def test_dual_derivative(self) -> None:
        value, derivative = dedekind.dual_derivative(lambda x: x * x + 3 * x, 2.0)
        self.assertEqual(value, 10.0)
        self.assertEqual(derivative, 7.0)

    def test_solve_mixed_integer_plan(self) -> None:
        result = dedekind.solve_mixed_integer_plan(
            [
                dedekind.LinearChoice("cache", value=9.0, cost=4.0, max_units=1),
                dedekind.LinearChoice("bridge", value=7.0, cost=3.0, max_units=1),
                dedekind.LinearChoice("audit", value=6.0, cost=2.0, max_units=2),
            ],
            budget=7.0,
        )
        selected = result["decision_table"]
        picked = selected[selected["quantity"] > 0].set_index("name")["quantity"].to_dict()
        self.assertEqual(result["cost"], 7.0)
        self.assertEqual(result["objective"], 19.0)
        self.assertEqual(picked, {"audit": 2, "bridge": 1})

    def test_critical_path_schedule(self) -> None:
        result = dedekind.critical_path_schedule(
            [
                dedekind.Activity("spec", duration=2.0),
                dedekind.Activity("kernel", duration=3.0, depends_on=("spec",)),
                dedekind.Activity("tests", duration=2.0, depends_on=("kernel",)),
                dedekind.Activity("docs", duration=1.0, depends_on=("spec",)),
            ]
        )
        self.assertEqual(result["project_duration"], 7.0)
        self.assertEqual(result["critical_path"], ["spec", "kernel", "tests"])


if __name__ == "__main__":
    unittest.main()
