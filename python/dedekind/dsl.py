"""Analyst and formal DSL shim classes for set operations.

This module provides prototype DSL interfaces demonstrating the intended
ergonomics for end-user set manipulation. These are executable shims for
UX validation (issue #241); output correctness refinement is a follow-up.
"""

from dataclasses import dataclass
from itertools import product
from numbers import Integral

from ._dedekind import (
    ordered_set_roundtrip,
    unordered_set_roundtrip,
    set_union,
    set_intersection,
    set_difference,
    set_cardinality,
)

try:
    import pandas as pd

    _HAS_PANDAS = True
except ImportError:
    _HAS_PANDAS = False


def _require_pandas():
    if not _HAS_PANDAS:
        raise ImportError("pandas is required for DataFrame/pivot shims")


def _as_column_list(columns):
    if isinstance(columns, str):
        return [columns]
    return list(columns)


def _normalize_join_value(series):
    # FIXME #253: replace with match ladder from smart join kernel
    # (strip → lower → alphanum fold → phonetic/synonym tiers)
    return (
        series.astype("string")
        .fillna("")
        .str.strip()
        .str.lower()
        .str.replace(r"[^a-z0-9]+", "", regex=True)
    )


def _is_text_column(series):
    dtype_name = str(series.dtype)
    return dtype_name == "string" or dtype_name == "object"


def _realize_extensional_values(values, *, ordered=True):
    """Realize extensional set members with integer fast-path + generic fallback.

    The C++ facade roundtrip functions currently accept integral sequences.
    For non-integral element domains (e.g. region labels), fall back to
    Python set semantics so formal/analyst shims remain executable.
    """
    if values is None:
        return []

    normalized = list(values)
    int_like = all(isinstance(v, Integral) and not isinstance(v, bool) for v in normalized)

    if int_like:
        int_values = [int(v) for v in normalized]
        if ordered:
            return ordered_set_roundtrip(int_values)
        return unordered_set_roundtrip(int_values)

    unique = set(normalized)
    if ordered:
        try:
            return sorted(unique)
        except TypeError:
            return sorted(unique, key=lambda v: str(v))
    return list(unique)


def _infer_join_keys(left_df, right_df):
    # FIXME #253: replace heuristic scoring with the core smart join kernel's
    # key-inference algorithm (uniqueness + completeness + semantic type)
    # FIXME #250: once panel primitives land, entity-key and time-index
    # columns should be preferred as inferred join keys automatically
    common = sorted(set(left_df.columns) & set(right_df.columns))
    if not common:
        raise ValueError("smart_join could not infer keys: no shared column names")

    scored = []
    for col in common:
        left_col = left_df[col]
        right_col = right_df[col]

        left_non_null = max(1, int(left_col.notna().sum()))
        right_non_null = max(1, int(right_col.notna().sum()))

        left_unique_ratio = float(left_col.nunique(dropna=True)) / left_non_null
        right_unique_ratio = float(right_col.nunique(dropna=True)) / right_non_null
        completeness = 1.0 - float(left_col.isna().mean() + right_col.isna().mean()) / 2.0

        name_bonus = 0.0
        lowered = col.lower()
        for token in ("id", "key", "code", "name", "region", "product", "category"):
            if token in lowered:
                name_bonus += 0.2

        type_bonus = 0.2 if _is_text_column(left_col) and _is_text_column(right_col) else 0.0
        score = completeness + right_unique_ratio + 0.5 * left_unique_ratio + name_bonus + type_bonus

        scored.append((score, col))

    scored.sort(reverse=True)
    return [scored[0][1]]


def _is_numeric_column(series):
    return bool(pd.api.types.is_numeric_dtype(series))


def _looks_like_time_axis(column_name, series):
    lowered = str(column_name).lower()
    if any(token in lowered for token in ("date", "month", "time", "year", "week", "day")):
        return True
    if pd.api.types.is_datetime64_any_dtype(series):
        return True
    return False


def _infer_smart_pivot_axes(df, *, index=None, columns=None, values=None):
    resolved_index = index
    resolved_columns = columns
    resolved_values = values

    if resolved_values is None:
        numeric_candidates = [
            col for col in df.columns
            if _is_numeric_column(df[col]) and col not in {resolved_index, resolved_columns}
        ]
        if not numeric_candidates:
            raise ValueError(
                "smart_pivot could not infer values column: provide values=... or include a numeric column"
            )
        resolved_values = numeric_candidates[0]

    if resolved_index is None:
        time_candidates = [col for col in df.columns if _looks_like_time_axis(col, df[col])]
        if time_candidates:
            resolved_index = time_candidates[0]
        else:
            fallback = [
                col for col in df.columns
                if col != resolved_values and not _is_numeric_column(df[col])
            ]
            if not fallback:
                raise ValueError(
                    "smart_pivot could not infer index column: provide index=..."
                )
            resolved_index = fallback[0]

    if resolved_columns is None:
        candidates = [
            col for col in df.columns
            if col not in {resolved_index, resolved_values}
        ]
        if not candidates:
            raise ValueError(
                "smart_pivot could not infer columns axis: provide columns=..."
            )

        scored = []
        for col in candidates:
            series = df[col]
            non_null = max(1, int(series.notna().sum()))
            cardinality_ratio = float(series.nunique(dropna=True)) / non_null
            text_bonus = 0.4 if _is_text_column(series) else 0.0
            name_bonus = 0.0
            lowered = col.lower()
            for token in ("category", "segment", "region", "type", "class", "group"):
                if token in lowered:
                    name_bonus += 0.2
            score = cardinality_ratio + text_bonus + name_bonus
            scored.append((score, col))

        scored.sort(reverse=True)
        resolved_columns = scored[0][1]

    return resolved_index, resolved_columns, resolved_values


def _compress_pivot_categories(
    df,
    *,
    columns,
    values,
    max_columns,
    min_coverage,
    other_label,
):
    if max_columns is None:
        return df, {
            "compressed": False,
            "columns_before": int(df[columns].nunique(dropna=True)),
            "columns_after": int(df[columns].nunique(dropna=True)),
            "value_coverage": 1.0,
            "other_rows": 0,
        }

    unique_columns = int(df[columns].nunique(dropna=True))
    if unique_columns <= max_columns:
        return df, {
            "compressed": False,
            "columns_before": unique_columns,
            "columns_after": unique_columns,
            "value_coverage": 1.0,
            "other_rows": 0,
        }

    by_value = (
        df.groupby(columns, dropna=False)[values]
        .apply(lambda s: s.abs().sum(skipna=True))
        .sort_values(ascending=False)
    )
    total = float(by_value.sum())

    if total <= 0:
        keep_count = max(1, max_columns - 1)
        keep_labels = set(by_value.head(keep_count).index.tolist())
        coverage = float(len(keep_labels)) / max(1.0, float(len(by_value)))
    else:
        keep_labels = set()
        running = 0.0
        for label, amount in by_value.items():
            if len(keep_labels) >= max_columns - 1:
                break
            keep_labels.add(label)
            running += float(amount)
            if running / total >= min_coverage:
                break
        if not keep_labels:
            keep_labels.add(by_value.index[0])
            running = float(by_value.iloc[0])
        coverage = running / total

    compressed_df = df.copy()
    keep_mask = compressed_df[columns].isin(keep_labels)
    other_rows = int((~keep_mask).sum())
    compressed_df.loc[~keep_mask, columns] = other_label

    columns_after = int(compressed_df[columns].nunique(dropna=True))
    return compressed_df, {
        "compressed": True,
        "columns_before": unique_columns,
        "columns_after": columns_after,
        "value_coverage": float(coverage),
        "other_rows": other_rows,
    }


class AnalystGrouped:
    """Grouped analyst relation with named aggregation combinators."""

    def __init__(self, frame, keys):
        self._frame = frame
        self._keys = _as_column_list(keys)

    def summarize(self, **measures):
        """Aggregate grouped rows using named measures.

        Example:
            .summarize(revenue=("revenue", "sum"), units=("units", "sum"))
        """
        _require_pandas()
        if not measures:
            raise ValueError("summarize(...) requires at least one named measure")

        named_aggs = {}
        for alias, spec in measures.items():
            if not (isinstance(spec, tuple) and len(spec) == 2):
                raise TypeError(
                    f"Measure '{alias}' must be a tuple of (column, aggfunc); got {spec!r}"
                )
            column, aggfunc = spec
            named_aggs[alias] = pd.NamedAgg(column=column, aggfunc=aggfunc)

        grouped = self._frame._df.groupby(self._keys, as_index=False).agg(**named_aggs)
        plan = self._frame._plan + [f"summarize(keys={self._keys}, measures={list(measures.keys())})"]
        return AnalystFrame(
            grouped,
            plan=plan,
            name=self._frame._name,
            issues=self._frame._issues,
        )


class AnalystFrame:
    """Analyst-tier relation wrapper with declarative combinators.

    This is a pandas-backed shim that keeps notebook code focused on intent
    while core relational operators evolve in the C++ layer.

    FIXME #250: once panel primitives land (entity key, time index, grain),
        AnalystFrame should carry explicit panel metadata so that smart_join
        can enforce grain constraints without user-supplied hints.
    FIXME #251: projection() and grouped aggregation should delegate to the
        core relational operators rather than pandas groupby directly.
    """

    def __init__(self, df, *, plan=None, name=None, issues=None):
        _require_pandas()
        self._df = df.copy()
        self._plan = list(plan or [])
        self._name = name
        self._issues = list(issues or [])

    def _spawn(self, df, step, *, issues=None):
        return AnalystFrame(
            df,
            plan=self._plan + [step],
            name=self._name,
            issues=self._issues if issues is None else issues,
        )

    def _record_issue(self, *, issue_type, column, count, details=None):
        if count <= 0:
            return
        payload = {
            "issue_type": issue_type,
            "column": column,
            "count": int(count),
            "step": len(self._plan),
            "relation": self._name,
            "details": details or "",
        }
        self._issues.append(payload)

    def where(self, predicate, *, label=None):
        """Filter rows by query string or DataFrame->mask callable."""
        if isinstance(predicate, str):
            filtered = self._df.query(predicate)
            step = label or f"where(query={predicate!r})"
        elif callable(predicate):
            mask = predicate(self._df)
            filtered = self._df[mask]
            step = label or "where(callable)"
        else:
            raise TypeError("where(...) expects a query string or callable")

        return self._spawn(filtered, step)

    def project(self, columns):
        cols = _as_column_list(columns)
        projected = self._df.loc[:, cols]
        return self._spawn(projected, f"project({cols})")

    def select(self, columns):
        """Alias for project(...) to match analyst intuition."""
        return self.project(columns)

    def derive(self, **new_columns):
        """Add derived columns from callables or constant values."""
        derived = self._df.copy()
        for name, expr in new_columns.items():
            derived[name] = expr(derived) if callable(expr) else expr
        return self._spawn(derived, f"derive({list(new_columns.keys())})")

    def derive_period(self, source_column, *, target_column="period", freq="M"):
        """Derive a period label from a timestamp-like column.

        FIXME #250: when panel primitives land, period derivation should bind to
            explicit time-index metadata rather than ad-hoc string columns.
        """
        derived = self._df.copy()
        derived[target_column] = pd.to_datetime(derived[source_column]).dt.to_period(freq).astype(str)
        return self._spawn(
            derived,
            f"derive_period(source={source_column}, target={target_column}, freq={freq})",
        )

    def normalize_text(
        self,
        columns,
        *,
        strip=True,
        lower=True,
        missing_tokens=("", "na", "n/a", "null", "none"),
    ):
        """Normalize text columns and canonicalize common missing tokens."""
        cols = _as_column_list(columns)
        normalized = self._df.copy()
        token_set = {t.lower() for t in missing_tokens}

        for col in cols:
            series = normalized[col].astype("string")
            if strip:
                series = series.str.strip()
            if lower:
                series = series.str.lower()

            is_missing_token = series.fillna("").str.lower().isin(token_set)
            replaced = int(is_missing_token.sum())
            if replaced:
                self._record_issue(
                    issue_type="normalized_missing_token",
                    column=col,
                    count=replaced,
                    details=f"tokens={sorted(token_set)}",
                )
            normalized[col] = series.mask(is_missing_token, pd.NA)

        return self._spawn(normalized, f"normalize_text({cols})")

    def coerce_numeric(self, columns, *, errors="coerce", strip_symbols=True):
        """Convert columns to numeric and report coercion losses."""
        cols = _as_column_list(columns)
        coerced = self._df.copy()

        for col in cols:
            raw = coerced[col]
            prepared = raw
            if strip_symbols:
                prepared = prepared.astype("string").str.replace(",", "", regex=False
                ).str.replace("$", "", regex=False)
            before_non_null = int(prepared.notna().sum())
            numeric = pd.to_numeric(prepared, errors=errors)
            after_non_null = int(numeric.notna().sum())
            lost = max(0, before_non_null - after_non_null)
            if lost:
                self._record_issue(
                    issue_type="numeric_coercion_loss",
                    column=col,
                    count=lost,
                    details=f"errors={errors}",
                )
            coerced[col] = numeric

        return self._spawn(coerced, f"coerce_numeric({cols})")

    def fill_missing(self, **defaults):
        """Fill missing values by column defaults."""
        filled = self._df.copy()
        for col, value in defaults.items():
            missing = int(filled[col].isna().sum())
            if missing:
                self._record_issue(
                    issue_type="filled_missing",
                    column=col,
                    count=missing,
                    details=f"fill={value!r}",
                )
                filled[col] = filled[col].fillna(value)
        return self._spawn(filled, f"fill_missing({list(defaults.keys())})")

    def expect_domain(self, column, allowed, *, on_fail="raise", unknown_value="unknown"):
        """Validate or coerce values outside an allowed domain.

        on_fail:
          - "raise": raise error on out-of-domain values
          - "report": record issue but keep values
          - "coerce": map out-of-domain values to unknown_value and record issue
        """
        allowed_set = set(allowed)
        checked = self._df.copy()
        mask = checked[column].notna() & ~checked[column].isin(allowed_set)
        out_count = int(mask.sum())
        if out_count == 0:
            return self._spawn(checked, f"expect_domain({column})")

        sample = checked.loc[mask, column].astype(str).drop_duplicates().head(5).tolist()
        details = f"allowed={sorted(allowed_set)}, sample={sample}"
        self._record_issue(
            issue_type="domain_violation",
            column=column,
            count=out_count,
            details=details,
        )

        mode = on_fail.lower()
        if mode == "raise":
            raise ValueError(
                f"Domain violation in column '{column}': {out_count} out-of-domain values"
            )
        if mode == "coerce":
            checked.loc[mask, column] = unknown_value
        elif mode != "report":
            raise ValueError(f"Unsupported on_fail mode: {on_fail}")

        return self._spawn(checked, f"expect_domain({column}, on_fail={on_fail})")

    def assert_unique(self, columns):
        """Fail if duplicate rows exist under the given key columns."""
        cols = _as_column_list(columns)
        dupes = self._df.duplicated(subset=cols, keep=False)
        if dupes.any():
            raise ValueError(
                f"Uniqueness assertion failed for key columns {cols}: found duplicates"
            )
        return self

    def join(
        self,
        other,
        *,
        on=None,
        left_on=None,
        right_on=None,
        how="inner",
        cardinality=None,
        suffixes=("_x", "_y"),
    ):
        """Join two analyst relations with optional cardinality checks.

        cardinality can be one of: one_to_one, one_to_many, many_to_one.
        """
        if not isinstance(other, AnalystFrame):
            raise TypeError("join(...) expects another AnalystFrame")

        if cardinality is not None:
            c = cardinality.lower()
            if on is not None:
                left_keys = _as_column_list(on)
                right_keys = _as_column_list(on)
            else:
                if left_on is None or right_on is None:
                    raise ValueError(
                        "join(..., cardinality=...) requires join keys. "
                        "Provide either on=... or both left_on=... and right_on=..."
                    )
                left_keys = _as_column_list(left_on)
                right_keys = _as_column_list(right_on)

            if c in {"one_to_one", "1:1"}:
                self.assert_unique(left_keys)
                other.assert_unique(right_keys)
            elif c in {"many_to_one", "m:1"}:
                other.assert_unique(right_keys)
            elif c in {"one_to_many", "1:m"}:
                self.assert_unique(left_keys)
            else:
                raise ValueError(f"Unsupported cardinality mode: {cardinality}")

        joined = self._df.merge(
            other._df,
            on=on,
            left_on=left_on,
            right_on=right_on,
            how=how,
            suffixes=suffixes,
        )
        step = (
            f"join(how={how}, on={on}, left_on={left_on}, right_on={right_on}, "
            f"cardinality={cardinality})"
        )
        result = AnalystFrame(
            joined,
            plan=self._plan + other._plan + [step],
            name=self._name,
            issues=self._issues + other._issues,
        )
        result._baseline_rows = len(self._df)
        return result

    def smart_join(self, other, *, suffixes=("_x", "_y")):
        """Automatically infer join keys and perform normalized matching.

        No key arguments are required; keys are inferred from shared columns.

        Optional trust guidance will be introduced in this surface to bias
        matching decisions for specific columns/ranges when the user provides
        confidence hints. Without explicit hints, smart_join runs best-effort
        inference from observed overlap and normalization evidence.

        As a default posture, more data generally improves match quality:
        even rows that are not preserved directly in downstream outputs can
        still improve inferred key quality through scaffold statistics.

        Trusted-target semantics (planned):
        A trusted table expresses a structural prior about what records
        *should* exist.  For example, if the analyst knows that data must
        contain exactly one record per day per region, that skeleton can be
        supplied as the right-hand side of a smart_join.  The join then:
        - identifies *gaps*: rows present in the trusted target but absent
          in the source (missing records / nulls after the left-join);
        - surfaces *duplicates*: source rows that match more than once;
        - bootstraps error estimates from the known structural prior rather
          than purely from observed data statistics.
        This connects to the broader error-propagation model: a trusted
        target acts as a ground-truth skeleton that anchors quality labels
        and constrains the uncertainty interval on downstream aggregates.

        Adds match diagnostics columns:
          - _dedekind_match_type: exact | normalized | unmatched
          - _dedekind_match_score: 1.0 | 0.85 | 0.0
          - _dedekind_join_key: inferred key(s) used

        FIXME #253: delegate key inference and match ladder to the core
            smart join kernel once implemented in relational.cppm.
        FIXME #252: replace the _dedekind_match_type string column with a
            proper match witness type carrying ambiguity and confidence.
        FIXME #254: expose the join trace as a structured audit report rather
            than ad-hoc metadata columns so provenance is serializable.
        """
        if not isinstance(other, AnalystFrame):
            raise TypeError("smart_join(...) expects another AnalystFrame")

        keys = _infer_join_keys(self._df, other._df)

        left_work = self._df.copy()
        right_work = other._df.copy()

        left_on = []
        right_on = []
        left_raw_key_cols = []
        right_raw_key_cols = []
        for key in keys:
            ltmp = f"__dedekind_left_key_{key}"
            lraw = f"__dedekind_left_raw_{key}"
            rtmp = f"__dedekind_right_key_{key}"
            rraw = f"__dedekind_right_raw_{key}"

            left_work[ltmp] = _normalize_join_value(left_work[key])
            left_work[lraw] = left_work[key]
            right_work[rtmp] = _normalize_join_value(right_work[key])
            right_work[rraw] = right_work[key]

            left_on.append(ltmp)
            right_on.append(rtmp)
            left_raw_key_cols.append(lraw)
            right_raw_key_cols.append(rraw)

        # Preserve a single canonical copy of inferred join keys so chained
        # smart_join calls can continue to infer the same keys cleanly.
        right_payload = right_work.drop(columns=keys, errors="ignore")

        joined = left_work.merge(
            right_payload,
            how="left",
            left_on=left_on,
            right_on=right_on,
            suffixes=suffixes,
            indicator=True,
        )

        exact_mask = joined["_merge"].eq("both")
        for left_raw_col, right_raw_col in zip(left_raw_key_cols, right_raw_key_cols):
            left_raw = joined[left_raw_col].astype("string").fillna("")
            right_raw = joined[right_raw_col].astype("string").fillna("")
            exact_mask = exact_mask & left_raw.eq(right_raw)

        joined["_dedekind_match_type"] = "unmatched"
        joined.loc[joined["_merge"].eq("both") & ~exact_mask, "_dedekind_match_type"] = "normalized"
        joined.loc[exact_mask, "_dedekind_match_type"] = "exact"

        joined["_dedekind_match_score"] = 0.0
        joined.loc[joined["_dedekind_match_type"].eq("normalized"), "_dedekind_match_score"] = 0.85
        joined.loc[joined["_dedekind_match_type"].eq("exact"), "_dedekind_match_score"] = 1.0
        joined["_dedekind_join_key"] = ",".join(keys)

        drop_cols = left_on + right_on + left_raw_key_cols + ["_merge"] + right_raw_key_cols
        joined = joined.drop(columns=drop_cols, errors="ignore")

        step = f"smart_join(inferred_keys={keys})"
        result = AnalystFrame(
            joined,
            plan=self._plan + other._plan + [step],
            name=self._name,
            issues=self._issues + other._issues,
        )
        result._baseline_rows = len(self._df)
        return result

    def expect_no_multiplicity_increase(self, *, baseline_rows=None):
        """Fail if row count grows unexpectedly (join blow-up guard)."""
        baseline = baseline_rows
        if baseline is None:
            baseline = getattr(self, "_baseline_rows", None)
        if baseline is None:
            raise ValueError("No baseline row count available; pass baseline_rows=...")
        if len(self._df) > baseline:
            raise ValueError(
                f"Multiplicity increased: baseline={baseline}, current={len(self._df)}"
            )
        return self

    def group_by(self, keys):
        """Group rows by key columns for summarize(...).

        FIXME #251: replace with the core projection/aggregation operator
            once grouped aggregation lands in relational.cppm.
        """
        return AnalystGrouped(self, keys)

    def order_by(self, columns, ascending=True):
        cols = _as_column_list(columns)
        ordered = self._df.sort_values(cols, ascending=ascending)
        return self._spawn(ordered, f"order_by({cols})")

    def pivot(self, *, index, columns, values, aggfunc="sum", fill_value=None):
        pivoted = pivot_table(
            self._df,
            index=index,
            columns=columns,
            values=values,
            aggfunc=aggfunc,
            fill_value=fill_value,
        )
        step = (
            f"pivot(index={index}, columns={columns}, values={values}, aggfunc={aggfunc})"
        )
        return self._spawn(pivoted, step)

    def smart_pivot(
        self,
        *,
        index=None,
        columns=None,
        values=None,
        aggfunc="sum",
        fill_value=0,
        max_columns=12,
        min_coverage=0.9,
        other_label="__other__",
        sort=True,
    ):
        """Pivot with inferred axes and optional category compression.

        The method attempts to infer `index`, `columns`, and `values` when omitted,
        then compresses high-cardinality column values into an `other_label` bucket
        to keep the output matrix compact while preserving most signal.

        Optional user-interest hints are planned for this surface so analysts can
        prioritize specific dimensions/measures. Without explicit guidance,
        smart_pivot falls back to sensible defaults based on observed schema and
        distributional structure.
        """
        _require_pandas()
        if max_columns is not None and max_columns < 2:
            raise ValueError("smart_pivot requires max_columns >= 2 when set")
        if not (0 < float(min_coverage) <= 1):
            raise ValueError("smart_pivot requires 0 < min_coverage <= 1")

        resolved_index, resolved_columns, resolved_values = _infer_smart_pivot_axes(
            self._df,
            index=index,
            columns=columns,
            values=values,
        )

        compressed_df, compression = _compress_pivot_categories(
            self._df,
            columns=resolved_columns,
            values=resolved_values,
            max_columns=max_columns,
            min_coverage=float(min_coverage),
            other_label=other_label,
        )

        pivoted = pivot_table(
            compressed_df,
            index=resolved_index,
            columns=resolved_columns,
            values=resolved_values,
            aggfunc=aggfunc,
            fill_value=fill_value,
            sort=sort,
        )

        output_rows = int(len(pivoted))
        output_columns = max(0, int(len(pivoted.columns) - 1))
        dense_cells = output_rows * output_columns
        input_rows = int(len(self._df))
        cell_compression_ratio = float(input_rows) / float(max(1, dense_cells))

        step = (
            f"smart_pivot(index={resolved_index}, columns={resolved_columns}, "
            f"values={resolved_values}, compressed={compression['compressed']})"
        )
        result = self._spawn(pivoted, step)
        result._smart_pivot_diagnostics = {
            "index": resolved_index,
            "columns": resolved_columns,
            "values": resolved_values,
            "input_rows": input_rows,
            "output_rows": output_rows,
            "output_columns": output_columns,
            "dense_cells": dense_cells,
            "cell_compression_ratio": cell_compression_ratio,
            **compression,
        }
        return result

    def unpivot(
        self,
        *,
        id_vars,
        value_vars=None,
        var_name="variable",
        value_name="value",
    ):
        melted = unpivot_table(
            self._df,
            id_vars=id_vars,
            value_vars=value_vars,
            var_name=var_name,
            value_name=value_name,
        )
        step = (
            f"unpivot(id_vars={id_vars}, value_vars={value_vars}, "
            f"var_name={var_name}, value_name={value_name})"
        )
        return self._spawn(melted, step)

    def expect_row_count(self, *, exact=None, min_rows=None, max_rows=None):
        rows = len(self._df)
        if exact is not None and rows != exact:
            raise ValueError(f"Row count mismatch: expected exactly {exact}, got {rows}")
        if min_rows is not None and rows < min_rows:
            raise ValueError(f"Row count mismatch: expected >= {min_rows}, got {rows}")
        if max_rows is not None and rows > max_rows:
            raise ValueError(f"Row count mismatch: expected <= {max_rows}, got {rows}")
        return self

    def to_set(self, column):
        return SetDef.from_dataframe(self._df, column=column)

    def quality_report(self):
        """Return a DataFrame of tracked quality interventions and violations.

        FIXME #254: promote to a first-class join audit report that includes
            provenance chains across all pipeline steps, not just issues.
        """
        _require_pandas()
        if not self._issues:
            return pd.DataFrame(
                columns=["issue_type", "column", "count", "step", "relation", "details"]
            )
        return pd.DataFrame(self._issues)

    def explain(self):
        if not self._plan:
            return "(empty plan)"
        lines = [f"{i}. {step}" for i, step in enumerate(self._plan, start=1)]
        return "\n".join(lines)

    def realize(self):
        return self._df.copy()

    def __repr__(self):
        name = self._name or "<anonymous>"
        return f"AnalystFrame(name={name!r}, rows={len(self._df)}, steps={len(self._plan)})"


def table(name_or_df, df=None):
    """Create an analyst relation from a DataFrame.

    Supported forms:
      - table("sales", df)
      - table(df)
    """
    _require_pandas()
    if df is None:
        name = None
        frame = name_or_df
    else:
        name = str(name_or_df)
        frame = df
    return AnalystFrame(frame, plan=[f"table(name={name!r})"], name=name)


def smart_join(left, right):
    """Top-level convenience wrapper around AnalystFrame.smart_join(...)."""
    _require_pandas()
    left_frame = left if isinstance(left, AnalystFrame) else table(left)
    right_frame = right if isinstance(right, AnalystFrame) else table(right)
    return left_frame.smart_join(right_frame)


def smart_pivot(
    df,
    *,
    index=None,
    columns=None,
    values=None,
    aggfunc="sum",
    fill_value=0,
    max_columns=12,
    min_coverage=0.9,
    other_label="__other__",
    sort=True,
):
    """Top-level convenience wrapper around AnalystFrame.smart_pivot(...)."""
    _require_pandas()
    frame = df if isinstance(df, AnalystFrame) else table(df)
    return frame.smart_pivot(
        index=index,
        columns=columns,
        values=values,
        aggfunc=aggfunc,
        fill_value=fill_value,
        max_columns=max_columns,
        min_coverage=min_coverage,
        other_label=other_label,
        sort=sort,
    )


def pivot_table(
    df,
    *,
    index,
    columns,
    values,
    aggfunc="sum",
    fill_value=None,
    sort=True,
):
    """Middle-layer pivot shim backed by pandas.pivot_table.

    This keeps analyst notebooks focused on workflow while pivot support is
    tracked as a core relational feature in issue #170.
    """
    _require_pandas()
    return df.pivot_table(
        index=index,
        columns=columns,
        values=values,
        aggfunc=aggfunc,
        fill_value=fill_value,
        sort=sort,
    ).reset_index()


def unpivot_table(
    df,
    *,
    id_vars,
    value_vars=None,
    var_name="variable",
    value_name="value",
):
    """Middle-layer unpivot shim backed by pandas.melt."""
    _require_pandas()
    return df.melt(
        id_vars=id_vars,
        value_vars=value_vars,
        var_name=var_name,
        value_name=value_name,
    )


def _coerce_frame(frame):
    return frame if isinstance(frame, AnalystFrame) else table(frame)


def monthly_category_report(
    frame,
    *,
    date_column="date",
    period_column="month",
    category_column="category",
    measures=None,
    freq="M",
):
    """Build a compact period x category summary from a relation.

    FIXME #250: route through first-class panel metadata once available.
    FIXME #251: delegate grouped aggregation to the core relational layer.
    """
    base = _coerce_frame(frame)
    resolved_measures = measures or {
        "revenue": ("revenue", "sum"),
        "units": ("units", "sum"),
    }
    return (
        base
        .derive_period(date_column, target_column=period_column, freq=freq)
        .group_by([period_column, category_column])
        .summarize(**resolved_measures)
        .order_by([period_column, category_column])
    )


def repair_missing_by_group_ratio(
    frame,
    *,
    value_column,
    basis_column,
    group_column,
    target_column=None,
    min_basis=0,
    fill_value=0,
):
    """Impute missing values using per-group value/basis ratios.

    FIXME #253: replace this heuristic with a principled probabilistic repair
        layer once the smart-join kernel grows richer match evidence.
    """
    base = _coerce_frame(frame)
    repaired = base._df.copy()
    resolved_target = target_column or value_column

    valid = repaired[
        (repaired[basis_column] > min_basis) & repaired[resolved_target].notna()
    ].copy()
    ratio_by_group = valid.groupby(group_column).apply(
        lambda group: group[resolved_target].sum() / group[basis_column].sum()
        if group[basis_column].sum()
        else 0.0
    ).to_dict()

    total_basis = float(valid[basis_column].sum())
    global_ratio = float(valid[resolved_target].sum()) / total_basis if total_basis else 0.0

    repaired[resolved_target] = repaired[resolved_target].fillna(
        repaired[basis_column] * repaired[group_column].map(ratio_by_group).fillna(global_ratio)
    )
    repaired[resolved_target] = repaired[resolved_target].fillna(fill_value)

    result = base._spawn(
        repaired,
        (
            f"repair_missing_by_group_ratio(value={value_column}, basis={basis_column}, "
            f"group={group_column})"
        ),
    )
    result._repair_diagnostics = {
        "strategy": "group_ratio",
        "value_column": resolved_target,
        "basis_column": basis_column,
        "group_column": group_column,
        "groups": len(ratio_by_group),
        "global_ratio": global_ratio,
    }
    return result


def repair_missing_by_product(
    frame,
    *,
    value_column,
    factor_columns,
    target_column=None,
    fill_value=0,
):
    """Impute missing values as the product of trusted factors.

    FIXME #231: expose this as one constraint family inside a broader
        optimization/reconciliation toolkit rather than a standalone heuristic.
    """
    base = _coerce_frame(frame)
    repaired = base._df.copy()
    resolved_target = target_column or value_column

    product_value = 1
    for factor in factor_columns:
        product_value = product_value * repaired[factor]

    repaired[resolved_target] = repaired[resolved_target].fillna(product_value)
    repaired[resolved_target] = repaired[resolved_target].fillna(fill_value)

    result = base._spawn(
        repaired,
        f"repair_missing_by_product(value={value_column}, factors={list(factor_columns)})",
    )
    result._repair_diagnostics = {
        "strategy": "product",
        "value_column": resolved_target,
        "factor_columns": list(factor_columns),
    }
    return result


def align_pivot_table(df, *, index_column="month", value_columns=None, fill_value=0.0):
    """Align a pivot-like DataFrame to a stable ordered set of value columns.

    FIXME #170: replace manual alignment with a first-class relational wide-table
        witness once pivot/crosstab support matures beyond the MVP shim.
    """
    _require_pandas()
    resolved_columns = list(value_columns or [col for col in df.columns if col != index_column])
    aligned = df.copy()
    for column in resolved_columns:
        if column not in aligned.columns:
            aligned[column] = fill_value
    return aligned[[index_column, *resolved_columns]].sort_values(index_column).reset_index(drop=True)


def pivot_mean_absolute_error(
    candidate,
    reference,
    *,
    index_column="month",
    value_columns=None,
):
    """Compute MAE between two pivot-like DataFrames after alignment."""
    candidate_aligned = align_pivot_table(
        candidate,
        index_column=index_column,
        value_columns=value_columns,
    ).set_index(index_column)
    reference_aligned = align_pivot_table(
        reference,
        index_column=index_column,
        value_columns=value_columns,
    ).set_index(index_column)
    return float((candidate_aligned - reference_aligned).abs().stack().mean())


def pivot_quality_report(reference, *, index_column="month", value_columns=None, **candidates):
    """Compare multiple candidate pivots against a reference pivot."""
    _require_pandas()
    rows = []
    for stage, candidate in candidates.items():
        rows.append(
            {
                "stage": stage,
                "pivot_mae": pivot_mean_absolute_error(
                    candidate,
                    reference,
                    index_column=index_column,
                    value_columns=value_columns,
                ),
            }
        )
    return pd.DataFrame(rows)


def _quality_issue_count(frame):
    """Return the total tracked quality issue count for an AnalystFrame."""
    issues = frame.quality_report()
    if issues.empty:
        return 0
    return int(issues["count"].sum())


def table_quality_metrics(
    frame,
    *,
    label,
    key_columns=None,
    numeric_columns=None,
):
    """Compute quality metrics and lightweight certifications for one table."""
    _require_pandas()
    base = _coerce_frame(frame)
    data = base._df

    resolved_keys = [col for col in _as_column_list(key_columns or []) if col in data.columns]
    resolved_numeric = [col for col in _as_column_list(numeric_columns or []) if col in data.columns]

    row_count = int(len(data))
    missing_cells = int(data.isna().sum().sum())
    duplicate_rows = int(data.duplicated(subset=resolved_keys, keep=False).sum()) if resolved_keys else 0

    numeric_parse_losses = 0
    for column in resolved_numeric:
        before_non_null = int(data[column].notna().sum())
        after_non_null = int(pd.to_numeric(data[column], errors="coerce").notna().sum())
        numeric_parse_losses += max(0, before_non_null - after_non_null)

    return pd.DataFrame(
        [
            {
                "table": str(label),
                "rows": row_count,
                "missing_cells": missing_cells,
                "duplicate_rows": duplicate_rows,
                "numeric_parse_losses": int(numeric_parse_losses),
                "cert_no_duplicates": bool(duplicate_rows == 0),
                "cert_numeric_parse_clean": bool(numeric_parse_losses == 0),
            }
        ]
    )


def analyst_sales_quality_lift_report(
    sales,
    products,
    regions,
    *,
    reference_pivot,
    product_price_hq=None,
    date_column="date",
    product_key="product_id",
    region_key="region",
    category_column="category",
    units_column="units",
    revenue_column="revenue",
):
    """Build a compact analyst-tier quality-lift report for sales-style tables.

    The report intentionally keeps notebook code thin by returning consumable
    data products (tables suitable for display/charting).
    """
    _require_pandas()

    sales_frame = _coerce_frame(sales)
    products_frame = _coerce_frame(products)
    regions_frame = _coerce_frame(regions)

    if isinstance(reference_pivot, AnalystFrame):
        reference_df = reference_pivot.realize()
    else:
        reference_df = reference_pivot.copy()

    input_quality = pd.concat(
        [
            table_quality_metrics(
                sales_frame,
                label="table_a_sales",
                key_columns=[date_column, product_key, region_key],
                numeric_columns=[units_column, revenue_column],
            ),
            table_quality_metrics(
                products_frame,
                label="table_b_products",
                key_columns=[product_key],
            ),
            table_quality_metrics(
                regions_frame,
                label="table_regions",
                key_columns=[region_key],
            ),
        ],
        ignore_index=True,
    )

    vanilla_plan = (
        sales_frame
        .join(products_frame, on=product_key, how="left")
        .join(regions_frame, on=region_key, how="left")
        .derive(revenue_num=lambda table_df: pd.to_numeric(table_df[revenue_column], errors="coerce"))
        .fill_missing(revenue_num=0)
    )
    vanilla_pivot = monthly_category_report(
        vanilla_plan,
        date_column=date_column,
        category_column=category_column,
        measures={"revenue": ("revenue_num", "sum")},
    ).pivot(
        index="month",
        columns=category_column,
        values="revenue",
        aggfunc="sum",
        fill_value=0,
    ).order_by("month").realize()

    regions_clean = regions_frame.normalize_text([region_key], lower=True, strip=True)
    region_domain = sorted(set(regions_clean.realize()[region_key].dropna().astype(str)))

    sales_clean = (
        sales_frame
        .normalize_text([product_key, region_key], lower=True, strip=True)
        .coerce_numeric([units_column, revenue_column], strip_symbols=True)
        .fill_missing(**{units_column: 0})
        .expect_domain(region_key, region_domain, on_fail="report")
    )
    products_clean = products_frame.normalize_text([product_key, category_column], lower=True, strip=True)

    smart_joined = smart_join(sales_clean, products_clean).smart_join(regions_clean)
    smart_plan = smart_joined
    if product_price_hq is not None:
        hq_frame = _coerce_frame(product_price_hq)
        smart_plan = repair_missing_by_product(
            smart_join(smart_plan, hq_frame),
            value_column=revenue_column,
            factor_columns=[units_column, "unit_price_hq"],
        )

    smart_pivot_df = monthly_category_report(
        smart_plan,
        date_column=date_column,
        category_column=category_column,
        measures={"revenue": (revenue_column, "sum")},
    ).pivot(
        index="month",
        columns=category_column,
        values="revenue",
        aggfunc="sum",
        fill_value=0,
    ).order_by("month").realize()

    value_columns = sorted(set([column for column in reference_df.columns if column != "month"]))
    vanilla_check = align_pivot_table(
        vanilla_pivot,
        index_column="month",
        value_columns=value_columns,
    )
    smart_check = align_pivot_table(
        smart_pivot_df,
        index_column="month",
        value_columns=value_columns,
    )

    error_report = pivot_quality_report(
        reference_df,
        value_columns=value_columns,
        vanilla=vanilla_check,
        smart=smart_check,
    )

    vanilla_mae = float(error_report.loc[error_report["stage"] == "vanilla", "pivot_mae"].iloc[0])
    smart_mae = float(error_report.loc[error_report["stage"] == "smart", "pivot_mae"].iloc[0])
    quality_delta = pd.DataFrame(
        [
            {
                "metric": "pivot_mae",
                "vanilla": vanilla_mae,
                "smart": smart_mae,
                "improvement": vanilla_mae - smart_mae,
            },
            {
                "metric": "quality_issue_count",
                "vanilla": float(_quality_issue_count(vanilla_plan)),
                "smart": float(_quality_issue_count(sales_clean)),
                "improvement": float(_quality_issue_count(vanilla_plan) - _quality_issue_count(sales_clean)),
            },
        ]
    )

    quality_labels = pd.DataFrame(
        [
            {
                "stage": "vanilla",
                "quality_issue_count": _quality_issue_count(vanilla_plan),
                "estimated_pivot_error_mae": vanilla_mae,
            },
            {
                "stage": "smart",
                "quality_issue_count": _quality_issue_count(sales_clean),
                "estimated_pivot_error_mae": smart_mae,
            },
        ]
    )

    return {
        "input_quality": input_quality,
        "vanilla_pivot": vanilla_check,
        "smart_pivot": smart_check,
        "error_report": error_report,
        "quality_labels": quality_labels,
        "quality_delta": quality_delta,
        "sales_quality_issues": sales_clean.quality_report(),
        "region_values": regions_clean.to_set(region_key).realize(),
    }


@dataclass(frozen=True)
class Dual:
    """Minimal dual number for first-order automatic differentiation.

    FIXME #177: promote scalar derivative objects into the denotational linear-map
        API rather than keeping them as a local numeric convenience.
    FIXME #272: unify this with the broader dual-number roadmap once the AD epic
        lands in the core ontology.
    """

    real: float
    dual: float = 0.0

    def __add__(self, other):
        other = other if isinstance(other, Dual) else Dual(float(other), 0.0)
        return Dual(self.real + other.real, self.dual + other.dual)

    def __radd__(self, other):
        return self.__add__(other)

    def __sub__(self, other):
        other = other if isinstance(other, Dual) else Dual(float(other), 0.0)
        return Dual(self.real - other.real, self.dual - other.dual)

    def __rsub__(self, other):
        other = other if isinstance(other, Dual) else Dual(float(other), 0.0)
        return other.__sub__(self)

    def __mul__(self, other):
        other = other if isinstance(other, Dual) else Dual(float(other), 0.0)
        return Dual(
            self.real * other.real,
            self.real * other.dual + self.dual * other.real,
        )

    def __rmul__(self, other):
        return self.__mul__(other)

    def __truediv__(self, other):
        other = other if isinstance(other, Dual) else Dual(float(other), 0.0)
        denominator = other.real * other.real
        return Dual(
            self.real / other.real,
            (self.dual * other.real - self.real * other.dual) / denominator,
        )

    def __rtruediv__(self, other):
        other = other if isinstance(other, Dual) else Dual(float(other), 0.0)
        return other.__truediv__(self)

    def __neg__(self):
        return Dual(-self.real, -self.dual)

    def __pow__(self, power):
        if isinstance(power, Dual):
            raise TypeError("Dual exponentiation only supports scalar exponents in this MVP")
        exponent = float(power)
        return Dual(
            self.real ** exponent,
            exponent * (self.real ** (exponent - 1.0)) * self.dual,
        )


def dual_derivative(function, x):
    """Evaluate a scalar function and its derivative at x using Dual numbers."""
    result = function(Dual(float(x), 1.0))
    if not isinstance(result, Dual):
        return float(result), 0.0
    return result.real, result.dual


@dataclass(frozen=True)
class LinearChoice:
    """Small mixed-integer choice primitive for constrained planning."""

    name: str
    value: float
    cost: float
    max_units: int = 1
    min_units: int = 0


def solve_mixed_integer_plan(choices, *, budget):
    """Solve a tiny mixed-integer linear selection problem by exhaustive search.

    This is intentionally small-scale and notebook-friendly.

    FIXME #231: replace exhaustive enumeration with a proper constrained
        optimization toolkit (Lagrange multipliers / LP-MIP bridge).
    """
    _require_pandas()
    resolved = [choice if isinstance(choice, LinearChoice) else LinearChoice(**choice) for choice in choices]
    best_decision = None
    best_value = float("-inf")

    ranges = [range(choice.min_units, choice.max_units + 1) for choice in resolved]
    for quantities in product(*ranges):
        total_cost = sum(choice.cost * quantity for choice, quantity in zip(resolved, quantities))
        if total_cost > budget:
            continue
        total_value = sum(choice.value * quantity for choice, quantity in zip(resolved, quantities))
        if total_value > best_value:
            best_value = total_value
            best_decision = quantities

    if best_decision is None:
        raise ValueError("No feasible mixed-integer plan satisfies the budget")

    decision_rows = []
    total_cost = 0.0
    total_value = 0.0
    for choice, quantity in zip(resolved, best_decision):
        cost = float(choice.cost * quantity)
        value = float(choice.value * quantity)
        total_cost += cost
        total_value += value
        decision_rows.append(
            {
                "name": choice.name,
                "quantity": int(quantity),
                "unit_cost": float(choice.cost),
                "unit_value": float(choice.value),
                "cost": cost,
                "value": value,
            }
        )

    return {
        "objective": total_value,
        "budget": float(budget),
        "cost": total_cost,
        "decision_table": pd.DataFrame(decision_rows),
    }


@dataclass(frozen=True)
class Activity:
    """A small DAG activity for critical-path style planning."""

    name: str
    duration: float
    depends_on: tuple = ()


def critical_path_schedule(activities):
    """Compute earliest starts/finishes and a critical path for a DAG.

    FIXME #329: replace with a graph-theoretic core once max-flow/min-cut and
        related graph kernels are available in the ontology.
    FIXME #339: align with the graph-algorithms backlog once topological and
        shortest/longest path primitives become first-class.
    """
    _require_pandas()
    resolved = [activity if isinstance(activity, Activity) else Activity(**activity) for activity in activities]
    by_name = {activity.name: activity for activity in resolved}
    earliest_start = {}
    earliest_finish = {}
    predecessor = {}
    remaining = set(by_name)

    while remaining:
        progressed = False
        for name in list(remaining):
            activity = by_name[name]
            deps = tuple(activity.depends_on)
            if any(dep not in earliest_finish for dep in deps):
                continue
            if deps:
                best_dep = max(deps, key=lambda dep: earliest_finish[dep])
                start = earliest_finish[best_dep]
                predecessor[name] = best_dep
            else:
                best_dep = None
                start = 0.0
                predecessor[name] = best_dep
            earliest_start[name] = float(start)
            earliest_finish[name] = float(start + activity.duration)
            remaining.remove(name)
            progressed = True
        if not progressed:
            raise ValueError("critical_path_schedule requires an acyclic dependency graph")

    terminal = max(earliest_finish, key=earliest_finish.get)
    critical_path = []
    cursor = terminal
    while cursor is not None:
        critical_path.append(cursor)
        cursor = predecessor.get(cursor)
    critical_path.reverse()

    rows = []
    for activity in resolved:
        rows.append(
            {
                "activity": activity.name,
                "duration": float(activity.duration),
                "earliest_start": earliest_start[activity.name],
                "earliest_finish": earliest_finish[activity.name],
                "is_critical": activity.name in critical_path,
            }
        )
    return {
        "project_duration": earliest_finish[terminal],
        "critical_path": critical_path,
        "schedule": pd.DataFrame(rows).sort_values(["earliest_start", "activity"]),
    }


class SetDef:
    """Analyst-tier set definition: intensional (symbolic) representation.

    Uses SQL/Python-like syntax familiar to users without mathematical training.
    Supports both extensional (concrete values) and intensional (predicate-based)
    representations.
    """

    def __init__(self, values=None, predicate=None):
        # Extensional: explicit values
        self._values = list(values) if values is not None else None
        # Intensional: symbolic predicate
        self._predicate = predicate

    @staticmethod
    def from_list(items):
        """Create an extensional (concrete) set from a list."""
        return SetDef(values=items)

    @staticmethod
    def from_range(start, stop=None, step=1):
        """Create a set from a numeric range."""
        if stop is None:
            stop = start
            start = 0
        return SetDef(values=list(range(start, stop, step)))

    @staticmethod
    def from_dataframe(df, column=None):
        """Create a set from a DataFrame column."""
        _require_pandas()
        if column is None:
            if len(df.columns) != 1:
                raise ValueError(
                    f"Expected exactly one column, got {len(df.columns)}; set column=..."
                )
            column = df.columns[0]
        return SetDef(values=df[column].tolist())

    def where(self, condition):
        """Filter: keep elements satisfying the condition.

        Returns an intensional set definition if starting from a predicate,
        or filtered values if starting from concrete values.
        """
        if self._values is not None:
            filtered = [v for v in self._values if condition(v)]
            return SetDef(values=filtered)
        return self

    def select(self, mapping):
        """Transform: apply a function to each element (map/image).

        This is the set-builder equivalent of 'select'.
        """
        if self._values is not None:
            mapped = [mapping(v) for v in self._values]
            return SetDef(values=mapped)
        return self

    def union(self, other):
        """Set union: A ∪ B (dedekind.sets)."""
        if self._values is not None and other._values is not None:
            result = set_union(set(self._values), set(other._values))
            return SetDef(values=list(result))
        return self

    def intersect(self, other):
        """Set intersection: A ∩ B (dedekind.sets)."""
        if self._values is not None and other._values is not None:
            result = set_intersection(set(self._values), set(other._values))
            return SetDef(values=list(result))
        return self

    def minus(self, other):
        """Set difference: A \\ B (dedekind.sets)."""
        if self._values is not None and other._values is not None:
            result = set_difference(set(self._values), set(other._values))
            return SetDef(values=list(result))
        return self

    def __len__(self):
        if self._values is not None:
            return set_cardinality(set(self._values))
        raise NotImplementedError("Cardinality of intensional SetDef is not yet supported")

    def realize(self, *, ordered=True):
        """Extensional (concrete) realization: evaluate to get actual values.

        This transitions from intensional (symbolic) to extensional (computed).
        Uses the dedekind facade functions for final normalization.
        """
        if self._values is not None:
            return _realize_extensional_values(self._values, ordered=ordered)
        raise NotImplementedError("Symbolic set realization not yet implemented")

    def to_dataframe(self, column_name="value"):
        """Render extensional set values as a single-column DataFrame."""
        _require_pandas()
        return pd.DataFrame({column_name: self._values or []})

    def __repr__(self):
        if self._values is not None:
            return f"SetDef({self._values})"
        return "SetDef(symbolic)"


class Ensemble:
    """Formal-tier set representation using mathematical terminology.

    'Ensemble' emphasizes that this is a formal mathematical object.
    Supports set-theoretic operations with mathematically precise names.
    """

    def __init__(self, members=None, predicate=None):
        # Extensional: explicit members
        self._members = list(members) if members is not None else None
        # Intensional: symbolic predicate
        self._predicate = predicate

    @staticmethod
    def from_members(*items):
        """Create extensional ensemble from members: {a, b, c, ...}."""
        return Ensemble(members=items)

    @staticmethod
    def comprehension(universe, predicate):
        """Create intensional ensemble via set-builder: {x ∈ U | P(x)}.

        Universe must be iterable (e.g. range, list).  The predicate is kept
        symbolically until realize() is called, at which point it is applied to
        every element of the universe.

        FIXME: once the core symbolic domain layer lands, universe should be a
        typed domain object (ℕ, ℤ, ℝ, …) rather than a plain iterable.
        """
        members = [x for x in universe if predicate(x)]
        return Ensemble(members=members)

    @staticmethod
    def from_dataframe(df, column=None):
        """Create an ensemble from a DataFrame column."""
        _require_pandas()
        if column is None:
            if len(df.columns) != 1:
                raise ValueError(
                    f"Expected exactly one column, got {len(df.columns)}; set column=..."
                )
            column = df.columns[0]
        return Ensemble(members=df[column].tolist())

    def restrict(self, predicate):
        """Restrict to elements satisfying a property: {x ∈ S | Q(x)}.

        Corresponds to analyst 'where' but with mathematical naming.
        """
        if self._members is not None:
            restricted = [v for v in self._members if predicate(v)]
            return Ensemble(members=restricted)
        return self

    def map_to(self, morphism):
        """Apply morphism (function) to each element: f(S) = {f(x) | x ∈ S}."""
        if self._members is not None:
            image = [morphism(v) for v in self._members]
            return Ensemble(members=image)
        return self

    def union(self, other):
        """Ensemble union: A ∪ B (dedekind.sets)."""
        if self._members is not None and other._members is not None:
            result = set_union(set(self._members), set(other._members))
            return Ensemble(members=list(result))
        return self

    def intersection(self, other):
        """Ensemble intersection: A ∩ B (dedekind.sets)."""
        if self._members is not None and other._members is not None:
            result = set_intersection(set(self._members), set(other._members))
            return Ensemble(members=list(result))
        return self

    def difference(self, other):
        """Ensemble difference (relative complement): A \\ B (dedekind.sets)."""
        if self._members is not None and other._members is not None:
            result = set_difference(set(self._members), set(other._members))
            return Ensemble(members=list(result))
        return self

    # --- Python operator aliases (issue #241: operator/method parity) ---

    def __or__(self, other):
        """A | B  ≡  A.union(B)"""
        return self.union(other)

    def __and__(self, other):
        """A & B  ≡  A.intersection(B)"""
        return self.intersection(other)

    def __sub__(self, other):
        """A - B  ≡  A.difference(B)"""
        return self.difference(other)

    def __le__(self, other):
        """A <= B  ≡  A.is_subset_of(B)"""
        if self._members is not None and other._members is not None:
            return set(self._members) <= set(other._members)
        return NotImplemented

    def __len__(self):
        if self._members is not None:
            return set_cardinality(set(self._members))
        raise NotImplementedError("Cardinality of intensional Ensemble is not yet supported")

    def realize(self, *, ordered=True):
        """Realize intensional definition to extensional members.

        In formal terms: evaluate the comprehension {x | P(x)} to get {x₁, x₂, ...}.
        Uses the dedekind facade functions for final normalization.
        """
        if self._members is not None:
            return _realize_extensional_values(self._members, ordered=ordered)
        raise NotImplementedError("Symbolic ensemble realization not yet implemented")

    def to_dataframe(self, column_name="element"):
        """Render extensional ensemble members as a single-column DataFrame."""
        _require_pandas()
        return pd.DataFrame({column_name: self._members or []})

    def __repr__(self):
        if self._members is not None:
            return f"{{{', '.join(map(str, self._members))}}}"
        return "Ensemble(intensional)"


__all__ = [
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
