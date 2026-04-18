"""Analyst and formal DSL shim classes for set operations.

This module provides prototype DSL interfaces demonstrating the intended
ergonomics for end-user set manipulation. These are executable shims for
UX validation (issue #241); output correctness refinement is a follow-up.
"""

from . import ordered_set_roundtrip, unordered_set_roundtrip

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
        right_raw_key_cols = []
        for key in keys:
            ltmp = f"__dedekind_left_key_{key}"
            rtmp = f"__dedekind_right_key_{key}"
            rraw = f"__dedekind_right_raw_{key}"

            left_work[ltmp] = _normalize_join_value(left_work[key])
            right_work[rtmp] = _normalize_join_value(right_work[key])
            right_work[rraw] = right_work[key]

            left_on.append(ltmp)
            right_on.append(rtmp)
            right_raw_key_cols.append(rraw)

        joined = left_work.merge(
            right_work,
            how="left",
            left_on=left_on,
            right_on=right_on,
            suffixes=suffixes,
            indicator=True,
        )

        exact_mask = joined["_merge"].eq("both")
        for key, raw_col in zip(keys, right_raw_key_cols):
            left_raw = joined[key].astype("string").fillna("")
            right_raw = joined[raw_col].astype("string").fillna("")
            exact_mask = exact_mask & left_raw.eq(right_raw)

        joined["_dedekind_match_type"] = "unmatched"
        joined.loc[joined["_merge"].eq("both") & ~exact_mask, "_dedekind_match_type"] = "normalized"
        joined.loc[exact_mask, "_dedekind_match_type"] = "exact"

        joined["_dedekind_match_score"] = 0.0
        joined.loc[joined["_dedekind_match_type"].eq("normalized"), "_dedekind_match_score"] = 0.85
        joined.loc[joined["_dedekind_match_type"].eq("exact"), "_dedekind_match_score"] = 1.0
        joined["_dedekind_join_key"] = ",".join(keys)

        drop_cols = left_on + right_on + ["_merge"] + right_raw_key_cols
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
        """Set union: A ∪ B."""
        if self._values is not None and other._values is not None:
            result = list(set(self._values) | set(other._values))
            return SetDef(values=result)
        return self

    def intersect(self, other):
        """Set intersection: A ∩ B."""
        if self._values is not None and other._values is not None:
            result = list(set(self._values) & set(other._values))
            return SetDef(values=result)
        return self

    def minus(self, other):
        """Set difference: A \\ B."""
        if self._values is not None and other._values is not None:
            result = list(set(self._values) - set(other._values))
            return SetDef(values=result)
        return self

    def realize(self, *, ordered=True):
        """Extensional (concrete) realization: evaluate to get actual values.

        This transitions from intensional (symbolic) to extensional (computed).
        Uses the dedekind facade functions for final normalization.
        """
        if self._values is not None:
            if ordered:
                return ordered_set_roundtrip(self._values)
            else:
                return unordered_set_roundtrip(self._values)
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
        """Create intensional ensemble via set-builder: {x ∈ U | P(x)}."""
        return Ensemble(predicate=predicate)

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
        """Ensemble union: A ∪ B."""
        if self._members is not None and other._members is not None:
            result = list(set(self._members) | set(other._members))
            return Ensemble(members=result)
        return self

    def intersection(self, other):
        """Ensemble intersection: A ∩ B."""
        if self._members is not None and other._members is not None:
            result = list(set(self._members) & set(other._members))
            return Ensemble(members=result)
        return self

    def difference(self, other):
        """Ensemble difference (relative complement): A \\ B."""
        if self._members is not None and other._members is not None:
            result = list(set(self._members) - set(other._members))
            return Ensemble(members=result)
        return self

    def realize(self, *, ordered=True):
        """Realize intensional definition to extensional members.

        In formal terms: evaluate the comprehension {x | P(x)} to get {x₁, x₂, ...}.
        Uses the dedekind facade functions for final normalization.
        """
        if self._members is not None:
            if ordered:
                return ordered_set_roundtrip(self._members)
            else:
                return unordered_set_roundtrip(self._members)
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
    "table",
    "smart_join",
    "pivot_table",
    "unpivot_table",
]
