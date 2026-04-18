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
        return AnalystFrame(grouped, plan=plan, name=self._frame._name)


class AnalystFrame:
    """Analyst-tier relation wrapper with declarative combinators.

    This is a pandas-backed shim that keeps notebook code focused on intent
    while core relational operators evolve in the C++ layer.
    """

    def __init__(self, df, *, plan=None, name=None):
        _require_pandas()
        self._df = df.copy()
        self._plan = list(plan or [])
        self._name = name

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

        return AnalystFrame(filtered, plan=self._plan + [step], name=self._name)

    def project(self, columns):
        cols = _as_column_list(columns)
        projected = self._df.loc[:, cols]
        return AnalystFrame(projected, plan=self._plan + [f"project({cols})"], name=self._name)

    def select(self, columns):
        """Alias for project(...) to match analyst intuition."""
        return self.project(columns)

    def derive(self, **new_columns):
        """Add derived columns from callables or constant values."""
        derived = self._df.copy()
        for name, expr in new_columns.items():
            derived[name] = expr(derived) if callable(expr) else expr
        return AnalystFrame(
            derived,
            plan=self._plan + [f"derive({list(new_columns.keys())})"],
            name=self._name,
        )

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
        result = AnalystFrame(joined, plan=self._plan + other._plan + [step], name=self._name)
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
        """Group rows by key columns for summarize(...)."""
        return AnalystGrouped(self, keys)

    def order_by(self, columns, ascending=True):
        cols = _as_column_list(columns)
        ordered = self._df.sort_values(cols, ascending=ascending)
        return AnalystFrame(ordered, plan=self._plan + [f"order_by({cols})"], name=self._name)

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
        return AnalystFrame(pivoted, plan=self._plan + [step], name=self._name)

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
        return AnalystFrame(melted, plan=self._plan + [step], name=self._name)

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
    "pivot_table",
    "unpivot_table",
]
