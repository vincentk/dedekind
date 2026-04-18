"""Analyst and formal DSL shim classes for set operations.

This module provides prototype DSL interfaces demonstrating the intended
ergonomics for end-user set manipulation. These are executable shims for
UX validation (issue #241); output correctness refinement is a follow-up.
"""

from . import ordered_set_roundtrip, unordered_set_roundtrip


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
        """Set difference: A \ B."""
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
        """Ensemble difference (relative complement): A \ B."""
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

    def __repr__(self):
        if self._members is not None:
            return f"{{{', '.join(map(str, self._members))}}}"
        return "Ensemble(intensional)"


__all__ = ["SetDef", "Ensemble"]
