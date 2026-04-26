"""Python mirror of ``dedekind::numbers::booleans`` (and ``dedekind::algebra::boolean``).

Per #400 (carrier-type migration), the canonical Boolean species symbol
``𝔹`` names the @b carrier itself.  In Python the carrier is ``bool``;
this module exposes the symbol plus the symbolic-scout factory ``var``
so set-builder DSL expressions read the way they read in C++::

    from dedekind.numbers.boolean import 𝔹, B, var

    b = var(𝔹)
    universe = {x for x in (False, True)}
    truthy = {x for x in universe if (b == True)(x)}
    falsy  = {x for x in universe if (!b)(x)}        # via .not_() helper

The Python surface is intentionally minimal — just enough for an
external-API test that the C++ migration's "plain math" reading
survives across the language boundary.  Richer set-builder
ergonomics (``Set{b % B | (b == true)}`` parity, predicate-set
collapse, etc.) are tracked separately.
"""

# Carrier symbols.  ``𝔹`` is the canonical Unicode form; ``B`` is the
# ASCII alias for environments where Unicode input is inconvenient.
𝔹 = bool
B = bool


class Variable:
    """The symbolic scout (``id_S``) of a carrier type.

    Mirrors ``dedekind::sets::Variable<S>`` from
    ``src/main/modules/dedekind/sets/expressions.cppm``.  Holds the
    carrier domain and supports basic comparison / logical operators
    that build callable predicates.
    """

    __slots__ = ("domain",)

    def __init__(self, domain):
        self.domain = domain

    def __eq__(self, rhs):
        return lambda v: v == rhs

    def __ne__(self, rhs):
        return lambda v: v != rhs

    def __lt__(self, rhs):
        return lambda v: v < rhs

    def __le__(self, rhs):
        return lambda v: v <= rhs

    def __gt__(self, rhs):
        return lambda v: v > rhs

    def __ge__(self, rhs):
        return lambda v: v >= rhs

    def not_(self):
        """``!b`` ≡ ``b == False`` (the C++ side has this as ``operator!``)."""
        if self.domain is not bool:
            raise TypeError(
                f"Variable.not_() requires a bool domain; got {self.domain!r}"
            )
        return self == False  # noqa: E712 — predicate-builder, not a comparison

    # Python operator __invert__ (``~b``) maps to logical-not on a bool variable.
    def __invert__(self):
        return self.not_()

    def __repr__(self):
        return f"var({self.domain.__name__})"


def var(domain):
    """Symbolic-scout factory: ``var(𝔹)`` ranges over ``bool``.

    Mirrors ``dedekind::sets::var<S>`` (the
    ``inline constexpr Variable<S> var{}`` template variable).  In
    Python ``var`` is a function rather than a template variable, so
    the C++ ``var<𝔹>`` becomes ``var(𝔹)`` — the carrier is the
    runtime argument.
    """
    return Variable(domain)


__all__ = ["𝔹", "B", "Variable", "var"]
