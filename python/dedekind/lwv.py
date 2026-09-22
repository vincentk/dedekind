"""Lawvere-set (𝐋𝐰𝐯) grammar surface: the README set-expression exhibit in
Python, in blackboard-math notation.

First cut for #886.  Two deliberate choices, both from the design pass:

* **Mathy symbols, not "pythonic".**  ``ℕ`` / ``χ`` / ``ext`` and the operators
  ``&`` / ``|`` / ``~`` / ``in`` / ``S(x)`` mirror the C++ grammar verbatim
  (Listing 3/4).  Python 3 admits the Unicode identifiers, so the same
  expression a reader sees in C++ is writable here.
* **A structured combinator, not a lambda.**  ``χ > 3`` builds a transparent
  predicate *node* (``_Cmp``), and ``&`` / ``|`` / ``~`` build ``_Meet`` /
  ``_Join`` / ``_Not`` nodes — an expression tree that mirrors the C++
  Meet/Join/Not reducer, rather than the opaque closures the legacy
  ``sets.Variable`` returns.  This is the shape that will later *delegate*
  evaluation/normalisation to the native C++ core (``_dedekind``) + the
  value-level normal form; the Python-side evaluation here is the placeholder.

``fix`` is intentionally absent: it is the compile-time NTTP artefact that
cannot cross the phase wall (a type cannot depend on a runtime value), so the
honest Python port of ``ℕ | (χ > fix(3_c))`` is ``ℕ | (χ > 3)``.

Scope: the decidable Boole corner (Python ``in`` truth-tests, so no Unknown).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable


# ── χ: the coordinate scout; comparisons build structured predicate nodes ──
@dataclass(frozen=True)
class _Cmp:
    op: str
    rhs: Any

    def __call__(self, x: Any) -> bool:
        return {
            ">": lambda: x > self.rhs,
            "<": lambda: x < self.rhs,
            ">=": lambda: x >= self.rhs,
            "<=": lambda: x <= self.rhs,
            "==": lambda: x == self.rhs,
            "!=": lambda: x != self.rhs,
        }[self.op]()

    def __repr__(self) -> str:
        return f"(χ {self.op} {self.rhs})"


class _Chi:
    """The coordinate scout ``χ`` (the identity projection).  A comparison
    against it returns a *structured* predicate node, never a Python lambda."""

    def __gt__(self, r: Any) -> _Cmp:
        return _Cmp(">", r)

    def __lt__(self, r: Any) -> _Cmp:
        return _Cmp("<", r)

    def __ge__(self, r: Any) -> _Cmp:
        return _Cmp(">=", r)

    def __le__(self, r: Any) -> _Cmp:
        return _Cmp("<=", r)

    def __eq__(self, r: Any) -> _Cmp:  # type: ignore[override]
        return _Cmp("==", r)

    def __ne__(self, r: Any) -> _Cmp:  # type: ignore[override]
        return _Cmp("!=", r)

    __hash__ = None  # type: ignore[assignment]

    def __repr__(self) -> str:
        return "χ"


χ = _Chi()


# ── the lattice combinators ∧ / ∨ / ¬ (the C++ Meet/Join/Not mirror) ───────
@dataclass(frozen=True)
class _Meet:
    a: Callable[[Any], bool]
    b: Callable[[Any], bool]

    def __call__(self, x: Any) -> bool:
        return bool(self.a(x)) and bool(self.b(x))

    def __repr__(self) -> str:
        return f"({self.a!r} ∧ {self.b!r})"


@dataclass(frozen=True)
class _Join:
    a: Callable[[Any], bool]
    b: Callable[[Any], bool]

    def __call__(self, x: Any) -> bool:
        return bool(self.a(x)) or bool(self.b(x))

    def __repr__(self) -> str:
        return f"({self.a!r} ∨ {self.b!r})"


@dataclass(frozen=True)
class _Not:
    a: Callable[[Any], bool]

    def __call__(self, x: Any) -> bool:
        return not bool(self.a(x))

    def __repr__(self) -> str:
        return f"¬{self.a!r}"


# ── sets: a predicate closed under & | ~, membership via `in` ──────────────
@dataclass(frozen=True)
class Set:
    """An intensional set: its characteristic predicate χ_S, closed under the
    lattice operators.  ``x in S`` and ``S(x)`` both evaluate χ_S(x)."""

    pred: Callable[[Any], bool]

    def __contains__(self, x: Any) -> bool:  # x ∈ S
        return bool(self.pred(x))

    def __call__(self, x: Any) -> bool:  # χ_S(x)
        return bool(self.pred(x))

    def __and__(self, other: "Set") -> "Set":  # A ∩ B
        return Set(_Meet(self.pred, other.pred))

    def __or__(self, other: "Set") -> "Set":  # A ∪ B
        return Set(_Join(self.pred, other.pred))

    def __invert__(self) -> "Set":  # ∁A
        return Set(_Not(self.pred))

    def __repr__(self) -> str:
        return f"{{ x | {self.pred!r} }}"


class _Universe:
    """A canonical set (universe boundary): every carrier value is a member,
    and ``| pred`` is the set-builder filter ``S | χ``."""

    def __init__(self, name: str) -> None:
        self.name = name

    def __contains__(self, x: Any) -> bool:
        return True

    def __or__(self, pred: Callable[[Any], bool]) -> Set:  # ℕ | (χ > 3)
        return Set(pred)

    def __repr__(self) -> str:
        return self.name


# The canonical carrier sets (universe boundaries).  ℕ is Python's unbounded
# ``int`` line (see numbers.natural); 𝔹 the two-element Boolean universe.
ℕ = _Universe("ℕ")
𝔹 = _Universe("𝔹")


def ext(store: Any, s: Set) -> set:
    """The partial extension μ (``materialise`` → ``ext``, #915): keep the
    members of a finite ``store`` where χ_S accepts.  The intensional →
    extensional bridge (Listing 5)."""
    return {x for x in store if x in s}
