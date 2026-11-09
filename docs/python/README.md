# Python User Guide

The [API reference](https://vincentk.github.io/dedekind/python/) is generated from the docstrings below; the [C++ API reference](https://vincentk.github.io/dedekind/) is its counterpart.

The C++ core owns the mathematics; Python holds handles to it.
Each native extension (`nanobind_*.cpp`) binds the library's own types; nothing
is decided in Python.

## Modules

- `pynzli.jlt`: the arrows `id`, `refl`, `succ`, `pred` over `bool` and
  `int` (the word u64), composed with `>>`.
- `pynzli.pst`: the finite carriers and the sets over them.
  - The universes `B = A(bool)` (𝔹) and `K3 = A(Ternary)` (K₃), carrying their
    chain's order; `A(Ternary, Kleene)` is K₃-valued.
  - The canonical ring `u64` (ℤ/2⁶⁴: `+ − ·` wrap, `//` and `%` by a nonzero
    word, ordered) and field `𝔽ₚ` (p = 2⁶⁴ − 59: `+ − ·` and `/` by a nonzero
    element, no order).  `𝔽ₚ` is the name `Fp` (Python normalises identifiers).
  - The term surface: `π`, `π_1`, `π_2` (`π[k]` the family) and constants build terms; `A(carrier) |
    term` binds them, and `A(carrier) * A(carrier) & term` is a relation.

The laws decide the syntax: a term may use the operators the carrier's
certified algebras have, and anything else is refused when the set is built.
Equality of sets is equality in the free algebra the term lives in, where that
has a normal form: a chain's run lists, and a ring's or field's polynomials
(`canonical(S)`: the same set, a chain fragment as its run list written back,
a ring's equations as `p == 0` with `p` a polynomial).
Equal normal forms cut out equal sets, so `==` answers `True` when they agree
and raises `TypeError` when they differ, which decides nothing.
A term outside that catalogue (a remainder, a division by a term) evaluates but
does not compare.

## Install

### From source (packaging / wheel)

For wheel/sdist builds:

```bash
python -m pip install --upgrade pip
python -m pip install .
```

For iterative local work:

```bash
python -m pip install --upgrade pip
python -m pip install --no-build-isolation -e .
```

## Quickstart

```python
from pynzli.jlt import succ
from pynzli.pst import A, U64, canonical, image, u64, 𝔽ₚ, π, π_1, π_2

U = U64  # = A(u64)
region = (U & (π > 3)) & (U & (π < 5))
print(region == U & (π == 4))           # True: one normal form, the run list
print(canonical(region), list(region))  # 𝔸(u64) & π == 4  [4]

third = A(𝔽ₚ) * A(𝔽ₚ) & (π_2 == π_1 / 3)  # a field's division by a constant
print((3, 1) in third)                  # True

print(image(succ(int), U & (π > 5)) == U & ((π > 6) | (π == 0)))   # True: it wraps
```

## Error Handling Contract

Refusals are raised when a term is written or bound, never when it is read:

- an operator the carrier's algebras do not have (`/` on `u64`, `<` on `𝔽ₚ`,
  `+` on 𝔹, a division by a term): `TypeError`;
- a constant the carrier cannot hold (`A(u64) & (π > -1)`): `ValueError`;
- a division by the constant 0, or by a constant that is 0 on the carrier
  (`π / p` on 𝔽ₚ): `ValueError`;
- equality of sets outside the catalogue (`A(u64) & (π % 2 == 0)`):
  `TypeError`.

These are covered by `src/test/python/test_pst_terms.py` and
`test_pst_runs.py`.

## Smoke Test

The repository includes a Python smoke test integrated into CTest:

```bash
ctest --test-dir build --output-on-failure -R test_python_bindings
```

## Reviewer Verification (CI Artifacts)

For pull requests, wheel/sdist and Python-native docs are expected to be
published as GitHub Actions artifacts from the `C++ CI with Cmake` workflow.

Review path:

1. Open the PR checks and select the `C++ CI with Cmake` run.
2. Open the run's `Artifacts` section.
3. Download `python-dist` and confirm it includes:
   - wheel/sdist files from `dist/`
   - distribution manifest (`dist-manifest.txt`)
   - distribution checksums (`dist-sha256.txt`)
   - installed wheel report (`install-report.txt`)
   - generated pydoc output (`pydoc-pynzli.txt`)
4. Confirm the workflow step `Verify Installed Wheel Smoke Test` is green.
5. Confirm the workflow step `Build & Run the Tests` is green.

This provides reviewer-visible evidence that packaging and runtime smoke checks
worked in CI for the PR changeset.

## Related Work

- Python bindings MVP: #234
- User docs and release checklist: #236
- DSL design sketch: #241
