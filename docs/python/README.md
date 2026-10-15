# Python User Guide

The C++ core owns the mathematics; Python holds handles to it.
Each native extension (`nanobind_*.cpp`) binds the library's own types; nothing
is decided in Python.

## Modules

- `dedekind.jlt`: the arrows `id`, `refl`, `succ`, `pred` over `bool` and
  `int` (the word u64), composed with `>>`.
- `dedekind.pst`: the finite carriers and the sets over them.
  - The truth chains `B` (𝔹) and `K3` (K₃), and ℕ's proxy `N`.
  - The canonical ring `u64` (ℤ/2⁶⁴: `+ − ·` wrap, `//` and `%` by a nonzero
    word, ordered) and field `𝔽ₚ` (p = 2⁶⁴ − 59: `+ − ·` and `/` by a nonzero
    element, no order).  `𝔽ₚ` is the name `Fp` (Python normalises identifiers).
  - The term surface: `π`, `π_1`, `π_2` (`π[k]` the family) and constants build terms; `A(carrier) |
    term` binds them, and `A(carrier) * A(carrier) | term` is a relation.

The laws decide the syntax: a term may use the operators the carrier's
certified algebras have, and anything else is refused when the set is built.
Equality of sets is equality in the free algebra the term lives in, where that
has a normal form: a chain's run lists, and a ring's or field's polynomials
(`canonical(S)`: the same set, its equations `p == 0` with `p` a polynomial).
Equal normal forms cut out equal sets, so `==` answers `True` when they agree
and raises `TypeError` when they differ, which decides nothing.
A term outside that catalogue (a remainder, a division by a term) evaluates but
does not compare.

## Install

### Using make jupyter (recommended for notebooks)

From the repository root:

```bash
make jupyter
```

This creates a `.venv` if needed, builds the C++ library, installs the
`dedekind` package into `.venv`, and opens a Jupyter Notebook server in
`docs/python/notebooks/`.

### From source (packaging / wheel)

For wheel/sdist builds and non-notebook use:

```bash
python -m pip install --upgrade pip
python -m pip install .
```

For iterative local work without Jupyter:

```bash
python -m pip install --upgrade pip
python -m pip install --no-build-isolation -e .
```

## Quickstart

```python
from dedekind.jlt import succ
from dedekind.pst import A, image, u64, 𝔽ₚ, π, π_1, π_2

U = A(u64)
region = (U | (π > 3)) & (U | (π < 5))
print(region == U | (π == 4))           # True: one normal form, the run list
print(region.runs(), list(region))      # [(4, 4)] [4]

third = A(𝔽ₚ) * A(𝔽ₚ) | (π_2 == π_1 / 3)  # a field's division by a constant
print((3, 1) in third)                  # True

print(image(succ(int), U | (π > 5)) == U | ((π > 6) | (π == 0)))   # True: it wraps
```

## Error Handling Contract

Refusals are raised when a term is written or bound, never when it is read:

- an operator the carrier's algebras do not have (`/` on `u64`, `<` on `𝔽ₚ`,
  `+` on 𝔹, a division by a term): `TypeError`;
- a constant the carrier cannot hold (`A(u64) | (π > -1)`): `ValueError`;
- a division by the constant 0, or by a constant that is 0 on the carrier
  (`π / p` on 𝔽ₚ): `ValueError`;
- equality of sets outside the catalogue (`A(u64) | (π % 2 == 0)`):
  `TypeError`.

These are covered by `src/test/python/test_pst_terms.py` and
`test_pst_runs.py`.

## Smoke Test

The repository includes a Python smoke test integrated into CTest:

```bash
ctest --test-dir build --output-on-failure -R test_python_bindings
```

For the broader Python integration lane, run:

```bash
make integration-test
```

This target runs the standard test suite, then discovers and executes every
notebook in `docs/python/notebooks/` headlessly. Each integration notebook is
required to import `dedekind` explicitly.

## CI Modes (Current Decision Boundary)

The current workflow effectively has two execution depths:

- CI mode (fast path): compile + test signal for PR iteration speed.
- Integration mode (full path): packaging + notebook execution + artifact
  verification, used when validating end-to-end delivery readiness.

Practical guidance:

1. Use `make test` during rapid local/PR iteration when the question is core C++
   build-and-test correctness.
2. Use `make integration-test` before review/merge when Python facade,
   notebooks, and packaging assumptions must be validated together.
3. Treat integration evidence (`python-notebooks`, `python-dist`) as required
   for staged publication decisions.

This split is intentionally conservative for now: it preserves release confidence
while giving us a clear place to optimize CI runtime (issue #243) without
dropping end-to-end checks required by staged publication (issue #240).

## Notebook Demos

The notebooks live in `docs/python/notebooks/`; each asserts what it shows, so
it is also a test:

- `01_terms_on_the_ring_and_the_field.ipynb`: the canonical ring and field, the
  README exhibit as one normal form, refusals at construction, the successor's
  image wrapping, the ring's polynomial normal form.
- `02_pst_sets_by_evaluation.ipynb`: sets over the truth chains, with `Unknown`
  as a verdict.

CI verifies execution via `make integration-test`; rendered copies
of executed notebooks are uploaded as the `python-notebooks` artifact on every
CI run.

To run the notebooks interactively, use:

```bash
make jupyter
```
## Reviewer Verification (CI Artifacts)

For pull requests, wheel/sdist and Python-native docs are expected to be
published as GitHub Actions artifacts from the `C++ CI with Cmake` workflow.

Review path:

1. Open the PR checks and select the `C++ CI with Cmake` run.
2. Open the run's `Artifacts` section.
3. Download `python-notebooks` and confirm it includes:
   - executed notebook outputs from `docs/python/notebooks/`
   - `integration-summary.txt` with per-notebook pass/fail status
4. Download `python-dist` and confirm it includes:
   - wheel/sdist files from `dist/`
   - distribution manifest (`dist-manifest.txt`)
   - distribution checksums (`dist-sha256.txt`)
   - installed wheel report (`install-report.txt`)
   - generated pydoc output (`pydoc-dedekind.txt`)
5. Confirm the workflow step `Verify Installed Wheel Smoke Test` is green.
6. Confirm the workflow step `Build & Run Integration Tests` is green.

This provides reviewer-visible evidence that packaging and runtime smoke checks
worked in CI for the PR changeset.

## Related Work

- Python bindings MVP: #234
- User docs and release checklist: #236
- Notebook demos: #239
- DSL design sketch: #241
