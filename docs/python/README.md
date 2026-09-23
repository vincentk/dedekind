# Python MVP User Guide

This guide covers the current Python MVP surface built on top of the C++ `dedekind.python` facade.

Design slogan: the C++ core owns the mathematics; Python holds handles to it.

Operational policy: the Python facade is handle-only over the native core.
Python objects are thin handles plus accessibility bits (`__repr__`,
`__contains__`, membership, `ext`); they carry no Python-side predicate AST and
no Python-side reducer. Composition (`& | ~`, set-builder filters) is deferred
to the value-first reducer on the C++ side (#922) and is not reimplemented in
Python.

## Iteration Architecture (Current)

This iteration uses a three-tier architecture:

1. Top-level: notebooks
   - Define the look-and-feel and user interaction model.
   - Must execute in CI and produce visible output for review.
2. Middle tier: the native Python facade (`dedekind._dedekind`)
   - Nanobind handles over the C++ core: canonical sets, membership, `ext`.
   - Deliberately thin: no Python-side predicate structure or reduction.
3. Lower tier: C++ core logic
   - Owns the core mathematical invariants and implementation semantics.
   - Python and notebook layers are consumers of this source of truth.

## Scope

Current Python bindings intentionally expose a small, reviewable API:

- interop/path smoke helpers: `ordered_set_roundtrip(values)`,
  `unordered_set_roundtrip(values)`, `path_from_range(values)`
- canonical native sets (#886): `𝔹` (the Boolean universe `𝔸<bool>`), `ℕ`
  (the ambient natural-numbers universe `𝔸<Cardinality>`), and `Nat` (the
  discriminating ℕ⊂ℤ classifier, χ: x ↦ x ≥ 0). Membership `x in s` runs the
  native characteristic morphism χ from the C++ core.
- `ext(universe)`: the native retraction μ: Int ⇀ Ext. It materialises a finite
  candidate universe through the native ℕ⊂ℤ classifier into a Python `set`, so
  `ext([-2, -1, 0, 1, 2]) == {0, 1, 2}` is decided in C++.

There is no pandas dependency and no pure-Python DSL: those pre-target-
architecture modules were removed in the #886 cleanup. The facade is handle-
only over the native core.

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
import dedekind
from dedekind import 𝔹, ℕ, Nat, ext  # 𝔹→B, ℕ→N via NFKC normalisation

print(dedekind.ordered_set_roundtrip([3, 1, 2, 2]))
print(dedekind.unordered_set_roundtrip([4, 2, 4, 1]))
print(dedekind.path_from_range([2, 4, 6, 8]))

# Membership runs the native χ from the C++ core, not a Python reimplementation.
print(True in 𝔹)          # True
print(4 in ℕ)             # True (ℕ is the ambient universe: χ_ℕ always holds)
print(-7 not in Nat)      # True (the discriminating ℕ⊂ℤ classifier, decided in C++)
print(ext([-2, -1, 0, 1, 2]))  # {0, 1, 2}
```

Expected output:

```text
[1, 2, 3]
[1, 2, 4]
[2, 4, 6, 8]
True
True
True
{0, 1, 2}
```

## Caveats (MVP)

- API stability: function names and packaging details may change before a stable v0.1.0 release.
- Coverage: only a narrow interop/path surface is bound in this phase.
- Semantics: this package is a consumer of system-language semantics defined in C++ modules; Python is not the source of core invariants.
- Performance: set conversion roundtrips are linear in collection size. Container lookup properties still follow destination container behavior.
- **Handle-only, composition deferred**: Python holds handles plus accessibility bits over the native core; it carries no Python-side predicate AST or reducer. Composition (`& | ~`, set-builder filters) and passing a Python predicate into `ext` are deferred to the value-first reducer on the C++ side (#922). The pure-Python DSL shims that once sketched this surface were removed in the #886 cleanup.

## Error Handling Contract (MVP)

- The current bound functions accept integer sequences for this MVP.
- Invalid payloads are rejected at the Python boundary with `TypeError` (for example, non-iterables where a sequence is required, or non-integral elements in an otherwise iterable payload).
- This behavior is covered by unhappy-path smoke tests in `src/test/python/test_dedekind.py` and surfaced in CI logs.

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

The MVP notebook demos live in `docs/python/notebooks/`:

- `01_facade_roundtrip_basics.ipynb`: happy-path facade demo plus the native
  canonical-sets (`𝔹` / `ℕ`) README exhibit (membership via the C++ `χ`).

The DSL-tier notebooks (`03`/`04`) were removed with the pre-target pure-Python
modules in the #886 cleanup; the notebook set is kept to a single native-facade
smoke test that executes as a CI integration check.

Notebook outputs are committed to version control so that GitHub renders them
without executing code. To refresh outputs locally:

```bash
make jupyter
# execute each notebook in the Jupyter UI, then save and commit the outputs
```

CI independently verifies execution via `make integration-test`; rendered copies
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
