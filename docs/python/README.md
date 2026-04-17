# Python MVP User Guide

This guide covers the current Python MVP surface built on top of the C++ `dedekind.python` facade.

## Scope

Current Python bindings intentionally expose a small, reviewable API:

- `ordered_set_roundtrip(values)`
- `unordered_set_roundtrip(values)`
- `path_from_range(values)`

These functions validate the initial interop and sequence/path boundaries for notebook and scripting workflows.

## Install

### From source (recommended for MVP)

From the repository root:

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
import dedekind

print(dedekind.ordered_set_roundtrip([3, 1, 2, 2]))
print(dedekind.unordered_set_roundtrip([4, 2, 4, 1]))
print(dedekind.path_from_range([2, 4, 6, 8]))
```

Expected output:

```text
[1, 2, 3]
[1, 2, 4]
[2, 4, 6, 8]
```

## Caveats (MVP)

- API stability: function names and packaging details may change before a stable v0.1.0 release.
- Coverage: only a narrow interop/path surface is bound in this phase.
- Semantics: this package is a consumer of system-language semantics defined in C++ modules; Python is not the source of core invariants.
- Performance: set conversion roundtrips are linear in collection size. Container lookup properties still follow destination container behavior.

## Error Handling Contract (MVP)

- The current bound functions accept integer sequences for this MVP.
- Invalid payloads are rejected at the Python boundary with `TypeError` (for example, non-iterables where a sequence is required, or non-integral elements in an otherwise iterable payload).
- This behavior is covered by unhappy-path smoke tests in `src/test/python/test_dedekind.py` and surfaced in CI logs.

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
	- generated pydoc output (`pydoc-dedekind.txt`)
4. Confirm the workflow step `Verify Installed Wheel Smoke Test` is green.

This provides reviewer-visible evidence that packaging and runtime smoke checks
worked in CI for the PR changeset.

## Related Work

- Python bindings MVP: #234
- User docs and release checklist: #236
- Notebook demos: #239
