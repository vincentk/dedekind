# Python Facade Notebook Demos

This directory contains one demo notebook for the Python native facade. A single
notebook is kept to exercise the integration pipeline (`make integration-test`).
The earlier DSL-tier prototype notebooks (03/04) were removed together with the
pre-target pure-Python modules (`dsl` / `sequences` / `numbers` / `algebra`) in
the #886 cleanup; the handle-only surface will grow its own demos later.

## Notebook

1. `01_facade_roundtrip_basics.ipynb`
   - Imports the `dedekind` package
   - Demonstrates the three MVP roundtrip/path calls
   - Prints deterministic expected outputs

## Payload definition

The notebook exercises the native Python facade:

- `ordered_set_roundtrip(values)`
- `unordered_set_roundtrip(values)`
- `path_from_range(values)`

### Happy-path payload

Inputs:

- ordered roundtrip: `[3, 1, 2, 2]`
- unordered roundtrip: `[4, 2, 4, 1]`
- path from range: `[2, 4, 6, 8]`

Expected outputs:

- ordered roundtrip: `[1, 2, 3]`
- unordered roundtrip: `[1, 2, 4]`
- path from range: `[2, 4, 6, 8]`

## Non-goals

- No complete symbolic API yet; the notebook is a UX smoke test for the facade.
- No performance benchmarking yet.
- No publication workflow yet; release publication stays deferred to #240.

## How to run

From repository root:

```bash
python -m pip install --upgrade pip
python -m pip install .
python -m pip install jupyter
jupyter notebook docs/python/notebooks/
```

To run as integration checks:

```bash
make integration-test
```

`integration-test` discovers all `*.ipynb` files in this directory, executes each
notebook headlessly, and fails if any notebook execution fails.

## Notes

- The notebook is intentionally small and deterministic to support quick review.
- It mirrors the current native facade scope documented in `docs/python/README.md`.
