# Python Facade Notebook Demos

This directory contains demo notebooks for the Python MVP facade.

## Notebooks

1. `01_facade_roundtrip_basics.ipynb`
   - Imports the `dedekind` package
   - Demonstrates the three MVP roundtrip/path calls
   - Prints deterministic expected outputs

2. `02_facade_error_contract.ipynb`
   - Demonstrates the accepted input contract
   - Shows representative failing payloads
   - Confirms `TypeError` behavior at the Python boundary

3. `03_dsl_design_sketch_demo.ipynb`
   - First executable iteration of the issue #241 DSL sketch
   - Demonstrates easy-tier and symbolic-flavored usage in a prototype shim
   - Uses `dedekind` MVP facade functions at the realization boundary

## Payload Definition

These notebooks intentionally exercise only the current Python MVP facade:

- `ordered_set_roundtrip(values)`
- `unordered_set_roundtrip(values)`
- `path_from_range(values)`

### Notebook 1: Happy-path payload

Inputs:

- ordered roundtrip: `[3, 1, 2, 2]`
- unordered roundtrip: `[4, 2, 4, 1]`
- path from range: `[2, 4, 6, 8]`

Expected outputs:

- ordered roundtrip: `[1, 2, 3]`
- unordered roundtrip: `[1, 2, 4]`
- path from range: `[2, 4, 6, 8]`

### Notebook 2: Failure-path payload

Inputs:

- valid control call: `[10, 20, 30]`
- ordered invalid payload: `[1, "x", 3]`
- unordered invalid payload: `42`
- path invalid payload: `[1, None, 3]`

Expected outputs:

- valid control call succeeds and prints the preserved path
- each invalid payload raises `TypeError`
- the notebook prints per-case confirmation and ends with a success marker

### Notebook 3: DSL design sketch payload

Inputs:

- easy-tier demo sets: `[1, 2, 3]`, `[2, 3, 4]`
- symbolic-flavored demo set: finite prefix of ℕ

Expected outputs:

- easy-tier union/intersection/difference behave deterministically
- projection-style image call shows square mapping output
- symbolic-flavored even/odd partition prints expected prefixes
- notebook ends with success marker `notebook-03-ok`

## Non-goals

- No broader symbolic API coverage yet
- Notebook 3 is explicitly a prototype shim for API discussion, not final package surface
- No performance benchmarking yet
- No publication workflow yet; release publication stays deferred to #240

## How to run

From repository root:

```bash
python -m pip install --upgrade pip
python -m pip install .
python -m pip install jupyter
jupyter notebook docs/python/notebooks/
```

To run these notebooks as integration checks:

```bash
make integration-test
```

`integration-test` discovers all `*.ipynb` files in this directory, executes each notebook headlessly, and fails if any notebook execution fails.

## Notes

- These notebooks are intentionally small and deterministic to support quick review.
- They mirror the current MVP scope documented in `docs/python/README.md`.
