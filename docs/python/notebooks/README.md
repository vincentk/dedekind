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

3. `03_dsl_beginner_tier.ipynb`
   - First executable DSL sketch for beginner users (issue #241)
   - Uses SQL/Python-like syntax: `where()`, `select()`, method chaining
   - Demonstrates intensional (symbolic) → extensional (realized) progression
   - Prototype shim for API ergonomics discussion

4. `04_dsl_expert_tier.ipynb`
   - Second executable DSL sketch for expert users (issue #241)
   - Uses mathematical terminology: `comprehension()`, `restrict()`, `map_to()`
   - Demonstrates same intensional → extensional progression with formal notation
   - Prototype shim for mathematical API surface

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

### Notebook 3: Beginner-tier DSL

Demonstrates accessible set operations with SQL/Python terminology.

Inputs:

- Set A: `{1, 2, 3, 4, 5}`
- Set B: `{3, 4, 5, 6, 7}`
- Filtered set E: `{x ∈ [1..20] | x % 2 == 0}`
- Mapped set S: `{x² | x ∈ [1..10], x % 2 == 0}`

Expected outputs (intensional → extensional progression):

- Union A ∪ B = `[1, 2, 3, 4, 5, 6, 7]`
- Intersection A ∩ B = `[3, 4, 5]`
- Difference A \ B = `[1, 2]`
- Evens: `[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]`
- Squares: `[4, 16, 36, 64, 100]`
- Notebook ends with success marker `notebook-03-ok`

### Notebook 4: Expert-tier DSL

Demonstrates mathematical notation and formal set-theoretic operations.

Inputs:

- Ensemble A: `{1, 2, 3, 4, 5}`
- Ensemble B: `{3, 4, 5, 6, 7}`
- Comprehension E: `{n ∈ [1..30] | n ≡ 0 (mod 2)}`
- Comprehension O: `{n ∈ [1..30] | n ≡ 1 (mod 2)}`
- Morphism image Q: `{n² | n ∈ E}`

Expected outputs (intensional → extensional progression):

- Union A ∪ B = `[1, 2, 3, 4, 5, 6, 7]`
- Intersection A ∩ B = `[3, 4, 5]`
- Difference A \ B = `[1, 2]`
- Evens: `[2, 4, 6, ..., 28, 30]` (15 elements)
- Odds: `[1, 3, 5, ..., 27, 29]` (15 elements)
- Squares image: `[4, 16, 36, ..., 900]` (15 elements)
- Notebook ends with success marker `notebook-04-ok`

## Non-goals

- No complete symbolic API yet; notebooks are UX prototypes for discussion
- Notebooks 03 and 04 are prototype shims, not the final package surface
- Output correctness is a follow-up; the shims demonstrate desired input/execution patterns
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
