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
