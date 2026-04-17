# Python v0.1 Release Checklist

This checklist defines a repeatable publication path for the Python MVP package.

## Pre-release

1. Confirm branch/PR readiness.
2. Ensure CI is green for the release candidate PR.
3. Confirm unresolved review threads are zero.
4. Confirm docs are up to date:
   - `docs/python/README.md`
   - `docs/python/release-checklist-v0.1.md`
   - `docs/python/notebooks/README.md`
5. Confirm package metadata in `pyproject.toml`:
   - version
   - description
   - Python requirement
   - classifiers
6. Confirm notebook integration checks pass:
   - `make integration-test`

## Build

From repository root:

```bash
python -m pip install --upgrade pip build twine
python -m build
```

Expected artifacts:

- `dist/*.whl`
- `dist/*.tar.gz`

## Validate Artifacts Locally

```bash
python -m twine check dist/*
python -m pip install --force-reinstall dist/*.whl
python -c "import dedekind; print(dedekind.path_from_range([1, 2, 3]))"
make integration-test
```

## TestPyPI Publish (recommended)

```bash
python -m twine upload --repository testpypi dist/*
```

Then verify in a clean environment:

```bash
python -m pip install --index-url https://test.pypi.org/simple/ dedekind
python -c "import dedekind; print(dedekind.ordered_set_roundtrip([2, 1, 2]))"
```

## Production PyPI Publish

```bash
python -m twine upload dist/*
```

## Post-release Verification

1. Confirm package appears on PyPI with expected version and metadata.
2. Install from PyPI in a clean environment.
3. Verify import succeeds and representative calls work:
   - `ordered_set_roundtrip`
   - `unordered_set_roundtrip`
   - `path_from_range`
4. Confirm CI remains green after release tagging.
5. Confirm notebook demos still execute successfully against the published package expectations.
6. Post release note linking:
   - issue #234 scope delivered for MVP surface
   - issue #236 docs/checklist maintenance
   - issue #239 notebook demos and integration coverage

## Rollback / Recovery

If a release is broken:

1. Do not overwrite existing artifact files for the same version.
2. Cut a patch version (for example, `0.1.1`) with fixes.
3. Publish corrected artifacts under the new version.
4. Update docs with a short incident note and migration guidance.

## Follow-up Backlog Pointers

- Notebook demo set: #239
- Broader API surface expansion after MVP validation: #234
- Documentation iteration and user feedback loop: #236
