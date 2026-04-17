# Contributing to Dedekind

Thank you for your interest in contributing!

## How to contribute

1. **Fork** the repository on GitHub and clone your fork locally.
2. **Pick an issue** from the [issue tracker](https://github.com/vincentk/dedekind/issues).
   If you have a new idea, open an issue first so scope and direction can be agreed on before
   you invest time in an implementation.
3. **Create a branch** named after the issue(s) you are addressing, e.g.
   `feat/issues-40-121`.
4. **Implement your changes** and push to a draft PR early and often — you do not need
   to verify that the code compiles or tests pass locally before each push.  The CI build
   is the reference build; offloading compilation and test execution to CI is explicitly
   encouraged so local development is not blocked by slow or unavailable toolchains.
   Use the `Makefile` targets when a local build is convenient:
   ```bash
   make compile   # configure (first run) and build everything
   make test      # build then run the full CTest suite
   ```
   The `Makefile` selects the correct compiler (`clang++` from LLVM) and passes
   all required CMake flags automatically, including `DEDEKIND_ENABLE_DOUBLE_REAL_PROXY=ON`
   which is needed for tests that use `double` as the scalar type.
5. **Format** before committing — the CI will reject unformatted code:
   ```bash
   make format   # runs clang-format-21 on all *.cpp / *.cppm files
   ```
   To enforce this automatically before each push, install the repo-managed hook once:
   ```bash
   make install-hooks
   ```
   The `pre-push` hook runs `make format` and aborts the push if it had to rewrite files,
   so you can review and commit the formatting changes explicitly.
6. **Open a draft PR** against `main` early — even before the implementation is complete.
   Push incremental checkpoints freely; the draft status signals work-in-progress.
   Mark the PR **Ready for Review** only once **all CI checks are green**.

## Development workflow

- The `Makefile` is the **preferred** build interface; use `make <target>` rather than
   raw `cmake`/`ninja`/`ctest` commands whenever an equivalent target exists.
   Available targets: `compile`, `test`, `format`, `coverage`, `doxygen`, `clean`, `install-hooks`.
- Treat the GitHub CI build as the reference build for the project.  Local builds are useful,
   but merge readiness is determined by the PR checks.
- Before starting new work, check that the most recent `main` branch CI run is green.
- Keep the README small and stable.  Only update it when something is plainly wrong or deeply
   misleading; routine progress belongs in the report and inline documentation.
- Prefer plain `gh` CLI commands when working with issues, pull requests, and CI.
- Open a draft PR early, even before the implementation is complete.  Push checkpoints
   freely — you do not need to confirm that the code compiles or tests pass locally before
   pushing.  The CI build is the authoritative reference; use it to offload compilation and
   test validation so local development stays unblocked.
- This project intentionally follows an optimistic concurrency model: contributors can
   continue shipping small draft-PR checkpoints while CI validates correctness in parallel,
   which improves overall throughput and reduces workstation idle/blocking time.
- Before pushing, run `make format` or install the managed hook with `make install-hooks`.
- Check the PR's CI status before each push to keep optimistic concurrency tight:
   push small checkpoints, but verify the reference build state first so failures do not
   compound into large divergences. If a check fails, read the workflow logs and address
   the failure before continuing with unrelated work.
- After a green CI run, also check the Codecov report for regressions before marking the PR ready.
- Mark the PR **Ready for Review** only once **all CI checks show green**.  A draft PR
   with failing CI should never be marked ready.

## Review workflow

- Resolve review comments and threads explicitly; do not leave unresolved conversations behind
   when preparing a PR for merge.
- For low-risk mechanical fixes, batching routine changes is acceptable; for semantic or API
   changes, prefer manual edits so the trade-offs remain explicit.
- After pushing follow-up commits, re-request review so the latest state is reviewed.

## Alignment with the codebase

- Follow the module/partition layout described in `CMakeLists.txt` and the existing
  `main` partitions.
- Mirror the doxygen header style used in neighbouring files (brief, partition summary,
  copyright notice, a quote).
- Keep concepts and naming aligned with the textbook literature cited in
  `docs/report/references.bib`.  UTF-8 mathematical symbols (ℤ, ℝ, …) are preferred
  over verbose ASCII alternatives.
- Add or extend tests in the corresponding `src/test/cpp/modules/…` file.

## Questions

Open an issue or leave a comment on an existing one — contributions of any size are
welcome.
