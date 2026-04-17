# Contributing to Dedekind

Thank you for your interest in contributing!

## How to contribute

1. **Fork** the repository on GitHub and clone your fork locally.
2. **Pick an issue** from the [issue tracker](https://github.com/vincentk/dedekind/issues).
   If you have a new idea, open an issue first so scope and direction can be agreed on before
   you invest time in an implementation.
3. **Create a branch** named after the issue(s) you are addressing, e.g.
   `feat/issues-40-121`.
4. **Implement and test** your changes.  The project builds with CMake + Ninja:
   ```bash
   cmake -B build -G Ninja
   cmake --build build
   ctest --test-dir build --output-on-failure
   ```
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
6. **Open a pull request** against `main` from your fork branch.  Draft PRs are welcome
   early; mark it ready for review once CI is green.

> `main` is a protected branch.
> Direct pushes to `main` are blocked. A merge (or any push path that targets protected branches)
> requires a PR, a passing build, at least one review, and resolution of review conversations.

## Development workflow

- Treat the GitHub CI build as the reference build for the project.  Local builds are useful,
   but merge readiness is determined by the PR checks.
- Before starting new work, check that the most recent `main` branch CI run is green.
- Keep the README small and stable.  Only update it when something is plainly wrong or deeply
   misleading; routine progress belongs in the report and inline documentation.
- Prefer plain `gh` CLI commands when working with issues, pull requests, and CI.
- Open a draft PR early, even before the implementation is complete, so the work is visible and
   the reference CI starts running immediately.
- Before pushing, run `make format` or install the managed hook with `make install-hooks`.
- Poll PR checks regularly during development.  If a check fails, inspect the failed workflow
   logs before making further changes.
- After a green CI run, also check the Codecov report for regressions before marking the PR ready.
- Once CI is green and coverage looks acceptable, mark the PR ready for review.
- Remember that protected-branch rules are enforced server-side: PR + green CI + review +
   resolved conversations are mandatory before merge to `main`.

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
