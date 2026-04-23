# Contributing to Dedekind

Thank you for your interest in contributing!

## How to contribute

1. **Fork** the repository on GitHub and clone your fork locally.
2. **Pick an issue** from the [issue tracker](https://github.com/vincentk/dedekind/issues).
   If you have a new idea, open an issue first so scope and direction can be agreed on before
   you invest time in an implementation.
3. **Create a branch** named after the issue(s) you are addressing, e.g.
   `feat/issues-40-121`.
   When work on a new branch is begun, all local edits SHOULD be triaged and
   either pushed to a draft branch PR or discarded to prevent divergence.
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
   Priority by PR state:
   Draft PR: prioritize closing acceptance-criteria gaps checkpoint by checkpoint.
   Near-Ready/Ready PR: prioritize polishing (clarity improvements,
   documentation/report alignment, low-risk quality refinements, and review
   thread closure).
   Mark the PR **Ready for Review** only once **all CI checks are green**.
   Before flagging a PR as ready, contributors SHOULD compare and contrast the
   updated modules and the corresponding chapters in the report, and verify the
   report documentation is up to date.

## Workflow at a glance

The intended UX is:

1. **Edit** source files locally — no local build required.
2. **Format** with `make format` (or let the pre-push hook do it).
3. **Push** — `git push` is the only command you strictly need to run.
4. **CI takes over** — GitHub Actions compiles with clang-22, runs all tests,
   collects coverage, builds Doxygen docs, and checks the LaTeX report.
   Your workstation stays idle and cool.
5. **Check progress** with `make pr-checks` or `make pr-watch` while CI runs.
6. **Iterate** — if CI flags a failure, read the workflow logs, fix locally,
   push again.  Repeat until green.
7. **Mark ready** — once all CI checks are green, mark the PR ready for review.

**When local builds are needed:** non-trivial build failures or toolchain-specific
issues that cannot be diagnosed from CI logs alone warrant a local `make compile`
or `make test`.  This should be the exception, not the routine.

The goal is to keep the contributor's focus on the mathematics and the code, not
on build management.

For modeling decisions, prefer abstract/intensional carriers and symbolic types by
default; choose a concrete runtime numeric representation only at realization points
where concrete evaluation is intentionally required.

## Operating principles

Two rules of thumb that shape how PRs are prepared and shipped in this repo. They
interact: the second justifies the first.

### Boy-scout the surface you touch

> Leave the place in better shape than you encountered it.

When preparing a PR, compare its acceptance criteria against the set of open issues
that touch the same module / API / surface. Where the current PR can make
incremental progress toward one or more of those adjacent issues without widening
scope unreasonably, it **SHOULD** do so — tiny quality improvements, dead-variable
cleanups, deduplicated helpers, better doxygen, corrected comments, test-coverage
fills, `// FIXME(#NNN)` breadcrumbs at newly-visible scope boundaries, Makefile
ergonomics, etc. The objective is **eventual consistency with the broader backlog**,
achieved one PR at a time rather than with dedicated sweeps.

The lens is _"what's one or two sizes up from the minimum acceptance bar?"_, not
_"how much can I refactor while I'm here?"_.

### Under-promise and over-deliver

> Some scope creep on the PR is desirable to enable boy-scouting.

Quote the PR's stated acceptance criteria modestly — aim to land exactly what the
issue asked for in the initial commit — and then use the draft-PR flywheel to
deliver more than quoted where the additional scope is:

1. **Directly improves the in-scope deliverable** (cleaner code, better tests,
   richer docs for what already landed), or
2. **Makes the next PR smaller** (a breadcrumb, a tiny upstream prep, a moved
   helper), or
3. **Costs essentially zero review time** (formatter runs, target renames,
   comment fixes, typos).

The key boundary: additional scope **SHOULD** be bounded by _directly improving
the landing deliverable or the adjacent issues' reach_. It is **NOT** an
invitation to open-ended expansion. When in doubt, leave a `// FIXME(#NNN)`
breadcrumb and move on — that is itself boy-scouting without scope-creeping.

Paired together, these rules pull the backlog toward consistency PR-by-PR rather
than via dedicated refactor-burn sprints, at the cost of modest PR-body creep
that is easy to justify to the reviewer.

## Development workflow

- The `Makefile` is the **preferred** build interface; use `make <target>` rather than
   raw `cmake`/`ninja`/`ctest` commands whenever an equivalent target exists.
   Available targets: `compile`, `test-compile`, `test`, `format`, `format-check`, `coverage`, `doxygen`, `report`, `paper`,
   `clean`, `install-hooks`, `ci-history`, `ci-main`, `pr-init`, `pr-status`, `pr-checks`, `pr-watch`, `pr-sync`, `pr-review-comments`, `pr-review-unresolved`.
- Contributor workflow helper targets:
  `make ci-history BRANCH=<name> [LIMIT=<n>]` checks recent CI runs for a specific branch.
  `make ci-main` checks recent `main` branch CI runs.
  `make pr-init ISSUES="234 236" [TYPE=feat] [BASE=main]` creates the issue-scoped branch, makes an empty initialization commit, pushes it, and opens the draft PR.
  `make pr-status` shows PR metadata for the current branch.
  `make pr-checks` snapshots current PR check state.
  `make pr-watch` blocks until PR checks complete.
   `make pr-sync` runs fetch/status/PR-check snapshot before a push and fails fast if the PR has merge conflicts (`mergeStateStatus` is `DIRTY`/`CONFLICTING`).
   `make pr-review-comments` lists inline review comments on the current PR (or `PR=<number>`).
   `make pr-review-unresolved` scans unresolved review threads on the current PR (or `make pr-review-unresolved PR=<number>`) and prints thread IDs.
   `make pr-resolve-thread THREAD_ID=<thread_id> REASON="<resolution note>"` resolves exactly one review thread and requires a reason.
- Treat the GitHub CI build as the reference build for the project.  Local builds are useful,
   but merge readiness is determined by the PR checks.
- **Net result:** the CI pipeline uses the same `make` targets as contributors do locally
   (`make test`, `make coverage`, `make doxygen`, `make report`).  For routine development,
   the only thing a contributor strictly needs to run locally is `git push`; GitHub Actions
   handles the heavy compilation, test execution, coverage processing, and documentation
   builds on every push.  Local builds remain necessary when debugging non-trivial build
   failures or investigating toolchain-specific issues that cannot be diagnosed from CI logs
   alone.
- Workflow model note: this project intentionally uses an optimistic concurrency
   model rather than a pessimistic one. In practice, CI is more authoritative
   than local builds, and a small temporary divergence between local and CI
   status is acceptable while iterating on draft PR checkpoints.
- Operational consistency note: this workflow is intentionally asynchronous and
   eventually consistent. Contributors should ship small checkpoints promptly,
   let CI validate in parallel, and converge based on CI feedback.
- Practical intent: during active PR development, temporary local/remote status
   skew is expected; the team should optimize for fast convergence back to green
   CI rather than for lock-step local synchronization at every step.
- Motivation: this model is not only about throughput; it also mitigates
   "works on my laptop" failures by treating CI as the reproducible baseline
   and local environments as potentially non-reproducible.
- RFC 2119 usage note (Section 6): use imperative terms proportionally.
   Reserve `MUST` / `MUST NOT` for hard governance or safety constraints,
   and prefer `SHOULD` / `SHOULD NOT` for routine workflow guidance.

## Module placement and build-order discipline

The build DAG is not merely a compilation convenience — it is a semantic
constraint.  The partial order "is defined in terms of" on the library's
mathematical concepts MUST be a refinement of the partial order "is downstream
of" on its module graph.  When a new definition is placed correctly, its
module-level position acts as a proof certificate: the fact that it compiles at
all witnesses that all its dependencies were already established.

Concretely, when introducing a new concept or definition, contributors MUST
place it in the earliest module (furthest upstream) that its mathematical
dependencies permit.  If a concept can be expressed using only `category`
primitives, it belongs in `category`, not in `sets` or `algebra`.  If it
requires `order` but nothing from `algebra`, it belongs in `order`.  Pulling a
definition downstream beyond its natural position is not merely a style issue
— it severs the compile-time proof that the definition is independent of the
downstream context.

If a concept that would greatly facilitate a new definition exists only in a
downstream module, contributors SHOULD treat this as a signal that either (a)
the facilitating concept should itself be refactored upstream, or (b) the new
definition should be expressed in more primitive terms without relying on the
downstream concept.  Introducing a circular or inverted dependency to work
around this is not permitted.

This discipline mirrors the pedagogical progression described in the report
(§2): foundational axioms reside upstream; theorems and derived constructions
appear downstream, in the same order a textbook would present them.
- Before starting new work, check that the most recent `main` branch CI run is green.
- For backlog grooming, run a short preflight in this order: check `main` CI health, then list
   open issues, then list open fork PRs, then classify each item as actionable / ambiguous /
   already resolved / exact duplicate.
- If the latest `main` CI run is red, prioritize stabilization before starting unrelated new work.
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
   Suggested command flow:
   `make pr-sync` (refresh branch + current PR state), then push, then `make pr-watch`.
- After a green CI run, also check the Codecov report for regressions before marking the PR ready.
- Mark the PR **Ready for Review** only once **all CI checks show green**.  A draft PR
   with failing CI should never be marked ready.

### Make Target Quick Reference

| Target | Description |
|---|---|
| `make clean` | Remove the `build/` directory |
| `make compile` | Configure (if needed) and build all targets |
| `make test-compile` | `make compile` + build IR-fixture showcases; type-checks every C++ TU without running tests (fast local sanity gate) |
| `make test` | Build then run the full test suite via CTest |
| `make clean compile test` | Clean, rebuild, and run the verification suite |
| `make format` | Auto-format all `*.cpp` / `*.cppm` sources with `clang-format` |
| `make format-check` | Verify formatting without modifying files |
| `make coverage` | Build, run tests, and produce an LLVM coverage report |
| `make python-coverage` | Run Python tests and generate `build/python-coverage.xml` |
| `make doxygen` | Build Doxygen API docs into `build/docs/` |
| `make report` | Fast report compile check (`docs/report` `ci-check`) |
| `make install-hooks` | Install the pre-push git hook |

### Backlog Grooming And CI Health Quick Loop

1. Check CI health for `main`:
   - `make ci-history BRANCH=main LIMIT=3`
2. Collect backlog inputs:
   - `gh issue list --state open --limit 50 --json number,title,body,labels,comments`
   - `gh pr list --state open --json number,title,body,headRepositoryOwner --jq '.[] | select(.headRepositoryOwner.login != "vincentk")'`
3. Classify each item:
   - Actionable: clear scope and no blocking ambiguity; include in current plan.
   - Ambiguous: add a clarifying issue/PR comment and defer implementation.
   - Already resolved: close with a note referencing the resolving PR.
   - Exact duplicate: keep one canonical issue open and close duplicates with a cross-reference comment.
4. Pick a focused batch:
   - Prefer exactly two actionable items in parallel when feasible.

### Backlog Prioritization Matrix (Correlation x Causality)

When selecting from the backlog, use the quadrant labels to separate broad thematic correlation from likely enabling/causal force:

- `priority:q1-high-corr-high-causal`:
   High cross-cluster leverage and likely enabling effect. Default first-pick lane.
- `priority:q2-high-corr-low-causal`:
   Strong bridge/correlation, but usually not a strict prerequisite.
- `priority:q3-low-corr-high-causal`:
   Narrow scope, but locally blocking/enabling for a concrete chain.
- `priority:q4-low-corr-low-causal`:
   Exploratory or lower immediate leverage.

Selection heuristic:

1. Prefer Q1 issues for primary execution.
2. Pull one Q2 issue in parallel when bridge visibility is useful.
3. Use Q3 issues to unblock local chains when blocked.
4. Batch or defer Q4 unless it unlocks a near-term objective.

Operational note:

- Treat quadrant labels as best-effort, revisable metadata rather than permanent truth.
- When new evidence appears, relabel and add a short rationale comment on the issue.
- Keep native parent-child links for causal decomposition; use cross-references/comments for correlation edges.

### Coverage Verification

The project collects both C++ (via LCOV) and Python (via pytest-cov) coverage on every CI run.
Both reports are uploaded to Codecov for centralized visibility.

**Verifying Python coverage presence (for reviewers):**

1. Open the PR's Codecov report link (available in PR checks or at `https://app.codecov.io/gh/vincentk/dedekind`).
2. In the "Files" tab, verify that Python source files are listed alongside C++ files:
   - Look for entries like `python/dedekind/__init__.py`, `python/dedekind/sequences.py`, etc.
   - These indicate Python code is being tracked for coverage.
3. Each file shows line-by-line coverage: green for covered, red for uncovered, gray for non-executable.

**Collecting coverage locally (for contributors):**

- **C++ coverage**: `make coverage` generates `build/coverage.info` (LCOV format).
- **Python coverage**: `make python-coverage` generates `build/python-coverage.xml` (pytest-cov XML).
- Both can be run and inspected before pushing to verify coverage baseline.

**Why both C++ and Python coverage matter:**

- C++ modules define the core mathematical operations and data structures.
- Python bindings (via nanobind) provide the public API and integration points.
- Full project coverage requires both layers; Python-only or C++-only coverage leaves integration
  blind spots.

## Review workflow

- Resolve review comments and threads explicitly; do not leave unresolved conversations behind
   when preparing a PR for merge.
- Run `make pr-review-unresolved` before marking a PR ready; this target exits non-zero when unresolved
  review threads are found.
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
