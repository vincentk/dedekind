Please consider during reviews:
- the build pipeline status which is the refernce build for the project. If it passes, it means it compiles.
- the module layout indicated in the CMake configuration and the partition layouts indicated in the respective `main` partitions.
- the codecov coverage report.

As external north stars for implementation correctness and terminology go, please consider some of the literature items in the docs/paper/references.bib.

Try to prevent divergence between the textbook literature, the implementation, the doxygen inline documentation, the README and the LaTeX draft paper.

For the doxygen headers, follow the convention documented in `CONTRIBUTING.md` under `Alignment with the codebase` → `Doxygen header convention` (brief, partition summary, copyright notice, Wikipedia-leads, and the practitioner-quote social-embedding line). This is a shared contributor norm, not an agent-specific constraint — the instructions file defers to the contributor guide as the single source of truth for the convention.

For build checks, it's OK to use the CI build on a draft PR as opposed to building locally. Commits are squashed before a merge to `main` is executed so it does not have to look or perfect prior to pushing to a draft PR. Quite the contrary: the CI build is the authoritative reference build, so check the PR build status on a regular basis.

Consult `CONTRIBUTING.md` for the repository's contributor workflow before planning or delivering work. Keep the instructions file focused on agent-specific constraints; keep generally useful contributor process in `CONTRIBUTING.md`.
All agents MUST be aware of the contributor guide lines in `CONTRIBUTING.md` while working on this code base.

Session-start requirement:
- Agents SHOULD read `CONTRIBUTING.md` at the beginning of each new chat/session before planning, triage, or implementation work.
- Agents SHOULD read `README.md` at the beginning of each new chat/session and perform the severe-divergence check described below.
- Agents SHOULD read the core project documentation references at the beginning of each new chat/session: `docs/report/references.bib` and `docs/paper/references.bib`.

Execution policy for routine work in this repository:
- Operate autonomously for vanilla workloads so the user does not need to babysit the session.
- Use approved `make` targets and approved command inventory paths by default.
- PR workload protocol:
  - `Draft` protocol: leverage CI aggressively and prioritize narrowing the gap between the current implementation and the ticket acceptance criteria. Assess criteria coverage, implement the highest-impact missing criteria, add or adjust tests, and push small checkpoints without over-investing in polish.
  - `Preparing Ready` protocol: once the implementation gap is narrow, shift priority toward polish. Tighten documentation and report alignment, review clarity and naming, check test coverage quality, and reduce low-risk rough edges before crossing the Ready gate.
  - `Ready / Review` protocol: once the PR is ready or under review, prioritize reviewer response and merge readiness. Address review commentary to the best of your abilities, resolve review threads explicitly, preserve CI green status, and focus work on achieving a clean path to merge. Before final ready/merge passes, check `git stash list` and triage entries explicitly: apply+commit anything relevant to the active PR branch, and drop clearly irrelevant artifacts (for example temporary profiling outputs) so hidden local state does not linger.
- All agents SHOULD remain aware of the available repository `make` targets while working on this code base.
- At session start (or before a new workflow path), agents SHOULD load the relevant `Makefile` target context (for example via `make`/`make all` output or the documented target list).
- Agents SHOULD add `.github/copilot/housekeeping` to `${PATH}` for the current session before invoking approved helper scripts from that directory.
- Agents SHOULD avoid prefixing routine commands with temporary environment overrides (for example `TERM=...`, `FOO=... cmd`) unless explicitly requested, because such prefixes can interfere with approval flow behavior in this environment; prefer running repository `make` targets directly (for example `make pr-status`, `make pr-checks`, `make pr-watch`) since direct target invocation has proven to work reliably with approvals.
- Keep actions auditable: prefer workflows that are easy to review and report the exact commands and outcomes.
- Bias toward provably safe operation: perform read/check steps first, then minimal writes; avoid risky/destructive operations unless explicitly requested.
- If a task requires manual-approval command paths, stop using that path and add a clean helper script or `make` target instead.

RFC 2119 policy note (normative language):
- Per RFC 2119 Section 6 guidance, use imperative strength sparingly: reserve `MUST` / `MUST NOT` for hard governance and safety constraints, and prefer `SHOULD` / `SHOULD NOT` for routine workflow guidance.
- Per RFC 2119 terminology, agents SHOULD NOT compile locally as a default workflow; they SHOULD push checkpoints and rely on the CI reference build, for the reasons stated in `CONTRIBUTING.md` (authoritative build parity, reduced local toolchain drift, and faster contributor throughput).
- Per RFC 2119 terminology, agents SHOULD NOT use ad hoc scripts for routine repo operations; they SHOULD use standardized commands and repository `make` targets, for the reasons stated in `CONTRIBUTING.md` (auditability, approval clarity, and repeatability across sessions).
- Per RFC 2119 terminology, agents SHOULD use repository `make` targets or other standardized command paths for routine work so activity is auditable and can be pre-approved in this environment.
- Per RFC 2119 terminology, agents SHOULD push reasonable, reviewable changes to CI without undue delay, as stated in `CONTRIBUTING.md`, so integration feedback is timely and branch divergence stays small.
- Per RFC 2119 terminology, agents SHOULD keep implementation flow asynchronous and non-blocking where practical (small commits, immediate CI push, then continue parallel review/planning), for the reasons described in `CONTRIBUTING.md` (faster feedback loops, less idle wait time, and clearer progress checkpoints).
- Per RFC 2119 terminology, agents SHOULD treat CI-driven development as eventually consistent: push small checkpoints, continue useful parallel work while checks run, and reconcile promptly when CI feedback arrives.
- Per RFC 2119 terminology, while waiting between CI builds agents SHOULD use that time to check documentation consistency (report/paper/readme/doxygen), validate test coverage, groom the backlog, or review the Python shim (`python/dedekind/dsl.py`) for complexity that could be pushed down into the C++ layer — particularly any logic that duplicates or approximates operations already available in `dedekind.sets` (e.g. `set_union`, `set_intersection`, `set_difference`) or planned for `relational.cppm`, and open issues or FIXMEs that should become first-class C++ bindings exposed via `nanobind.cpp`, for the reasons described in `CONTRIBUTING.md` (reduced drift, earlier quality-signal detection, and better review quality).
- Per RFC 2119 terminology, a PR MUST pass pull request review before merge; agents MUST wait for a human review signal (approval or explicit owner instruction) prior to invoking merge operations.
- Per RFC 2119 terminology, agents MUST NOT attempt to push to protected branches (including `main`); all updates MUST flow through a feature branch and PR.
**Backlog grooming completed on April 19, 2026:**
- ✅ Main build verified green (3 recent CI runs all success)
- ✅ README checked for severe divergence (none found)
- ✅ 50+ issues triaged, organized by priority
- ✅ PR #341 (Quick-Win Gaps & Bidirectional Backlog Links) marked READY for review
- ✅ Architectural gaps prioritized: Derivative Abstraction (HIGH) → Forms Integration (MEDIUM-HIGH)
- ✅ Parallel work streams identified: Derivative unification + Epic #286 Phase 1 (partial fractions)

Before backlog triage, agents SHOULD run the following checks (typically at session start and after merges to `main`):

1. **Verify the `main` build is green.** Check `gh run list --branch main --limit 3` and confirm the most recent CI run passed. If it is failing, address that before any new work.

  If the latest `main` run is failing, pause unrelated backlog grooming and prioritize stabilization work first.

2. **Scan the README for severe divergence.** The README is the entry point to the project and should remain small and stable. Open it and check only for content that is plainly wrong or deeply misleading given the current state of the code base. If an update is needed, keep it minimal — a sentence or two at most. Do not add new sections or expand existing ones; routine progress is captured in the report and doxygen, not the README.

Then triage the backlog:
- Prefer plain `gh` CLI invocations (for example `gh pr checks <number>`).
- When available, prefer repository `make` wrappers for CI/PR status checks:
  `make ci-main`, `make pr-status`, `make pr-checks`, `make pr-watch`, `make pr-sync`.
- Commands in this environment may require explicit approval, so prefer standardized command-line utilities (especially `gh`, `git`, `cmake`, `ctest`, `make`) over free-flow Python or shell scripts for automation work; this keeps command intent auditable and easier to approve.
- If a housekeeping utility is still needed beyond the standardized commands and existing `make` targets, request it interactively first. If approved, place it under `.github/copilot/housekeeping` rather than inlining an ad-hoc script in the session.
- If such a utility is required, propose it as a reusable checked-in implementation with a clear interface and narrow purpose, so review noise stays manageable and future sessions can reuse the same approved path.
- Do not prepend `GH_PAGER=cat` (or similar env overrides) unless explicitly requested, because it can trigger repetitive local approval prompts in this environment.
- Prefer selecting exactly two actionable items to work on in parallel when feasible (for example, while CI builds/reviews are pending), to keep throughput high without over-fragmenting scope.
- Run `gh issue list --state open --limit 50 --json number,title,body,labels,comments` to fetch all open issues.
- Also run `gh pr list --state open --json number,title,body,headRepositoryOwner --jq '.[] | select(.headRepositoryOwner.login != "vincentk")'` to find open PRs from forks. Treat each fork PR like an issue: review whether it is actionable, leave a review comment if clarification is needed, and incorporate it into the work plan for the current session if it is ready.
- Apply triage in this order for determinism: CI health check, issue list, fork-PR list, per-item classification, then selection of the next focused work batch.
- For each issue, assess whether it is actionable with good confidence:
  - **Actionable**: scope is clear, key concepts are identifiable, no blocking ambiguity. Prefer selecting exactly 2 of these for the next PR when feasible.
  - **Ambiguous or unclear**: post a clarifying comment (`gh issue comment <number> --body "..."`) describing exactly what is missing, and wait for the owner to update the issue before picking it up.
  - **Already resolved**: close with `gh issue close <number> --comment "..."` citing the PR that addressed it.
  - **Exact duplicate**: keep one canonical issue open and close duplicate issues with a cross-reference comment.
- Name the branch after the selected issue numbers, e.g. `feat/issues-40-121`.
- Use `make pr-init ISSUES="40 121"` to initialize the branch, empty checkpoint commit, remote push, and draft PR. The script (`.github/copilot/housekeeping/pr-init.sh`) fetches each issue's title and body via `gh issue view` and uses them to populate the PR title and description automatically; no stub titles or placeholder bodies are needed.

**Never push directly to `main`.** All work must go through a feature branch and a PR. Direct pushes to `main` are not permitted.
**PRs MUST pass review before merge.** Do not merge solely on CI green; wait for review or explicit owner instruction.

Follow the branch / PR / review workflow described in `CONTRIBUTING.md`, then apply the additional agent-specific review-thread handling above when addressing PR feedback.
