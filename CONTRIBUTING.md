# Contributing to Dedekind

Thank you for your interest in contributing!

## How to contribute

1. **Fork** the repository on GitHub and clone your fork locally.
2. **Pick an issue** from the [issue tracker](https://github.com/vincentk/dedekind/issues).
   If you have a new idea, open an issue first so scope and direction can be agreed on before
   you invest time in an implementation.
3. **Create a branch** named after the issue(s) you are addressing, e.g. `feat/issues-40-121`
   (`make pr-init ISSUES="40 121"` does this and opens the draft PR).
4. **Open a draft PR** against `main` early, and push checkpoints freely.
   CI rejects unformatted code; `make install-hooks` catches that before the push.
5. **Let CI build it.** See [CI is the reference](#ci-is-the-reference).
6. **Mark the PR Ready for Review** when two things hold: you judge it ready, and all CI checks are green.
   CI can't tell whether the [paper](https://vincentk.github.io/dedekind/jlt.pdf) and [docs](https://vincentk.github.io/dedekind/) still hold; that part is yours.

While a PR is a draft, close its acceptance criteria checkpoint by checkpoint.
Near Ready, polish: clarity, documentation, low-risk refinements and open review threads.

Run `make help` for the build and workflow targets.

## CI is the reference

[CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml) runs the build, tests, coverage, linter, docs and papers through the same `make` targets you have locally.
Its checks decide merge readiness, so `git push` is the only command you strictly need.
A local `make compile` or `make test` is for failures you cannot diagnose from the CI logs.
It expects its tools installed: the build, LLVM's clang 22 or later with CMake and Ninja; `make doxygen`, Doxygen and Graphviz; `make papers`, the TeX, minted and font that [CI installs](.github/actions/paper-toolchain/action.yml), which on macOS MacTeX provides but for the font.
This also guards against "works on my laptop": CI is the reproducible baseline.

The workflow is deliberately optimistic: ship small checkpoints, let CI validate them in parallel, and converge on its feedback.
If CI is red, on your PR or on `main`, fixing it comes before new work.

## The Juliet Posture

The library is written in a fragment of C++23, the Juliet Posture (Jlt), under three rules:

| Rule | Statement |
|---|---|
| 1, values and pure functions | Every type passed around is `std::semiregular`, and every function is `constexpr` and depends on its arguments alone. |
| 2, behaviour through concepts and traits | Generic code does with a `T` only what `T`'s declared constraints say it can. |
| 3, recursion only folds | Recursion runs over a term, over a finite chain, or over an explicit budget. |

The compiler holds parts of these, the [linter](tools/jlt-lint/README.md) more, and [review](.github/skills/jlt-review/SKILL.md) the rest; the [paper](https://vincentk.github.io/dedekind/jlt.pdf) states them in full.
A fix is almost always a constraint stating what the body already does, not a cast or a workaround.

For modeling decisions, prefer abstract/intensional carriers and symbolic types by default; choose a concrete runtime numeric representation only at realization points where concrete evaluation is intentionally required.

[In Python](docs/python/README.md), the C++ core owns the mathematics and Python holds handles to it.

## Operating principles

Two rules of thumb shape how PRs are prepared and shipped in this repo.
They interact: the second justifies the first.
Together they apply to PR content what CI applies to builds: the backlog need only converge, not be consistent at every moment, and conflicts are reconciled when they appear rather than locked against in advance.

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

Foundational axioms reside upstream; theorems and derived constructions appear
downstream, in the same order a textbook would present them.

## Alignment with the codebase

- Follow the module/partition layout described in [`CMakeLists.txt`](CMakeLists.txt) and the existing
  `main` partitions.
- Mirror the doxygen header style used in neighbouring files — see
  `Doxygen header convention` below for the full shape.
- Keep concepts and naming aligned with the textbook literature cited in
  [`docs/paper/references.bib`](docs/paper/references.bib).  UTF-8 mathematical symbols (ℤ, ℝ, …) are preferred
  over verbose ASCII alternatives.
- Add or extend tests in the corresponding `src/test/cpp/modules/…` file.

### Doxygen header convention

Every `src/main/modules/dedekind/**/*.cppm` file SHOULD open with a doxygen
comment block whose elements, in order, are:

1. `@file` — path-qualified, matching the actual file location.
2. `@partition :NAME` — when the file defines a module partition; omit for
   umbrella / aggregator files.
3. `@brief` — one line, sentence-case, terminating in a period.
4. `@copyright 2026 The Dedekind Authors / Licensed under the Apache License,
   Version 2.0.`
5. One or more `@section` blocks summarising the partition's contents and
   its mathematical intent. Cross-reference the textbook literature in
   `docs/paper/references.bib` when a concept has a canonical attested source.
6. A `Wikipedia: ...` line enumerating two or three articles a reader might
   consult to place the partition in the broader literature.
7. A `@note` containing a **pertinent or serendipitous quote from a not
   very famous practitioner in the field**, in the practitioner's native
   language, with an English translation under `[Trans: ...]` on a
   subsequent line. Attribution SHOULD include author, work, year, and
   section / page where available. "Not very famous" means: avoid the
   obvious top-five names of the century (Einstein, Euler, Gauss, von
   Neumann, …); prefer a working mathematician / computer scientist /
   logician whose contribution is recognised within the subfield.

The practitioner-quote line is a social-embedding convention: it weaves the
code into the broader mathematical tradition and signals the care taken in
the work's intellectual lineage. New files SHOULD adopt it; existing files
missing it SHOULD acquire one as part of the touching PR (boy-scouting
applies).

## Review

- A PR gets a Copilot review when it is marked Ready; after pushing follow-up commits, re-request review so the latest state is reviewed.
- Resolve review threads explicitly, each with a reason (`make pr-resolve-thread`).
- For low-risk mechanical fixes, batching routine changes is acceptable; for semantic or API changes, prefer manual edits so the trade-offs remain explicit.

## Conventions

- Use [RFC 2119](https://www.rfc-editor.org/rfc/rfc2119) terms proportionally: `MUST` for hard governance or safety constraints, `SHOULD` for routine guidance.
- Keep the [README](README.md) small and stable; routine progress belongs in the [paper](https://vincentk.github.io/dedekind/jlt.pdf) and the [C++](https://vincentk.github.io/dedekind/) and [Python](https://vincentk.github.io/dedekind/python/) references.

## Questions

Open an issue or leave a comment on an existing one — contributions of any size are
welcome.
