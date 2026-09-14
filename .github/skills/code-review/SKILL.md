---
name: code-review
description: Review guidance for the dedekind repo. Use when performing a pull-request code review. Read the backing paper (docs/paper/paper.tex) for context and check that changes stay coherent with it, and that they reuse/extend existing abstractions rather than reinventing them (this maturing library trends toward negative net lines).
---

# dedekind code review

`dedekind` is a symbolic-algebra library that doubles as the artifact behind an
academic paper for the ‹Programming› journal. Review with two lenses: **paper
coherence** and **library coherence**. Keep findings actionable; prefer a few
high-signal comments over a long list.

## Read the paper for context

Read `docs/paper/paper.tex` from the source tree at the start of a review — it is
the specification the code serves. Do not rely on a summary; the paper is the
source of truth for the project's thesis, vocabulary, and worked exhibits.
`docs/paper/references.bib` is the external north star for terminology and
correctness.

Coherence checks:

- **Terminology parity.** Code and Doxygen comments should use the paper's
  vocabulary. Flag drift in either direction: concepts renamed in code that leave
  the paper stale, or comments that invent competing names for something the
  paper already names.
- **Behavior parity.** If a change alters behavior the paper describes (a
  closure, a carrier's algebra, a collapse claim, a worked exhibit), call it out
  so the paper, README, and Doxygen get updated in step. Prevent divergence
  between the textbook literature, the implementation, the Doxygen inline
  documentation, the README, and the LaTeX draft.

## Library coherence

- **Reuse over reinvention.** Before new surface lands, a concept-search should
  have happened: does an existing concept/trait already cover this? Flag ad-hoc
  reimplementation of upstream abstractions; prefer extending them.
- **Prefer algebraic gates over architectural ones.** Constraints should be
  expressed as concepts (`IsGroup`, `IsField`, …), not bespoke plumbing.
- **Burden of proof is on ADDING a struct/wrapper**, not on removing one. Push
  back on new wrappers that don't earn their place — especially thin "glorified
  for-loop" wrappers around `std::` containers/algorithms.
- **Trend toward negative net lines.** This is a maturing library; PRs are
  expected to often delete more than they add. A large net-positive diff wants
  justification.
- **static_assert / concept-binding is the safety boundary**, not prose. Prefer a
  compile-time witness over a comment vouching that an invariant holds. Note that
  `static_assert`s are invisible to coverage tooling.
- **New C++ commentary uses Doxygen blocks** per the convention documented in
  `CONTRIBUTING.md` (Alignment with the codebase → Doxygen header convention:
  brief, partition summary, copyright notice, Wikipedia-leads, and the
  practitioner-quote social-embedding line).

## Do NOT flag

- **Verbose type-level markers are intentional.** Restrictive-over-ergonomic is
  the design; verbose compile-time markers are the accepted price of compile-time
  collapse. Do not report them as verbosity or boilerplate.
- **Draft-PR polish.** Commits are squashed before merge; on a draft PR, focus on
  acceptance-criteria coverage, not commit hygiene or transient roughness.

## Build & correctness signals

- The **CI build is authoritative** — if the PR's CI is green, it compiles. Don't
  ask for local builds as evidence; the CI build is the reference build, so check
  the PR build status on a regular basis.
- Consider the module/partition layout (the CMake configuration and the
  respective `main` partitions) and the Codecov coverage report.
- Never approve pushing directly to `main`; all work flows through a PR.

See `CONTRIBUTING.md` for the contributor workflow and conventions.
