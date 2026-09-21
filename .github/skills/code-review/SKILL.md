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
- **Ground new structure in the basic categorical constructions, and inherit
  axioms from upstream.** A downstream type should reuse an upstream construction
  (product, coproduct, subobject, involution/dagger, pullback, the logic species,
  ...) rather than re-derive its laws locally. The Juliet posture (structural
  augmentation) is the mechanism: take the upstream type, add the extra structure
  the downstream needs *selectively*, then **witness and assert** that structure
  downstream (a `static_assert` binding the added law to the upstream concept).
  When a fix restores a law (an involution `f∘f = id`, an idempotent `a∧a = a`, a
  De Morgan dual), check whether the law already lives upstream and can be
  *inherited* (a downstream gate consulting an upstream witness) before
  accepting a local, hand-rolled fix that re-states the law. Flag a downstream
  quick-fix when the proper fix derives from an upstream construction or axiom.
- **Prefer concept gates over bespoke plumbing.** Express a constraint as a
  concept — algebraic (`IsGroup`, `IsField`) or categorical (`IsRegularEpi`,
  `IsRegularMono`, `IsImageOf`, the paper's epi–monic factorization) — rather
  than ad-hoc nominal plumbing. Categorical/architectural gates the paper
  prescribes are the *right* kind of gate; flag only hand-rolled structural
  checks that an existing concept already captures.
- **Template parameters should carry a constraint that establishes intent.** A
  new `template <typename T>` on an exported function or type wants at least a
  `requires` clause or a constrained-`auto` / concept-typed parameter that says
  what `T` is meant to be (`IsSet`, `IsArrow`, `IsPredicate`, ...). The bar is
  intent, not exhaustive rigor: a single well-chosen concept beats a bare
  `typename`, and a bare `typename` is fine only when the parameter is genuinely
  any type (a passthrough wrapper, a perfect-forwarding sink). Flag a new
  unconstrained parameter whose body clearly assumes a shape; do not demand
  maximal strictness where a light concept already pins the intent.
- **Burden of proof is on ADDING a struct/wrapper**, not on removing one. Push
  back on new wrappers that don't earn their place — especially thin "glorified
  for-loop" wrappers around `std::` containers/algorithms.
- **A deprecated pattern with a future-proof alternative wants the alternative,
  not the deprecation.** When a diff introduces or leans on something already
  flagged deprecated (a `[[deprecated]]` API, a legacy representation the PR is
  retiring, a shim kept only for callers), and a forward-looking replacement
  exists, request that the code adopt the replacement rather than perpetuate the
  deprecated form. Deprecation markers are a migration signal, not a resting
  place; prefer completing the migration in the PR that touches the call site.
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
- **Flag a leading `//` comment on any EXPORTED declaration.** Every `export`ed
  function / struct / concept / variable, and members (operator(), operator&&,
  ...) of an exported type, must be documented with a `/** @brief ... */`
  Doxygen block, never a leading `//` block. A `//` is only for in-body notes and
  `// FIXME(#NNN)` breadcrumbs. This is a recurring miss; call it out when the
  diff introduces a `//` header above an exported declaration.
- **Prefer short, descriptive sentences over long, snaky ones** in all new prose
  (Doxygen, paper text, PR descriptions). One clause, one point: subject, verb,
  object. Subject, verb, object. A sentence that chains three or more clauses
  with dashes, semicolons, and parentheticals should be split into several. This
  is the general rule; the em-dash below is its most common symptom.
- **Flag em-dashes** (`—`, U+2014) in new prose. An em-dash almost always joins
  clauses that should be separate sentences, and it reads as an AI-slop tell. Ask
  for the sentence to be split; where a genuine break remains, a colon,
  semicolon, parenthesis, or hyphen serves. Call out any em-dash a diff
  introduces.

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
