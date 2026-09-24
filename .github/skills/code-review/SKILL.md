---
name: code-review
description: Review guidance for the dedekind repo. Use when performing a pull-request code review. Read the backing paper (docs/paper/paper.tex) for context and check that changes stay coherent with it, and that they reuse/extend existing abstractions rather than reinventing them (this maturing library trends toward negative net lines).
---

# dedekind code review

`dedekind` is a symbolic-algebra library that doubles as the artifact behind an
academic paper for the ‹Programming› journal. Review with two lenses: **paper
coherence** and **library coherence**. Keep findings actionable; prefer a few
high-signal comments over a long list.

## Read the paper AND the README for context first

At the start of every review, read BOTH `docs/paper/paper.tex` and `README.md`
from the source tree, before reading the diff. They are the specification the
code serves: the paper is the source of truth for the thesis, vocabulary, and
worked exhibits; the README is the reader-facing surface (its intro claims and
its code listing). Do not rely on a summary. `docs/paper/references.bib` is the
external north star for terminology and correctness. Hold all three in view (code,
paper, README) so you can flag drift between them, which is the most common
coherence miss on this repo.

Coherence checks:

- **Terminology parity.** Code and Doxygen comments should use the paper's
  vocabulary. Flag drift in either direction: concepts renamed in code that leave
  the paper stale, or comments that invent competing names for something the
  paper already names.
- **Behavior parity (code <-> paper <-> README).** If a change alters behavior
  the paper or README describes (a closure, a carrier's algebra, a collapse
  claim, a worked exhibit), call it out so all three move in step. A prose
  sentence that describes a mechanism the code does not implement is a finding,
  even when nothing is broken (e.g. a paper sentence saying complement routes
  through the reducer when it is a separate involution). Prevent divergence
  between the textbook literature, the implementation, the Doxygen, the README,
  and the LaTeX draft.
- **Listings track a verified artefact.** A code listing in the paper (`lstlisting`
  / `cppinline`) or in the README must match code that actually compiles, and the
  README says its examples mirror checked-in showcases. When a diff changes a
  demonstrated capability or the DSL surface, check that the paper listing and the
  README listing still reflect it and still correspond to a passing test. Flag a
  README/paper snippet that uses retired syntax, or that a code change has made
  stale, or whose asserted types/tiers no longer hold.

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
- **Negative net lines, and replacement over layering.** This is a maturing
  library; PRs are expected to often delete more than they add, and a
  change
  framed as "replace / unify / make X-first / simplify" should come out
  net-negative. Flag the recurring anti-pattern of adding a new abstraction
  *on top of* the one it is meant to replace while deferring the old code's
  removal to a "later slice" / follow-up: that deletion rarely lands, so the
  codebase only grows and the duplication the task meant to remove persists. The
  removal should ride in the SAME PR as the addition. Concretely: if the new
  code still CALLS the code it claims to replace (e.g. a value-level reducer that
  internally invokes the type-level one), nothing was replaced yet; say so. A
  net-positive diff on a replace/unify/simplify task wants strong justification.
- **static_assert / concept-binding is the safety boundary**, not prose. Prefer a
  compile-time witness over a comment vouching that an invariant holds. Note that
  `static_assert`s are invisible to coverage tooling.
- **Python bindings are handle-only; no second reducer, and fail fast.** The C++
  value-first / `constexpr` reducer is the single implementation across every
  phase: compile-time (`static_assert`), runtime C++, and Python via the native
  binding. A Python binding must NOT re-implement predicate structure or
  reduction. Composed predicates are C++ structures, built and reduced on the C++
  side; Python holds only a handle plus accessibility bits (`__repr__`,
  `__call__` / `__contains__`, `ext`). Flag any Python-side predicate AST,
  combinator normalisation, or lattice-law rewrite as a forbidden second reducer.
  When a capability is not yet value-first in C++, the binding must **fail fast**
  at the boundary (raise, or the operation is absent), never fall back to a
  Python re-implementation. The binding's working surface is thereby an honest
  witness of the `constexpr`-first frontier; a silent Python fallback destroys
  that signal.
- **New C++ commentary uses Doxygen blocks** per the convention documented in
  `CONTRIBUTING.md` (Alignment with the codebase → Doxygen header convention:
  brief, partition summary, copyright notice, Wikipedia-leads, and the
  practitioner-quote social-embedding line).
- **Doxygen shape: generous partition header, terse per-symbol.** Calibrate the
  block to its scope. A partition / `@section` header may be generous: it carries
  the partition summary, the Wikipedia-lead, the social-embedding quote, and the
  theory anchor. A per-symbol block stays terse. It is a short `@brief` summary of
  three sentences at most, plus the structured Doxygen fields the symbol actually
  has and Doxygen renders: `@tparam` for each template parameter, `@param` for
  each parameter, `@return`, and `@deprecated` where applicable. Fill those
  pre-configured fields rather than writing free-form prose. Flag a per-symbol
  block that pads a long `@details` essay onto one declaration, or that omits the
  `@param` / `@tparam` / `@return` fields the symbol's signature calls for.
- **Flag a leading `//` comment on any EXPORTED declaration, and flag distracting
  in-body `//` narration.** Every `export`ed function / struct / concept /
  variable, and members (operator(), operator&&, ...) of an exported type, must be
  documented with a `/** @brief ... */` Doxygen block, never a leading `//` block.
  In-body `//` comments are reserved for `// FIXME(#NNN)` breadcrumbs and the
  occasional load-bearing note: a non-obvious overload-resolution or ordering
  hazard, a correctness subtlety. They are not for narrating line by line what the
  code does. Prefer terse code and the structured Doxygen over a running `//`
  commentary. The leading-`//`-on-an-exported-declaration miss is recurring; and a
  diff that adds many explanatory in-body `//` comments reads as clutter, not
  documentation, so call that out too.
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
