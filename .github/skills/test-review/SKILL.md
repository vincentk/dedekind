---
name: test-review
description: Review guidance for test code in the dedekind repo (src/test/**). Use when a pull request touches tests, or when reviewing a suite for redundancy. Every test must serve one of a few named purposes; a test that serves none, restates a witness the module already pins, or tests retired behaviour is a finding, and the recommended action is removal.
---

# dedekind test review

Tests in this repo are part of the deliverable, not padding. The library trends
toward negative net lines, and the suites should too: a test earns its place by
the question it answers, and a test whose question no longer exists is dead
weight that slows CI and misleads the next reader. Prefer a few high-signal
findings over a long list.

## Every test names its pattern

Each `TEST_CASE` (or standalone `static_assert` block in a test file) should be
recognisable as exactly one of these. If you cannot say which, that is the
finding.

| pattern | what it checks | where its subject lives |
|---|---|---|
| **unit** | the exported surface of ONE partition: a concept fires or fails on the partition's own types, an operation on the partition's own values gives the textbook answer | the partition under test |
| **integration** | the interaction between the partition and functionality it IMPORTS: a downstream type satisfies an upstream concept, an upstream combinator reduces a downstream leaf, ADL finds the leg the downstream module provides | two layers, upstream of the test target (tests import same-layer or upstream only; never a module downstream of the partition under test) |
| **witness / exhibit** | a claim the paper or README makes, mechanically: an IR fixture folds to a constant, a listing compiles and gives the stated answer, a showcase collapses | `src/test/.../python/showcase_*`, the `.ll` fixtures, README-mirrored listings |
| **regression** | one past bug, pinned at its minimal reproduction, with the issue number | wherever the bug was |
| **coverage companion** | a runtime `CHECK` that exercises a body a `static_assert` already proves, because `static_assert`s are invisible to Codecov | next to the static witness it mirrors, kept to the minimum that runs the body |

## Recommend removal when

- **It restates the module.** A test that asserts a type identity, a species, a
  cardinality class, or a concept fact the module already pins with its own
  `static_assert` at the definition site adds nothing. The module is the single
  source of that truth; the test is a second copy that drifts. (Example: a
  `same_as<decltype(ℝ), 𝔸<…>>` check in a test when `real.cppm` asserts it.)
- **It tests retired behaviour.** The subject is a deleted type, a deleted
  deduction guide, a wrapper, a tag, a species re-tag that no longer happens, or
  a law that only held under an earlier representation. Look for `pre-#NNN`,
  `post-#NNN`, "CTAD", "guide", "wrapper", "migration", "legacy" in the comments:
  they mark history, and history belongs in git, not in a running test.
- **Its assertions are all comments.** A `SECTION` or `TEST_CASE` whose body is
  commented-out `REQUIRE`s under a `FIXME(#NNN)` is a to-do, not a test. The
  issue tracks the to-do; the empty section should go.
- **It asserts a law on a carrier that does not satisfy it.** Excluded middle or
  non-contradiction on a Kleene universe, totality on `double`, commutativity on
  floating-point `+`. Passing by evaluating at a convenient point is not
  evidence; the test is wrong, not the library.
- **It duplicates a sibling.** Two tests (often in two files) exercising the
  same operation on the same shape with different variable names. Keep the one
  in the partition's own suite.
- **It has no observable.** A `TEST_CASE` with only `CHECK(true)` or only
  `STATIC_CHECK`s of tautologies, kept "for coverage" of nothing.

## Do NOT flag

- **Coverage companions with a purpose.** A single runtime `CHECK` beside a
  `static_assert` is the repo's answer to Codecov blindness; keep it when it runs
  a body, remove it only if the body it would run is gone.
- **Verbose type-level witnesses.** A `STATIC_CHECK` that spells out a long type
  is the accepted price of compile-time collapse; the question is whether the
  claim is live, not whether it is verbose.
- **Witnesses placed downstream on purpose.** A concept-conformance check for an
  `order` type lives in `order`'s tests, not in `sets`'s, because `sets` cannot
  import `order`; do not ask for it to move upstream.

## How to report

- Name the pattern you think the test *should* be, or say that none fits.
- Quote the module-level `static_assert` that already covers it, when that is
  the reason.
- Recommend removal plainly; do not propose rewriting a legacy test into a new
  one unless the new question is one the suite lacks.
- When a PR deletes tests, check that each deletion matches one of the reasons
  above and say so; deleting a live unit or integration test is a finding.

See `.github/skills/code-review/SKILL.md` for the library-side review guidance
and `CONTRIBUTING.md` for the module DAG rule tests must respect.
