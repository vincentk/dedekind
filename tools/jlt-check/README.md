# jlt-check

A checker for the Jlt fragment (#1060): the three rules of the Jlt paper
(`docs/paper/jlt/jlt.tex`, Section 2), turned from a reading into a verdict.

It runs **after** the build: it reparses each file in scope through
`build/compile_commands.json`, which carries the module maps CMake wrote, so
the prebuilt module files must exist.

## Layout

- `rules/<rule>.query`: one `clang-query` matcher per check, named for the rule
  of the paper it checks.  A match is a finding.
- `scope`: one line per `<rule> <file-glob> <namespace> <expected|report>`.
  The rule matches beneath the namespace; a number is a gate, `report` is
  counted and printed, never failed.  Growing the checker is adding a rule file
  and a line here, and turning `report` into `0` when a file is clean.
- `fixtures/`: one negative control per rule, standalone (no imports), with
  its expected finding count in `scope`; it proves the matcher fires, so a
  zero on a real file is not vacuous.
- `jlt_check.py`: parses each file once with all its rules, prints gate
  results, reported counts and a coverage table per rule, and fails on a gate
  mismatch, or if a file does not parse or a matcher does not run.

Run it with `make jlt-check` after `make compile`.

## Plan

1. A trivial gate as a post-processing step in CI (this).
2. More rules and more files, line by line in `scope`.
3. A clang-tidy plugin when a rule needs code a matcher cannot express: a
   `mutable` member (clang-query has no matcher for it), Rule 3's
   proper-subterm check on self-calls; `scope` stays the switchboard.
