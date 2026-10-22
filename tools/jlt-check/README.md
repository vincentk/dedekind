# jlt-check

A checker for the Jlt fragment (#1060): the three rules of the Jlt paper
(`docs/paper/jlt/jlt.tex`, Section 2), turned from a reading into a verdict.

It runs **after** the build: it reparses each file in scope through
`build/compile_commands.json`, which carries the module maps CMake wrote, so
the prebuilt module files must exist.

## Layout

- `rules/<rule>.query`: one `clang-query` matcher per check, named for the rule
  of the paper it checks.  A match is a finding.
- `scope`: the gate, one line per `<rule> <file> [expected]`.  Growing the
  checker is adding a rule file and a line here.
- `fixtures/`: one negative control per rule, standalone (no imports), with
  its expected finding count in `scope`; it proves the matcher fires, so a
  zero on a real file is not vacuous.
- `jlt-check.sh`: runs every pair in `scope`, prints the findings, and fails if
  there are any, or if a file does not parse.

Run it with `make jlt-check` after `make compile`.

## Plan

1. A trivial gate as a post-processing step in CI (this).
2. More rules and more files, line by line in `scope`.
3. A clang-tidy plugin when a rule needs code a matcher cannot express (Rule 3's
   proper-subterm check on self-calls); `scope` stays the switchboard.
