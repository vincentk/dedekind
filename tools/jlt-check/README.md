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

## Rules

Each rule is one file under `rules/`, named for the rule of the paper it checks.

| rule | finding |
|---|---|
| `rule1-constexpr` | a function definition neither `constexpr` nor `consteval` |
| `rule1-const-call-operator` | an `operator()` that is not `const` |
| `rule1-no-throw` | a `throw` |
| `rule1-no-static-locals` | a static local that is not `constexpr` |
| `rule1-lvalue-reference-params` | a parameter by lvalue reference to a non-`const` type: the mutation channel |
| `rule1-rvalue-reference-params` | a parameter by rvalue reference to a concrete non-`const` type: a sink |
| `rule1-forwarding-reference-params` | a parameter `T&&` / `auto&&` on a template type parameter |
| `rule1-boxed-mutable-pointee` | a `make_shared<T>` call or `shared_ptr<T>` member with `T` not `const` |
| `rule1-boxed-identity` | `get()`, `use_count()`, `owner_before()`, or `==` between two `shared_ptr`s |
| `rule3-no-loops` | a `for`, range-`for`, `while` or `do` |

Exempt throughout: deleted and defaulted functions, lambdas, and a static
`operator()`.  **The boxed value:** `std::shared_ptr<const T>` is a Jlt value
(semiregular, immutable, cheap to copy; the heap cell a recursive structure
needs) so long as nothing observes which box it is; it exists at run time
only, since `shared_ptr` is not `constexpr` in C++23.

**Known blind spots** (plugin work): a `mutable` member has no matcher; a
`make_shared` whose argument depends on a template parameter is unresolved in
the template pattern; Rule 3's proper-subterm check on self-calls.

## Plan

1. A trivial gate as a post-processing step in CI (this).
2. More rules and more files, line by line in `scope`.
3. A clang-tidy plugin when a rule needs code a matcher cannot express: a
   the blind spots above; `scope` stays the switchboard.
