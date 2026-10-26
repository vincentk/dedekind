# jlt-check

A checker for the Jlt fragment (#1060): the three rules of the Jlt paper
(`docs/paper/jlt/jlt.tex`, Section 2), turned from a reading into a verdict.

It runs **after** the build: it reparses each file in scope through
`build/compile_commands.json`, which carries the module maps CMake wrote, so
the prebuilt module files must exist.

## Layout

- `rules/<rule>.query`: one `clang-query` matcher per check, named for the rule
  of the paper it checks.  A match is a finding.
- A rule file may name its own traversal (`set traversal AsIs`) when it needs
  implicit nodes; the driver restores the default after it.
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

## The tool

`tool/` is `jlt-tool`, a small LibTooling binary for the checks that need code
rather than a pattern.  `make jlt-tool` builds it against the LLVM that built
the tree (`JLT_TOOL_CMAKE_ARGS` names its CMake package dirs; CI installs
`libclang-23-dev` and `libclang-cpp23-dev` for them), and `make jlt-check`
runs it beside the clang-query rules on the same compilation database.  A
rule file names one of its checks with `tool <check>` instead of match lines;
the tool prints one finding per line as `path:line:col`, and the driver counts
them like any other rule.  Checks so far: `mutual-recursion` (functions of the
scope on a call cycle of length two or more, through clang's call graph) and
`definition` (Rule 2 at the level of names; what it does not see: class
templates' ordinary members, lambdas, operations reached through a
non-dependent helper, arity and types of a named operation; what it grants by
heuristic: a bool-returning constexpr template as a trait read), `inline-requires` (an `if`, constexpr or not, whose condition contains a
requires-expression) and `inheritance` (a base that is not an empty class, or
a virtual function; a dependent base judged by its template's primary).

## Rules

Each rule is one file under `rules/`, named for the rule of the paper it checks.

| rule | finding |
|---|---|
| `rule1-constexpr` | a function definition neither `constexpr` nor `consteval` |
| `rule1-const-call-operator` | an `operator()` that is not `const` |
| `rule1-no-throw` | a `throw` |
| `rule1-no-static-locals` | a static local that is not `constexpr` |
| `rule1-lvalue-reference-params` | a parameter by lvalue reference to a non-`const` type: the mutation channel |
| `rule1-rvalue-reference-params` | a parameter by rvalue reference to a non-`const` type that is not forwarding (a concrete type, or a class template's parameter): a sink |
| `rule1-forwarding-reference-params` | a parameter `T&&` with `T` a type parameter of the function template, or `auto&&` |
| `rule1-no-volatile` | any object, reference or pointer to `volatile`: never a value |
| `rule1-no-reference-returns` | a function returning a reference (including `const auto&`) |
| `rule1-const-member-functions` | a non-`static` member function that is not `const` |
| `rule1-no-mutable-globals` | a namespace-scope or static data member variable that is neither `const` nor `constexpr` |
| `rule1-no-escaping-reference-captures` | a lambda that captures by reference and escapes: inside a `return`, not under a call into `std`, not called on the spot (an approximation) |
| `rule1-boxed-mutable-pointee` | a `make_shared<T>` call or `shared_ptr<T>` member (in a template too) with `T` not `const` |
| `rule1-boxed-identity` | `get()`, `use_count()`, `owner_before()`, or `==` between two `shared_ptr`s |
| `rule3-no-loops` | a `for`, range-`for`, `while` or `do` (except `iterate`'s one loop) |
| `rule3-no-self-recursion` | a function calling itself at the same instantiation: recursion on a value; recursion over a type's structure (`cata`) passes |
| `rule3-no-mutual-recursion` | functions on a call cycle of length two or more (the tool's `mutual-recursion` check) |
| `rule2-constrained-templates` | a function template with an unconstrained type parameter and no `requires` clause: a **proxy** for Rule 2, reported only (a parameter the body never uses needs no constraint) |
| `rule2-definition` | the tool's `definition` check, Rule 2 at the level of names: an operation on a value of parameter type that no *declared* constraint names (the type constraints, the `requires` clauses, the enclosing class templates', followed through the library's concepts; standard concepts and traits from a hand-made list; a concept's written arguments count). Not vocabulary of the value: standard functions, non-template functions, the library's constrained templates, `constexpr` templates returning `bool` (traits in function spelling), the control operators. An inline `requires` or concept test in the body is **not** a constraint. Silent where it cannot read a constraint |
| `rule2-no-inline-requires` | the tool's `inline-requires` check: an `if`, constexpr or not, whose condition contains a requires-expression, a constraint tested in the body instead of declared (#1068); the fix is a named shape |
| `rule2-decided-resolution` | the tool's `undecided-resolution` check, GHC's rule: a call at a value of parameter type to an overload set separated by a concept the enclosing constraints neither require nor negate. Not seen: trait reads through class and variable templates (specialisations differ by pattern), calls through a helper. Reported only until the count on `category` is known |
| `rule1-no-inheritance` | the tool's `inheritance` check: no subtyping. A class with a base that is not an empty class, or with a virtual function; an empty base (`: std::true_type`, `: is_x<Y>`) is a registration spelled by inheritance |

Exempt throughout: deleted and defaulted functions, lambdas, and a static
`operator()`.  The standard iterator protocol (a nested `iterator` or
`sentinel`) is exempt from the reference-return and const-member rules: the
folds of Rule 3 run on it.  **The boxed value:** `std::shared_ptr<const T>` is a Jlt value
(semiregular, immutable, cheap to copy; the heap cell a recursive structure
needs) so long as nothing observes which box it is; it exists at run time
only, since `shared_ptr` is not `constexpr` in C++23.

**Local mutation** is allowed: a function may change its own locals,
because nothing it changes outlives the call.  The rules above close the
channels a mutation could leak through (parameters, results, the object,
static locals, globals, volatile, boxes, and escaping reference captures,
the last by an approximation of escape).

**Known blind spots** (plugin work): a `mutable` member has no matcher; a
`make_shared` whose argument depends on a template parameter is unresolved in
the template pattern; a `decltype(auto)` return in a template pattern is
undeduced; a lambda with a reference capture that escapes other than by a
return (stored into a member or global), and a returned one that a non-std
function calls at once (both need dataflow); a template never instantiated in the file being checked; a function
naming itself inside a requires-expression (reported, though not recursion).

## Plan

1. A trivial gate as a post-processing step in CI (this).
2. More rules and more files, line by line in `scope`.
3. A clang-tidy plugin when a rule needs code a matcher cannot express: a
   the blind spots above; `scope` stays the switchboard.
