# jlt-lint

A linter for the Jlt fragment (#1060): the three rules of the Jlt paper
(`docs/paper/jlt/jlt.tex`, Section 2), turned from a reading into a verdict.

It runs **after** the build: it reparses each file in scope through
`build/compile_commands.json`, which carries the module maps CMake wrote, so
the prebuilt module files must exist.  The build owns it: `lint` (and so
`make check` and `make jlt-lint`) lints the whole scope in one pass once
everything is built, one pool of `NCPU` workers over every file, and writes
`build/jlt-lint/report.txt` when it passes.  Its findings are kept in
`build/jlt-lint/parts/all.txt` (and `all.json`) either way, so a failed run's
findings are read there.  `lint-<layer>` lints one layer's lines of `scope`
once that layer and its tests are built, and `lint-fixtures` the fixtures,
for a look at one part; each writes `parts/<part>.txt`.

## Layout

- `rules/<rule>.query`: one `clang-query` matcher per check, named for the rule
  of the paper it checks.  A match is a finding.  The tool parses them with
  clang's dynamic matcher parser, as `clang-query` does, and runs them on the
  parse its own checks read.
- A rule file may name its own traversal (`set traversal AsIs`) when it needs
  implicit nodes; the others match the code as written
  (`IgnoreUnlessSpelledInSource`).
- `scope`: one line per `<rule> <file-glob> <namespace> <expected|report>`.
  The rule matches beneath the namespace; a number is a gate, `report` is
  counted and printed, never failed.  Growing the linter is adding a rule file
  and a line here, and turning `report` into `0` when a file is clean.
- `fixtures/`: one negative control per rule, standalone (no imports), with
  its expected finding count in `scope`; it proves the matcher fires, so a
  zero on a real file is not vacuous.
- A listing's test file keeps the listing in a namespace of its own and the
  test cases outside it: Catch2's macros expand to loops and non-`constexpr`
  functions, which the rules would count against the listing.
- `jlt_lint.py`: parses each file once, by the tool, with all its rules
  (`--query=<rule files>` and `--check=a,b,...`, each rule's findings in its
  own section), prints gate
  results, reported counts and a coverage table per rule, and fails on a gate
  mismatch, or if a file does not parse or a matcher does not run.  Files run
  in parallel, `NCPU` at a time (default: the number of cores); the report is
  in file order.  `--part` lints one layer (or `fixtures`), `--join` makes
  the whole report from the parts and fails if a part of `scope` did not run.

Run it with `make jlt-lint`, or with the tests as `make check`.

## The tool

`tool/` is `jlt-lint`, a small LibTooling binary that runs every rule of a
file over one parse: the rule files' matchers, and its checks, the rules that
need code rather than a pattern.  Each parse is a whole front end, constant
evaluation of every `static_assert` included, so a file is parsed once.  The
build makes it (`jlt-lint-tool`) against the LLVM
that compiles the tree, whose CMake packages it finds next to
the compiler (`-DClang_DIR`, `-DLLVM_DIR` name them
otherwise; CI installs `libclang-23-dev` and `libclang-cpp23-dev` for them;
without them `lint` fails and says so, and `-DDEDEKIND_JLT_LINT=OFF` drops it).  A
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
| `rule3-no-loops` | a `for`, range-`for`, `while` or `do` |
| `rule3-no-self-recursion` | a function calling itself at the same instantiation: recursion on a value, which the kernel writes as a bounded fold, however visibly it descends; recursion over a type's structure (instantiation, as `cata` and a template on a smaller count) passes |
| `rule3-no-mutual-recursion` | functions on a call cycle of length two or more (the tool's `mutual-recursion` check) |
| `rule3-bounded-fold` | a standard fold (`std::ranges::fold_left`, `fold_right`, …) outside a function whose range parameter is `IsBoundedRange` (#1172; the tool's `bounded-fold` check) |
| `rule2-constrained-templates` | a function template with an unconstrained type parameter and no `requires` clause: a **proxy** for Rule 2, reported only (a parameter the body never uses needs no constraint) |
| `rule2-definition` | the tool's `definition` check, Rule 2 at the level of names: an operation on a value of parameter type that no *declared* constraint names (the type constraints, the `requires` clauses, the enclosing class templates', followed through the library's concepts; standard concepts and traits from a hand-made list; a concept's written arguments count). Not vocabulary of the value: standard functions, non-template functions, the library's constrained templates, `constexpr` templates returning `bool` (traits in function spelling), the control operators. An inline `requires` or concept test in the body is **not** a constraint. A constraint that applies a concept to a standard function object (`IsMonoid<T, std::plus<T>>`) grants that object's operator (`+`), since the standard defines the call by it; a default argument (`typename Op = std::plus<T>`) grants nothing. Inside `if constexpr` on concepts applied to types, the branch's concepts grant their operations in the then-branch (the instance chain); a `requires` expression or a concept on `decltype(op(x))` there grants nothing. A conversion into a parameter type, `T(n)`, `T{n}` or `static_cast<T>(n)` from a value of another type, is an operation of `T` like any other: `constructible_from` or `convertible_to` names it, as do the integral concepts and a requirement that writes it; default construction and copies are what `semiregular` grants. Silent where it cannot read a constraint |
| `rule2-no-inline-requires` | the tool's `inline-requires` check: an `if`, constexpr or not, whose condition contains a requires-expression, a constraint tested in the body instead of declared (#1068); the fix is a named shape |
| `rule2-no-type-observation` | the tool's `type-observation` check: in a function template's body, `sizeof`, `alignof`, `typeid`, a standard type trait (`std::is_integral_v<T>`, `std::is_same<T, U>::value`, `__is_same`) or a standard concept used as a value, at a dependent type: an observation of a type no shape names, which an archetype would answer for itself.  The condition of an `if constexpr` is not judged (Table 7's derived form; the second stage leaves such a body undecided) |
| `rule2-decided-resolution` | the tool's `undecided-resolution` check, GHC's rule: a call at a value of parameter type to an overload set (candidates of viable arity) whose constraints differ in an atom, a concept on the argument the call passes with its polarity, that the enclosing constraints neither guarantee nor refute (conjunctions read through the library's concepts with arguments substituted; a disjunction guarantees nothing). Not seen: trait reads through class and variable templates (specialisations differ by pattern), candidates that differ by head alone, calls through a helper. Gated at zero on the listings and on `category` |
| `rule2-context-order` | the tool's `context-order` check, the well-formedness of resolution: an instance with a context whose head has no proper part (a bare type parameter, a concrete type) names in its context only shapes declared before the trait's primary; otherwise resolving the trait entails a shape whose requirement calls the trait, a satisfaction that depends on itself, which the compiler reads as unsatisfied and does not report. A structured head descends by its parts and is not judged |
| `rule2-converted-result` | the tool's `converted-result` check, the conversion discipline (#1143): the result of `{ e } -> std::convertible_to<T>` is used only at `T`. A finding where it is an operand of a further operation, has a member read, or is held in an `auto`; a condition, `!`, `&&` and `\|\|` are at `bool`. A call's argument is not judged, its parameter being unknown in a template |
| `inventory` | not a rule: the tool's `inventory` check prints, per file, every AST node kind used inside the scope (declarations, statements and expressions, type forms as written; instantiations and implicit code left out) with its count. Reported, never gated, on the listings, `category`, `sets` and `relational`, to diff by hand against the kernel's grammar (#1079); it has no fixture, since its output is a measurement rather than a finding |
| `rule1-no-inheritance` | the tool's `inheritance` check: no subtyping. A class with a base that is not an empty class, or with a virtual function; an empty base (`: std::true_type`, `: is_x<Y>`) is a registration spelled by inheritance |
| `rule2-by-type` | the tool's `admit` check, the second stage: a constrained function template no deny-rule refuses whose body does **not** type-check at archetypes of its shapes (see *Admission by type*); one finding per definition refused, its reason the compiler's or the generator's |
| `undecided-by-type` | the tool's `undecided` check: a definition the second stage cannot judge, with the requirement that stopped it (a shape the generator does not read, an `if constexpr`, a use that resolved to another overload) |
| `admitted` | the tool's `admitted` check: a definition that type-checks at its archetypes, a certificate, not a finding |
| `definitions` | not a rule: the tool's `definitions` check prints every definition with the syntax kinds it uses, which `levels.py` joins with the outcomes into FJlt, Jlt or refused |

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

## Admission by type (the second stage)

The deny-rules refuse by name; a definition they do not refuse is not thereby
inside Jlt.  The second stage admits by type.  For each constrained function
template the generator reads the shapes of its signature, builds an
*archetype* of each parameter (a type with exactly the calls and laws the
shape promises), appends to the file's text, in memory, the archetypes, the
traits' entries at them and a use of the template, and re-parses with the
file's own compile command (`-Wno-error`: warnings stay warnings).  An error
in the template's body refuses it; no error admits it; anything else, a
requirement no rule reads or an error outside the body, leaves it undecided,
with the reason.

- `archetypes.hpp` ships with the linter and depends on nothing in dedekind:
  carriers (`carrier`, `comparable_carrier`, `semiregular_carrier`,
  `regular_carrier`), `nullary<C>`, `binary<C, R>`, `arrow<A, B>`,
  `sized_range_of<C>`, `input_range_of<C>`, `converts_to<T>` (a result only
  `convertible_to<T>`, converted from the fresh value only), and the integer
  probes (`every_arithmetic`, `smallest_integer`, `largest_integer`).  Its
  `static_assert`s are its self-check.
- The generator's rules, one per kind of requirement, are the paper's Table 13
  (Appendix G.1), in `tool/jlt_lint.cpp` (`readConstraint`, `readRequires`,
  `readImplied`, `findAtoms`, `appendixFor`, `admit`).
- What an admission means is the paper's conjecture and its hypotheses
  (Appendix G.1): well typed at every type that satisfies the shapes and keeps
  the rules, the same resolution, under hypotheses such as qualified calls by
  name and a body that observes no type except through a shape.
- `fixtures/rule2-by-type.cpp` with `rule2-by-type.expected`: one outcome per
  template, in source order, checked by `outcomes.py` beside the count gates.
- `levels.py` joins the outcomes with the `definitions` kinds into the paper's
  Table 6 and fails if the printed table differs; `table.py` and
  `admission.py` make Tables 11 and 12 from a build report; `pin.py` pins a
  report's counts as gates (a glob at 0, a line per file with findings).

## Stated in the paper, not yet checked

The paper's Appendix B is the specification.  These of its conditions have no
rule here yet, and a reader holds them:

- **The guard.**  A dereference of a range's head, `*ranges::begin(r)`, or of
  an optional, `*s`, is admitted only (1) in the branch of a conditional that
  its guard selects (`ranges::size(r) == 0 ? a : b`, `s ? b : a`), (2) on the
  same variable the guard tests, (3) that variable being a variable and not an
  arbitrary expression (rules T-Guard, T-Guard-Opt).
- **The order of declarations.**  What a shape names stands above it; an entry
  of a trait stands above the first read it matches; every overload of a
  function stands above the first call it could be chosen for.
  **TODO, checkable:** the tool sees each declaration with its place (it
  already has `context-order`), so it can report all three.  Planned form:
  (1) each function or trait a shape mentions is declared above the shape;
  (2) no read of a trait, and no use of a shape that reads it, stands above an
  entry it could match, judged conservatively by the trait's name and the
  head's constructor; (3) all overloads of a name stand above the first call
  of that name.  Across files: an entry stands in the file or module of its
  trait or of a type in its head.  The check errs on the side of refusing, and
  it cannot see what the preprocessor hides.
- **Reads of a trait.**  At a type with a type variable a trait is read only in
  an entry's initialiser, at the parts of the entry's own head, or inside a
  shape.
- **The unproved claims beyond entries at one type.**  The rule `axioms` and
  the ledger list an entry at one type whose value is `true`.  They do not list
  an axiom for a family of types (an entry whose head has a parameter and whose
  value is `true`), a lemma (an entry at a form), or a unit named at a use.
- **Well-founded declarations.**  Nothing checks that the instantiations a
  program can ask for are well founded.  The linter checks one of the two
  syntactic conditions that approximate it; they do not imply it.
- **The second stage's own blind spots** (the conjecture's hypotheses,
  Appendix G.1): a trait of the program read at a type variable outside a
  shape (the standard's are `rule2-no-type-observation`); a negated constraint is false
  at an archetype; several integer parameters, and an equation between a
  member type and an integer parameter, are left undecided; a call by name
  inside a shape is assumed qualified.  The fixes are in the paper's TODO,
  "From the cold reads of #1212".

## Plan

1. Gates per file in CI, deny-rules and the second stage on every layer of the
   library but the Python bindings, the listings and the fixtures (done).
2. The ratchet: `pin.py` pins reported counts today; a check that a gate may
   only move one way (admitted up, refused and undecided down) is next.
3. More rules, as the histogram of undecided reasons and the paper's TODO ask.
4. A clang-tidy plugin when a rule needs code a matcher cannot express;
   `scope` stays the switchboard.
