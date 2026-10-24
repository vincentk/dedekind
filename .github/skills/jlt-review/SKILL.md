---
name: jlt-review
description: Review guidance for Jlt compliance of library code in the dedekind repo (src/main/modules/dedekind/**). Use when a pull request adds or changes C++ in the library. The three rules of the Juliet Posture (docs/paper/jlt/jlt.tex, Section 2) are the brief; the mechanical checker in tools/jlt-check gates what it can read, and this review covers what it cannot: types and arities behind names, constraints that name the wrong operation, conversions used at the wrong type, members of class templates, and laws declared where they could be decided.
---

# dedekind Jlt review

The library is written in a fragment of C++23, the Juliet Posture (Jlt), under
three rules. The compiler enforces parts of them, the checker in
`tools/jlt-check` enforces more, and the rest is held by reading. This review
is that reading. Report a breach with the rule number, the operation or
construct, and the fix in one line; the fix is almost always a constraint
stating what the body already does, never a cast or a workaround. Prefer a few
findings that are breaches over a list of style remarks.

## The three rules

| rule | statement | what the compiler holds | what the checker holds | what you hold |
|---|---|---|---|---|
| **1** values and pure functions | every type passed around is `std::semiregular`; every function is `constexpr` and depends on its arguments alone; values are `const`, parameters `const T` or `const T&`, results by value, member functions and call operators `const` | a `constexpr` function that reaches for state fails constant evaluation, when something evaluates it at compile time | the syntax: `constexpr`, `const` members and call operators, no `throw`, no static state, no `volatile`, no non-`const` or forwarding references as parameters, no reference results, no mutable globals, boxed values `shared_ptr<const T>` | a function that is `constexpr` in name but never evaluable (it only ever runs at runtime); a `const` that is cast away; a boxed value whose identity is observed (`get`, `use_count`, `==` between boxes); a closure that captures by reference and escapes |
| **2** behaviour through concepts and traits | generic code does with a `T` only what `T`'s declared constraints say it can | nothing: a template body is checked at instantiation, not against its `requires` clause | at the level of names: an operation on a value of parameter type must be named by some declared constraint, followed through the library's concepts; a concept test inside the body is not a constraint | the level of types: the constraint names the operation but at other operand or result types; a `std::convertible_to` result used at its own type instead of the declared one; an operation on a *member* of a parameter-typed value (`x.first + y.first`) that no constraint on the pair names; a class template's member functions, which the checker does not read; a trait read whose default (`= delete` primary) a new carrier never specialises |
| **3** recursion only folds | recursion runs over a term (`cata`), over a finite chain interval (`chain_view`, `iota`), or over an explicit budget (`fold_left` over `views::repeat(x, n)`, `iterate(f, n, x)`); no other loop, no self or mutual recursion | at compile time only, by the step limit | no loop outside `iterate`; no function calling itself at the same instantiation; no call cycle | a budget computed as the fold runs; a recursion on a *value* dressed as a recursion on a type; a `std::generator` or coroutine; `std::views::take` without the bound being the program's |

## What to check first

1. **New or changed `requires` clauses.** For each operation the body performs on a
   value of parameter type, name the constraint that permits it *at those types*.
   If none does, that is the finding, and the fix is the missing constraint. If the
   body only works because of what some instantiation happens to have (`twice_loose`
   in the paper: `x + x` with no constraint), say so.
2. **Conversions.** A requirement `{ e } -> std::convertible_to<T>` lets the body
   use the result as a `T` and nothing else. A body that compares two such results
   at their own type, or forwards one unconverted, breaches Rule 2.
3. **Members of class templates.** The checker reads free function templates and
   call operators. A member function of a class template that uses an operation on
   a parameter-typed field needs the class's constraints to name it.
4. **Mutation and identity.** Locals may change (encapsulated state, like `runST`);
   nothing else may. Parameters, the object, statics, globals, pointees of boxes
   and captured references are the leak channels. A `const_cast`, a `mutable`
   member, or a `shared_ptr` compared by address is a finding.
5. **Loops and recursion.** Any `for`, `while` or `do` outside `iterate`, any
   recursion that is not `cata` over a term, any bound that is not a value fixed
   before the fold starts.
6. **Laws.** A law stated as a `constexpr bool` specialisation is a declaration; a
   law the carrier could decide (finite ordered carriers, structures with normal
   forms) and that is declared instead is worth a remark, not a finding.

## What not to report

- Findings the CI gate already fails on: the `jlt-check` step reports them with
  file and line; do not duplicate them, but do say if a change *weakens* the
  gate (`tools/jlt-check/scope` moving a `0` to `report`, a rule file deleted,
  a fixture's expected count lowered without a reason in the PR).
- The planted breach `twice_loose` in `jlt_listings_test.cpp`, which the gate
  expects.
- Code outside the fragment by design: `tools/`, `docs/`, Python bindings,
  `src/test/**` test bodies (a `static_assert` or `CHECK` on a Jlt function is
  how the fragment is exercised, not a breach). Review test *subjects*, not test
  harness code.
- Style: naming, formatting (`clang-format` runs on push), comment wording.

## Reading the paper's own words

The checker's findings are read before they are acted on (a finding can be a
gap in its reading of a standard concept, not a breach), and its silence is not
a certificate; this review is the other instrument: it reaches what the checker
cannot, and its silence is not a certificate either. Say which rule, what breaches it, and the one-line
fix. When you are unsure whether a constraint names an operation at the right
types, ask in the thread rather than asserting.
