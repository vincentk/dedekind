# Local mutable state in Jlt: why a counted loop over local arrays denotes a pure function

Study for the Jlt paper (2026-11-24). Scope: the `fibonacci` listing (2x2 matrix power by squaring, `std::array<size_t, 4>` locals, `r[0] = a` etc.) and an in-place LU of a local `std::array<double, 9>`.
Read on the repository: `sections/3-kernel.tex` (Figure 2, Tables 5 and 6), `sections/a2-sugar.tex` (Table 7), `sections/2-three-examples.tex` (Rule 1), `tools/jlt-lint/README.md`, `rules/rule1-*.query`, `rules/rule3-no-loops.query`, `docs/paper/references.bib`.

Notation: `σ` is a state, a record with one field per mutable local in scope; `[[s]]` is the state-passing reading of a statement, a function from states to states; `a[i ↦ v]` is the array equal to `a` except at `i`, where it is `v`.

---

## 1. Literature

"Verified" means the metadata (authors, title, venue, year, pages, DOI) was checked against Crossref or the publisher in this session.
"Content verified" means I read the passage itself. Everything else about content is from memory and marked so.

| # | Source | Status | What it proves | What it assumes about aliasing |
|---|---|---|---|---|
| L1 | Reynolds, *Syntactic control of interference*, POPL 1978, 39–46, doi:10.1145/512760.512766 | metadata verified; content from memory | In an Algol-like language, a syntactic rule (in `P(Q)`, `P` and `Q` share no free identifier unless the sharing is passive, read-only) makes distinct identifiers non-interfering. Program reasoning can then treat commands on distinct identifiers as independent. | **The aliasing condition is what the paper sets out to prove**: distinct identifiers never denote overlapping storage. It names Jlt's premise exactly, but its machinery is about procedures that share state, and Jlt has none. |
| L2 | Lucassen and Gifford, *Polymorphic effect systems*, POPL 1988, 47–57, doi:10.1145/73560.73564 | metadata verified; content from memory (the masking rule; I could not reach the abstract) | Types carry effects on regions. *Effect masking*: an effect on a region that is free in neither the environment nor the result type may be dropped, so the expression is pure to its context. | Every reference is typed with its region, so aliases can only be in the same region. Masking needs the region to be unreachable from the environment and the result. Proved sound later by Talpin and Jouvelot (JFP 2(3), 1992, 245–271, doi:10.1017/S0956796800000393, metadata verified) and Tofte and Talpin (Inf. Comput. 132(2), 1997, doi:10.1006/inco.1996.2613, metadata verified). |
| L3 | Launchbury and Peyton Jones, *Lazy functional state threads*, PLDI 1994, 24–35, doi:10.1145/178243.178246 | metadata verified (already `launchbury1994lazy`); content from memory | `runST :: (∀s. ST s a) → a`: the rank-2 type keeps references of one state thread out of the result and out of other threads, so `runST m` is a pure value. The paper gives the type and an informal argument, not a safety proof. | A reference is `STRef s a`, tagged with the thread `s`. Parametricity in `s` stops a reference from escaping. Aliasing *inside* a thread is unrestricted: the encapsulation guarantees purity, not in-place correctness. |
| L4 | Launchbury and Sabry, *Monadic state: axiomatization and type safety*, ICFP 1997, 227–238, doi:10.1145/258948.258970 | metadata verified; content from memory | Equational axioms for `ST` and a type-safety proof for `runST`. | As L3. From memory, its proof was found to have a gap, which motivated L5. Do not cite it alone. |
| L5 | Moggi and Sabry, *Monadic encapsulation of effects: a revised approach (extended version)*, JFP 11(6), 2001, 591–627, doi:10.1017/S0956796801004154 | metadata verified; content from memory | Type safety of `runST`-style encapsulation, proved by subject reduction for a revised calculus. | As L3. Safety ("does not go wrong") is weaker than observational purity. |
| L6 | Timany, Stefanesco, Krogh-Jespersen, Birkedal, *A logical relation for monadic encapsulation of state: proving contextual equivalences in the presence of runST*, PACMPL 2(POPL), 2018, doi:10.1145/3158152 | metadata and abstract verified. **Correction to the brief**: the authors are Krogh-Jespersen and Birkedal, not Krebbers. | From the abstract: computations encapsulated by `runST` are heap independent, and the contextual equivalences expected of pure code still hold when `runST` is present. Mechanised in Coq (Iris). This is the strongest published form of "local state is observationally pure". | Higher-order references, impredicative polymorphism, recursive types. Aliasing within a thread is allowed. Encapsulation is held by the type alone. |
| L7 | Cytron, Ferrante, Rosen, Wegman, Zadeck, *Efficiently computing SSA form and the control dependence graph*, TOPLAS 13(4), 1991, 451–490, doi:10.1145/115372.115320 | metadata verified; **content verified** (read §3.1.1 and §3.1.3); already `cytron1991ssa` | §3.1.1 "Arrays", quoted: "Update(A, j, V) evaluates to an array value that is of the same size as A and has the same component values, except for V as the value of the jth component. Assigning a scalar value V to A(j) is equivalent to assigning an array value to the entire array A". The paper adds that the translation "is unconcerned with whether the values of variables are large objects". | §3.1.3 "Implicit references": aliased variables, globals changed by calls and dereferenced pointers must be made explicit before SSA applies. With no aliases the whole-array reading is exact. |
| L8 | Kelsey, *A correspondence between CPS and SSA form*, SIGPLAN Notices 30(3), 1995, 13–22, doi:10.1145/202530.202532 | metadata verified; content from memory; already `kelsey1995correspondence` | Translations both ways between SSA programs and a restricted CPS: φ-nodes are the parameters of local continuations. | Scalar variables, no aliasing. Arrays only through L7's Update. |
| L9 | Appel, *SSA is functional programming*, SIGPLAN Notices 33(4), 1998, 17–20, doi:10.1145/278283.278285 | metadata verified; content from memory; already `appel1998ssa` | An essay, not a theorem: SSA is a program of mutually tail-recursive local functions, block arguments being φ-nodes. | As L8. |
| L10 | Wadler, *Linear types can change the world!*, in Broy and Jones (eds), *Programming Concepts and Methods*, North-Holland, 1990, pp. 561–581 | from memory, **unverified** (no DOI; Crossref has no record) | A value of linear type is used exactly once, so a functional update of it may be done in place without being observed. | The type rules out aliasing: there is no second name to observe the update. |
| L11 | Barendsen and Smetsers, *Uniqueness typing for functional languages with graph rewriting semantics*, MSCS 6(6), 1996, 579–612, doi:10.1017/S0960129500070109 | metadata verified; content from memory | Soundness of Clean's uniqueness types with respect to graph rewriting: a unique object may be destructively updated. | The reference count is 1 when the object is updated, checked by the type. |
| L12 | Hofmann, *A type system for bounded space and functional in-place update*, ESOP 2000, LNCS 1782, 165–179, doi:10.1007/3-540-46425-5_11 | metadata verified; content from memory | A linearly typed first-order functional language compiled to in-place C, with the compiled code proved to compute the functional semantics. This is the converse direction from Jlt's: functional code read as in-place. | Linearity of the updated value, in the type. |
| L13 | Reynolds, *Separation logic: a logic for shared mutable data structures*, LICS 2002, 55–74, doi:10.1109/LICS.2002.1029817; O'Hearn, Reynolds, Yang, *Local reasoning about programs that alter data structures*, CSL 2001, LNCS 2142, 1–19, doi:10.1007/3-540-44802-0_1 | metadata verified; content from memory | The frame rule: `{P} c {Q}` gives `{P * R} c {Q * R}` when `c` changes no variable free in `R`. A computation on a footprint disjoint from the context leaves the context alone. | Separation `*` *asserts* disjointness of heap parts. Aliasing is allowed in general and excluded where `*` says so. Stack variables are not aliased by assumption. |
| L14 | Clarke, Potter, Noble, *Ownership types for flexible alias protection*, OOPSLA 1998, 48–64, doi:10.1145/286936.286947 | metadata verified; content from memory | The representation of an object is reachable only through its owner (owners-as-dominators). | Aliases of internal state are confined to the owner. |
| L15 | Jung, Jourdan, Krebbers, Dreyer, *RustBelt*, PACMPL 2(POPL), 2018, doi:10.1145/3158154 | metadata and abstract verified | Machine-checked safety of a realistic subset of Rust, `unsafe` libraries included, by a semantic model of types in Iris. | `&mut` is unique for the lifetime of its borrow. A shared `&` allows mutation only through interior-mutability types. |
| L16 | Pitts and Stark, *Operational reasoning for functions with local state*, in HOOTS, CUP 1998, 227–273 | in the bib (`pitts1998operational`); content from memory | Operational techniques (contextual equivalence, logical relations) for an ML-like language with dynamically allocated references held privately by closures, e.g. a counter object. | References are first-class values and may be aliased. Their result concerns state that **outlives** a call, held in a closure. |
| L17 | Knobe and Sarkar, *Array SSA form and its use in parallelization*, POPL 1998, 107–120, doi:10.1145/268946.268956 | metadata verified; content from memory | Element-level SSA for arrays: φ-functions merge element definitions, a refinement of L7's whole-array Update. | No aliasing between array names. |
| L18 | Benton, Kennedy, Beringer, Hofmann, *Relational semantics for effect-based program transformations with dynamic allocation*, PPDP 2007, 87–96, doi:10.1145/1273920.1273932 | metadata verified; content from memory | Effect-based transformations, effect masking among them, proved sound by a relational semantics of effects with regions. | Regions, abstracted. |
| T1 | Reynolds, *Theories of Programming Languages*, CUP 1998 (ISBN 0-521-59414-6) | table of contents verified against Cambridge Core (Ch. 2 "The simple imperative language", variable declarations in §2.5; Ch. 4 "Arrays"); content from memory | The denotational semantics of the simple imperative language: a command means a state transformer, a local declaration means the transformer with the local added and then dropped, and assignment to an array element means an update of the array's value. | Arrays are values held in variables, so there is no aliasing between arrays or between variables. |

### A correction to the current text of `a2-sugar.tex`

The paper now grounds local state in Pitts and Stark (L16).
That is a stronger theory than Jlt needs, and it is aimed at the case Jlt forbids: state that **outlives** a call, captured by a closure.
In Jlt, local state lives exactly as long as the call and is never named by a reference.
The `runST` sentence (L3) fits the idea better, but `runST` holds encapsulation by a type and Jlt does not need encapsulation at all.
Section 2 explains why.

## 2. Which account fits Jlt, and the recommendation

**Jlt's situation.**
- Locals are held by value, and `std::array` is a value type.
- The only references are the transient lvalues that `operator[]` returns. Each is consumed inside one full-expression, as an assignment target or as a read. None is bound to a name, stored, captured or passed on.
- Rule 1's deny-rules close escape: no reference parameters (except `const T&`), no reference returns, no static locals, no mutable globals, no `volatile`, no mutable boxes, and no escaping reference captures.

In that situation **no two names denote overlapping storage, ever.**
Reynolds' premise (L1) holds by construction, not by a type discipline.
What remains is first-order and alias-free, the simple imperative language of T1.
For that language the state-passing (SSA) reading is the textbook *semantics*, not a theorem that needs encapsulation.

**The decisive point.**
The derived form rewrites into FJlt, and FJlt has no state.
So "denotes a pure function" follows from "the rewrite keeps the value" alone.
No masking or `runST` result is needed, because no state is left in the target to mask.
Effect masking (L2), `runST` (L3, L6), linear and uniqueness types (L10, L11) and ownership (L14, L15) are all ways to *recover* alias-freedom in a language that has references as values.
Jlt has no references as values, so it needs none of them, though it is the special case each of them reduces to.

**Recommendation.**
- **Primary: Cytron et al. 1991, §3.1.1 (L7)**, already in the bib as `cytron1991ssa`.
  - It is the published source of exactly rewrite (a): an assignment to an element is an assignment of an `Update` value to the whole array.
  - Its §3.1.3 names aliasing as the condition that must be discharged first. That is the condition the linter checks.
  - Put Appel 1998 (L9) beside it for the reading of SSA as functional programs, which turns the SSA into a fold.
- **Textbook: Reynolds, *Theories of Programming Languages* (T1), Chapters 2 and 4.** Local declarations and arrays as state transformers.
- **Keep `runST` as the analogy, and cite its proof as Timany et al. 2018 (L6), not Launchbury and Sabry (L4).** One clause: "Haskell's `runST` holds the same by a type [L3], proved in [L6]".
- **Drop Pitts and Stark (L16) from this sentence**, or move it to related work as the theory for state that outlives a call.
- Positioning (Jlt between Rust and C++): Jlt's discipline is a strict sub-case of Rust's. Every `&mut` from `IndexMut` lives for one statement. Cite RustBelt (L15) only in related work, as the stricter side.

## 3. The derived forms for Table 7

The table's real headers are **derived form | read as | condition** (the brief's "Jlt writes | rewritten to | provided").
The rows below replace the current rows "an assignment, `++` on a local" and "a counted `for`" and add four.
**Mutable local** below means a variable that meets all of these:
- it is declared in the function body, or is a by-value parameter;
- it has automatic storage;
- it is neither `const` nor of reference type;
- its type is an atom, a floating carrier, a Jlt record, or `std::array<A, N>` with `A` one of those.

**State of a statement** means the mutable locals that the statement assigns and that are declared outside it.

| derived form | read as | condition |
|---|---|---|
| (a) an assignment to an element of a local array held by value, `r[i] = e;`, and `r[i] op= e;`, `++r[i];` | the assignment of the whole array, `r = r[i ↦ e]`, its update the record of `N` fields with field `j` equal to `i == j ? e : r.j` (with a constant `i`, field `i` replaced) [Cytron et al. §3.1.1; Reynolds 1998 Ch. 4]; `op=` and `++` as `r[i] = r[i] op e` and `r[i] = r[i] + 1` | `r` a mutable local; the reference that `operator[]` returns used only as the target of this assignment, which is a statement of its own, or read at once (A1–A4 below); `i` within `0..N-1` (A5); `e` and `i` assign nothing |
| (b) an assignment, `++`, `op=` on a mutable local, `x = e;` | the next state, `σ[x ↦ e]`; the state is the record of the mutable locals in scope, passed from statement to statement | `x` a mutable local, written before it is read (it has an initialiser); the assignment a statement of its own; `e` assigns nothing |
| (c) a sequence of statements, `s₁ s₂` | the composite `[[s₂]] ∘ [[s₁]]` of their readings | none beyond its parts |
| (d) a local constant, `const T x = e;`, also several in one declaration | `x` replaced by `e` in what follows, or the call of an arrow taking `x` (equal values: `e` is pure and its evaluation ends) | `T` not a reference type; `x` in scope only to the end of its block |
| (e) a statement `if (c) s₁ else s₂`, the `else` optional | the choice on states `c ? [[s₁]]σ : [[s₂]]σ`, a missing `else` the identity | no `return`, `break`, `continue` or `goto` in `s₁` or `s₂`; locals declared inside a branch die with it and are not part of the state |
| (f) a counted `for`, `for (size_t i = a; i < b; ++i) s`, nested ones included | `fold_left(iota(0uz, b > a ? b − a : 0), σ, step)`, the step `(σ, j) ↦ [[s]]σ` at `i = a + j`, its state the record of the mutable locals that `s` assigns and that are declared outside the loop; a nested loop is a fold inside the step | Rule 3's conditions as now (the counter and the bound not assigned in the body; no `break`, `return` or `goto`; Meyer and Ritchie); in addition the bound does not read a local that the body assigns; `a` and `b` evaluated once, at entry |
| (g) the body of a function, mutable locals, statements, `return e;` last | `λ params. [[e]]([[s₁ … s_k]] σ₀)`, `σ₀` the parameters and the initialised locals | one `return`, at the end (or one in each branch of a final `if`); the result a value, never a reference (Rule 1) |

### The aliasing conditions A1–A6, stated so that a clang visitor can check them

Let `v` be a mutable local of type `std::array<A, N>` or a scalar, within one function definition in scope.
Every `DeclRefExpr` to `v`, after `IgnoreParens`, is in exactly one of these positions; anything else is a finding and the derived form does not apply.

1. **A1, a read.**
   - For a scalar: the operand of `ImplicitCastExpr<LValueToRValue>`.
   - For an array: the operand of a copy (`CXXConstructExpr` of a copy constructor). Passing `v` by value or returning it is allowed.
   - An array may be the argument bound to a parameter of type `const std::array<A, N>&` of a function in scope, which Table 7 already reads as by value.
   - An array may be the object of a `const` member call that returns a value (`size()`, `empty()`).
2. **A2, an element.** The object argument (argument 0) of a `CXXOperatorCallExpr` whose callee is `std::array<A, N>::operator[]`. That call expression `E` must itself be:
   - (i) the LHS of a built-in assignment or compound assignment, or the operand of `++` or `--`, whose parent is an `ExprStmt` (or the increment of nothing else); or
   - (ii) the operand of `ImplicitCastExpr<LValueToRValue>`.

   Nothing else: not the initialiser of a reference-typed `VarDecl` or `BindingDecl`, not the operand of unary `&`, not an argument bound to a reference parameter, not captured.
3. **A3, a whole assignment.** For a scalar `v`: the LHS of an assignment, `op=`, `++` or `--` whose parent is an `ExprStmt`, or the counter of a counted `for`.
4. **A4, one write per full-expression.**
   - In the RHS of any assignment in the function, and in every index, condition and initialiser, there is no assignment, `op=`, `++` or `--` (the `for` increment excepted).
   - So each statement writes at most one object, and C++'s order of evaluation cannot be observed.
   - C++17 sequences an assignment's right operand before its left [expr.ass]/1, so `r[0] = r[1] + r[0]` reads before it writes.
5. **A5, in range.** The index of an `operator[]` on `v` is an integer expression whose interval, evaluated over its leaves, lies within `[0, N−1]`. The leaves are:
   - integer literals;
   - counters of enclosing counted `for`s, with interval `[lb(a), ub(b) − 1]` where `a` and `b` are literals or affine in outer counters;
   - `+`, `−` and `*` by a literal.

   This covers `r[0]` and the LU's `a[i*3 + j]` with `i, j < 3`. An index the interval check cannot bound leaves the definition *undecided*, not refused. Out of range is undefined behaviour at run time and a diagnosed non-constant during constant evaluation, so the two readings would part.
6. **A6, declaration.**
   - `v` has a written initialiser: `VarDecl::hasInit()`, or a by-value parameter.
   - `v` is not `static`, `thread_local` or `volatile`.
   - No lambda captures `v` by reference: no `LambdaCapture` with `LCK_ByRef` naming `v`, and no `[&]` default whose body mentions `v`.
   - No range-`for` over `v` binds by reference: the loop variable is not of reference type.
   - The element type `A` has no user-provided copy assignment (`!hasUserDeclaredCopyAssignment()` on a record), so `r[i] = e` is a value's assignment.

Also, function-wide: **no local of reference type at all.** Any `VarDecl` or `BindingDecl` of `ReferenceType` in the body is a finding.
This is the one hazard worth saying aloud in the paper.
`const auto& x = r[0];` is a *const* name for a *mutable* object, so `r[0] = …` changes `x`.
`const` on a reference is not immutability of its referent.

A1–A6 are a **whitelist** over uses, not a deny-list, which is why they can be checked syntactically.
They imply Reynolds' premise: every lvalue that denotes (part of) a local's object is built from that local's own name within one full-expression, so distinct names denote disjoint objects.
Rule 1's existing deny-rules then become redundant for the locals, though not for parameters and globals.
Two gaps that the current deny-list leaves (pointers, `std::span<T>` over a local, struct fields of reference type, `reference_wrapper`) are closed for the locals by A2's "nothing else".
Flagged for Rule 1 in general: no rule refuses a **field of reference type** or a returned struct that holds a reference. Today only the syntax-kind levels would catch it.

The `fibonacci` listing meets A1–A6: `r` and `m` are initialised, every `r[k]` and `m[k]` has a literal index (A5) and is either read or the target of an assignment statement, the `const` temporaries are by value, and the loop is `0..64` with a literal bound.
The LU meets them with the interval check of A5, `/=` and `-=` as statements (A2(i), A4), and `double` as a carrier whose operations are total.

## 4. The lemma

> **Lemma (state passing).** Let `f` be a `constexpr` function of Jlt. Suppose that:
> - every local it assigns is a mutable local meeting A1–A6;
> - its statements are those of rows (a)–(g);
> - every function it calls is of Jlt.
>
> Then the reading `[[body_f]]` of Table 7 is a term of FJlt, the derived forms of the other rows aside. For every argument `v`, the evaluation of `f(v)` by the C++ abstract machine ends with value `w` exactly when `[[body_f]](v)` has value `w`. In particular, `f` is a function of its arguments.

*Hypotheses.*
1. **H1, alias-freedom:** A1–A6.
2. **H2, sequencing:** A4, with [expr.ass]/1.
3. **H3, termination of the parts:** Rule 3, Meyer and Ritchie for the loops, and the callees.
4. **H4, adequacy for the fragment:** C++ evaluates these statements as T1's semantics says. This is the paper's existing hypothesis of adequacy, widened from kernel terms to these statements.
5. **H5, no escape:** Rule 1's deny-rules for parameters, returns and globals.

*Proof sketch.*
- Induction on the statement, with this invariant at each statement boundary: the objects of the mutable locals in scope hold exactly the fields of `σ`, and no other object visible to the caller has changed (a frame condition).
- H1 makes each write change exactly one field of `σ`, so the frame condition holds; this is separation logic's frame rule (L13) with a trivially separated store.
- H2 makes the order of evaluation inside a statement unobservable.
- Row (f) is L7 and L9: the loop's φ-variables are the fold's state.
- At `return`, the locals end their lifetime and the result is a value (H5), so dropping `σ` is sound. This is the masking step, trivial here because no reference to the frame exists.

*What is standard and what is new.*
- Standard: the state-transformer semantics of an alias-free first-order imperative language with arrays (T1), the whole-array `Update` reading (L7), SSA read as functional programs (L8, L9), and the counted loop as primitive recursion (Meyer and Ritchie).
- The lemma *instantiates* T1's semantics plus L7. It is not a corollary of any `runST` theorem, and the paper should not say it is.
- New, and the paper's own: the syntactic fragment of C++ (A1–A6) that pins the alias-free language inside C++, together with H4 for it.
- The paper can prove the lemma on paper in about a page, in the style of Appendix F. Or, consistent with "this paper does not prove that a derived form preserves values", it can state it as argued, with T1 and L7 as the support, and the linter's whitelist as the evidence that the hypotheses hold.

## 5. Translatability versus correctness of the in-place algorithm

These are two different claims, and the derived form makes only the first.

- **Translatability (the derived form's guarantee).** The C++ function and its state-passing reading compute the same value on every input, whatever the body does to its locals, including overwriting `r[0]` before reading it. If the body clobbers an entry, the reading clobbers the same field of `σ`, and both compute the same wrong number. The guarantee is that the code is a pure function, not that it is the function the author meant.
- **Correctness (the algorithm's obligation).** That the function computes the Fibonacci number, or `L` and `U` with `LU = A`, is a property of the algorithm. The `const` temporaries `a, b, c, d` discharge it in `fibonacci`. In the LU it is discharged by the order of the loops: `a[i*3+k]` is final before row `i` reads it, and `j > k` keeps the pivot row unread after it is written. It is proved, or tested by `static_assert`, like any other claim about what a function computes.

Suggested wording (paper style, one sentence per line):

> The derived form says what an assignment to a local means: the next state of a fold, so that the function is a function of its arguments.
> It says nothing about whether the program writes the right thing.
> An algorithm that overwrites an entry before it reads it computes the wrong value, and its reading computes the same wrong value.
> That the values are the intended ones is the algorithm's own claim, tested here by `static_assert`, as for every listing.

The same split appears in the literature.
`runST` and uniqueness types guarantee encapsulation, not in-place correctness (L3, L11).
Cytron's renaming turns an anti-dependence (read then overwrite) into explicit data flow that is visible in the functional reading, so an algorithmic mistake becomes a visible wrong value, not a hidden effect.

---

## BibTeX (new entries; `cytron1991ssa`, `appel1998ssa`, `kelsey1995correspondence`, `launchbury1994lazy` are already in `docs/paper/references.bib`)

```bibtex
@book{reynolds1998theories,
  author    = {Reynolds, John C.},
  title     = {Theories of Programming Languages},
  publisher = {Cambridge University Press},
  year      = {1998},
  isbn      = {0521594146},
  doi       = {10.1017/CBO9780511626364},
  note      = {Ch.~2, the simple imperative language; Ch.~4, arrays}
}

@inproceedings{timany2018logical,
  author    = {Timany, Amin and Stefanesco, L{\'e}o and Krogh-Jespersen, Morten and Birkedal, Lars},
  title     = {A Logical Relation for Monadic Encapsulation of State: Proving Contextual Equivalences in the Presence of runST},
  journal   = {Proceedings of the ACM on Programming Languages},
  volume    = {2},
  number    = {POPL},
  year      = {2018},
  pages     = {64:1--64:28},
  doi       = {10.1145/3158152}
}

@inproceedings{reynolds1978syntactic,
  author    = {Reynolds, John C.},
  title     = {Syntactic Control of Interference},
  booktitle = {Proceedings of the 5th ACM SIGACT-SIGPLAN Symposium on Principles of Programming Languages (POPL)},
  pages     = {39--46},
  year      = {1978},
  publisher = {ACM},
  doi       = {10.1145/512760.512766}
}

@inproceedings{lucassen1988polymorphic,
  author    = {Lucassen, John M. and Gifford, David K.},
  title     = {Polymorphic Effect Systems},
  booktitle = {Proceedings of the 15th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL)},
  pages     = {47--57},
  year      = {1988},
  publisher = {ACM},
  doi       = {10.1145/73560.73564}
}
```

Article numbers 64 (Timany et al.) and the CUP DOI of T1 are from memory. Check them before use. Everything else above is from Crossref.
