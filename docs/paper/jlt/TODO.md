# Jlt paper: what remains

Draft of 2026-11-20, for discussion. Nothing below is started unless it says so.

## What the paper promises

> Under the rules of this paper and a check that a tool performs, a template means what its signature says, the compiler carries a law through every composition, and what was taken on trust comes out as a list.

The reader is a C++ programmer who knows the theory and cannot use it at work.
The paper needs three things and no more:

- a) the theory, assembled and not invented, in a digestible form;
- b) the look and feel of the code, in reproducible listings;
- c) evidence of the tooling that a reviewer can try, not a product.

The model is Table 1 of the introduction.
The state of the promise is the comment "WHERE THE LIE STANDS" in the abstract's source; each item of work below should shorten it.

## Decided

- Perspective: Art. Done in the header.
- FJlt is a value layer (two atoms, carriers, pairs, arrows, the conditional, one iterator) and a resolution layer (named types, shapes, traits, overloads).
- Jlt is FJlt plus derived forms. The Juliet Posture is the three rules, a heuristic and an outer bound.
- "Sugar" names derived forms only. Templates, concepts, traits and overloads are kernel.
- A monoid is a triple of types, `IsMonoid<T, Op, E>`. Compiled on CI as a probe, with strings.
- Carriers are opaque. Every value is `semiregular`; `regular` only where a shape asks.
- The written negation `!C` stays, read as "not known here".
- `then` is dropped as it stands.
- The check restricts its audience to recent Clang. The language stays portable.

## For you to decide

1. Value parameters leave the kernel, now that the probe is green? (Table 1, and Thursday's rules.)
2. A first listing that shows the problem: an ordinary constrained template, and the bare type at which it does not compile. Which template?
3. The unit law: keep the check `Op{}(E{}(), E{}()) == E{}()` inside the shape, or make "E is the unit" an entry of a trait, listed like other axioms, with the check as an assertion beside it?
4. Where entries may live (with their trait or with a type in their head): a fourth rule, a condition of the check, or a stated limitation?
5. The case study on sequences (Appendix G) and the "laws license parallelism" paragraph: keep, shorten, or move to another paper?
6. The abstract's sentence in the third person, or addressed to the reader?

## Work, in order

Each step is discussed before it starts.

### 1. The plan by section (this file, agreed)

### 2. Delete from the kernel what no listing uses

Pure removal, with references repaired.

- `cata`, term types, boxes: T-Cata, T-Node, S-Cata, S-Unbox, "Folds over terms", Proposition on normal forms of arrows.
- Sums, `optional`, `variant`: T-Case, T-Guard-Opt, S-Case, the "Sums" paragraph of Appendix A.
- The ranges `chain` and `repeat`.
- D-Rec and T-Rec, their translation and proof cases; `then` from Listing 3 and from Figure 2.
- Conversions between carriers beyond a literal.
- Value parameters, if decision 1 says so: three grammar rows, T-Val, A-Val, A-Atom, A-Inst, the two "Value parameters" paragraphs, their proof rows.

Shortens: nothing in the promise, but most of Appendices B to E.

### 3. The iterator as the one recursion over a value

- The kernel's primitive is "apply a step to a state, n times". Decide its C++ spelling (a `constexpr` function that calls itself at n − 1, or `fold_left` over an `iota`).
- Ranges become a derived form: a count and an arrow. Three rewrites (`iota`, its reverse, `transform`).
- Out of the kernel with them: `IsRange`, E-Range, T-Fold-R, T-Size, the guard and its rules, Assumption 2.
- The finite-prefix figure moves from Appendix B to Rule 3, as the picture of the iterator.

### 4. The listings

- Listings 2 to 4 with the triple of types. `Zero`, `One`, an empty-string unit.
- The string monoid, and the free theorem as one assertion: the length of a concatenation is the sum of the lengths.
- Listing 4: `Mat<T, Zero, One>`, its identity as a function object, Fibonacci by `power`. The fast `concat` on an arrow and a count.
- The first listing, if decision 2 says so.
- Each listing beside its FJlt form, both compiled, with an assertion that they agree.

Shortens: "what was taken on trust comes out as a list", if units become entries.

### 5. The derived forms, written down

One table in the body: each form the listings use, its rewrite, why it means the same (the standard says so, or equal values on a checked pair), and that it keeps the three rules.
Definition of "observable": the value of a closed term at an atom, at compile time and at run time, cost aside.

### 6. The strict check

Designed on paper with you first.

- Accepts: kernel constructs and listed derived forms. Refuses everything else, naming the construct.
- One command runs it on the listings and prints the axioms.
- A handful of files that each break one thing, each refused with a message.
- States what it does not see.
- Reports two lists: axioms, and forms licensed beyond the paper's.

Shortens: "a check that a tool performs", and with it "a template means what its signature says".

### 7. The theory, assembled

- Value layer: state the system (prenex polymorphism over two atoms, with an iterator, parametric in its carriers) and cite its results. No proofs of ours.
- Resolution layer: our own argument, and only this. Resolution is a function; it is stable under instantiation; the conditions it needs (order of declarations, well-founded declarations, where entries live).
- The free theorem, stated once and instantiated at the string example.
- Appendices B to E rewritten against what is left. Target: a fraction of their fifty pages.

### 8. The body, reordered and rewritten to the promise

- Abstract and introduction: the four sentences that contradict Table 1; the context (what the withdrawn concepts design checked); one audience.
- Order: the problem; the three rules, each with what it keeps out of the jungle; the covered forms; the small kernel as the warrant; what the tool checks; limits.
- Figure 1: the arrow from Jlt to FJlt becomes solid.
- Sections 3 to 5 merge into one short section.
- "Sketch" labels and the sentence that calls those paragraphs drafts go.
- Main body at 22 pages.

### 9. Position against prior work

Read, verify and cite, or decide not to. The readers named these, several from memory:

- concepts for C++0x, their axioms, and the calculus that translates concepts by dictionary passing;
- algorithm specialisation against definition checking;
- axiom-based rewriting and testing in C++;
- coherent resolution with overlap; instance chains;
- reasoning in a total fragment of a partial language;
- the C++ subset with a grammar in *Elements of Programming*;
- core languages with derived forms, and tested desugaring;
- total functional programming.

### 10. A cold read, briefed against the Art criteria and the promise

## To verify before it is stated

- The dates and sources for the withdrawal of concepts from C++0x.
- Every reference in step 9.
- Clause names cited from memory: `temp.param`, `temp.type`, `temp.deduct.call`, `temp.spec.partial*`, `meta.rqmts`, `dcl.constexpr`, `bit.pow.two`, `arithmetic.operations`, `range.*`, `array.overview`, `expr.pre`.
- Chapter and section numbers from memory: Lang, Knuth, Stepanov and McJones, Golan, Bourbaki III §6.5, the direction of the subterm symbol in Baader and Nipkow.
- That the listings compile with GCC, and what MSVC says.
- Reference [15] of the bibliography holds a working note.

## Later, outside this paper

- Generated bare types for every shape.
- A registry for forms a project licenses.
- An executable desugarer.
- A port of ordinary existing code.
- Terms whose shape is data, for the Python bindings: a sequence of symbols folded by the iterator.
- `then`, rebuilt.
