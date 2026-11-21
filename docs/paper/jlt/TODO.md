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

## Architecture (agreed 2026-11-20)

Modelled on "The Essence of JavaScript": a short introduction, the language introduced by example, a short formal part that cites, an application, and the detail in an artefact.

| Section | Content |
|---|---|
| 1. Introduction | the gap between the concepts proposed and the concepts shipped; the promise; contributions as bullets; Figure 1 and Table 1 as the map |
| 2. Jlt, by three examples | what a signature prevents; what a law permits; what the compiler infers. Each: the listing, the rule it shows at work, which of its lines are kernel and which are derived forms, and its image. The three rules are stated as what the three have in common |
| 3. The kernel and its warrant | FJlt in a page, the cited theory, the free theorem; the adequacy of the derived forms, by test, stated as such |
| 4. The check | the tool, on the three listings: what it accepts, what it refuses, the list it prints |
| 5. At run time | what the compiler leaves |
| 6. Discussion | known limitations (the linter's own are in Section 4); related work; conclusion |

Appendix, short:

- the kernel, complete;
- the observation about resolution, stated;
- the desugarings, as a table; the linter points to it.

Artefact, on a stable branch released to Zenodo:

- the linter, first brought up to speed in features and packaged more nicely, reviewed just before submission;
- the examples "for the curious", packaged with it;
- the tests that pair each listing with its kernel form, the ledger of axioms, the build that prints the emitted code.

State, on the branch `docs/paper-three-examples`, stacked on `docs/paper-listings-in-action` (which is the backup of everything before the cut):

- The body has the agreed architecture: 1 Introduction, 2 Jlt by three examples (with 2.4, the three rules), 3 The kernel and its warrant (3.1 the kernel, 3.2 derived forms, 3.3 the image), 4 The check, 5 At run time, 6 Limits, 7 Related work, 8 Conclusion. It ends on page 25.
- Written new: 2.4, Section 3 and Section 4. Section 4 describes the linter as it is and says what it does not yet do.
- Deleted: the former Section 2 (the rules with the four old listings), the former Sections 3 to 5 (desugaring, translation, guarantees, with the grammar figure, the theorem statements and the worked derivations), the sequence case study, the paragraph on parallelism.
- Not yet touched: the introduction's old text, Section 5 (retitled only), Sections 6 to 8, and Appendices A to F, which still describe the larger kernel.
- The build prints about 130 broken references, by design. The old listings' labels mark text that still speaks of them; the labels of the deleted theorems, figures and sections mark appendix text that proves or uses what is gone.

## Handover, 2026-11-20 evening

Where to work: branch `docs/paper-three-examples`, draft PR #1209 (based on `main` so that CI runs). Branch `docs/paper-listings-in-action`, PR #1208, is the backup of the paper before the cut and is not to be developed further. The C++ workflow runs only for pull requests, so code is confirmed by CI on #1209. No local C++ builds; `lualatex` locally is fine.

Code of the three listings: namespace `triple_probe` in `src/test/cpp/modules/dedekind/category/jlt_listings_test.cpp`. The printed listings must stay verbatim with it, up to formatting and omitted declarations. The showcase `showcase_16_listing3_two_folds.cpp` still holds the older listings' code and prints the emitted code of `power` at the unit.

Confirmed by CI on #1209 (run on edbe86ed, 2026-11-20): the code of all three listings as printed, including the shape that asks only `copyable`, the unit's check as an assertion beside its axiom, `free_theorem` through the shape `IsHomomorphism`, and `Via` with its three carriers.

Open findings from the two reads of the preview (a C++ practitioner and a theorist), none acted on unless listed under "Decided":

1. Listing 1: the Boolean case is a correct "or", not a wrong "and". Claim only that the signature does not say which monoid. A clang-tidy check flags that line; the sentence "nothing objects at any warning level this paper's build enables" invites that reply. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)
2. Listing 2: show the accepted version of `guessed_power` (what Rule 2 wants written). Nothing turns a fold into `power`: the abstract's "a fold of n steps becomes one of at most 64" reads as a transformation.
3. Listing 3: "the compiler infers" is a conjunction through a specialisation: keep the tone plain. (Decided and done: `Via` names its three carriers, six parameters, and is a kernel term.)
4. Kernel (decided 2026-11-20, and in Section 3.1 as a table of what the grammar leaves out and what a rule refuses): a value at a type variable only inside a call a shape requires; no template on a call operator; a self-call refused by rule. The rules themselves are still to be written out.
5. Section 3.1 says a program "means what C++ says" and 3.3 says "its meaning is its image": state their agreement once, as an adequacy hypothesis. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)
6. Section 3.3: "under those two, a selection has one answer" is a coherence claim. Prove it in the appendix or mark it as a condition, and position it against the literature on coherence and stability of resolution (references the theorist gave from memory, to be verified). "Every program ends" needs its hypotheses: carrier arrows terminate, the compiler's selection ends, no function calls itself. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)
7. Name the fragments: what runs is simply typed with an iterator (System T with products and base types); what a signature has is a rank-1 type. Results 1 and 2 need nothing from Rule 2; only the free theorem does. Say so: it is the paper's structural point. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)
8. "Counts, a finite prefix of the naturals" in Table 1 is wrong for the test at 3 to the 10^9, which wraps: the atom is arithmetic modulo 2^64. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)
9. The list of what is trusted must include lemmas (entries at a form), not only axioms. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)
10. Restrict the claim about derived forms to "as used in Listings 1 to 3". The range row is a change of representation, not a local abbreviation. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)
11. Practitioner: the tool must be installable against a released Clang, read `compile_commands.json`, select rules singly, and print diagnostics in the usual form; the axiom list is most useful as a diff, and should cover the standard's own opt-in variable templates (`enable_borrowed_range`, `enable_view`); floating point as a declared, listed assumption is a use the paper undersells. Asked for: one module of a few hundred lines taken through the rules.
12. The agentic reviewer in the "held by" column of Table 2 lowers trust in the column. (Done 2026-11-21, in wording; the proofs and tests it promises are still to come.)

Not yet rewritten: the introduction (old text, four pages), Section 5 (retitled only; its figure still shows the old listings), the Discussion (old text), Appendices A to F (the larger kernel). The appendix is to become: the kernel complete, the observation on resolution, the desugarings.

## To evaluate (2026-11-21)

- **Planted axioms on the list.** The listings and the probe declare some axioms false on purpose (`idempotent<size_t, Plus2>` in Listing 2; `unital<size_t, plus<size_t>, Two>` and `unital<bool, logical_or<bool>, True>` in the probe). The printed list should mark them as planted and say where they are discussed, which needs a marker on such an entry in the source.
- **An axiom checker.** Each law has a meaning that is an equation (associative, unital, idempotent, commutative, homomorphism); a checker evaluates it at sample values for each entry on the list and reports a counterexample. A check, not a proof, as the unit's check at one point. Open: it means building and running test code during linting, which may not fit the linter; a way of working has to be found. Sources of samples: for a monoid the units are already at hand, and a carrier's author can supply a few values; samples must include the cases where a law fails (both truth values; wrap-around for `size_t`). Lemmas can be checked at the composites where they are used. Prior work to verify before citing: QuickCheck and the libraries that test class laws; Bagge and Haveraaen and colleagues on concept axioms used for testing.
- **`guessed_power` and its badge (Listing 2), discussed and not yet drawn.** Lower half: the idempotent case of `power`; `guessed_power`, refused by Jlt; `HasPower` and `named_power`, which wears the dependency as a required call; both give 2 at `Plus2`, because the fault is the planted axiom, which a check beside it (`Plus2{}(2, 2) == 2`) refutes. Image of `named_power` added below the panels; `guessed_power` has none. Table 4 gains a row: a call whose case depends on a law the declaration does not decide. The author wants to look at Listing 2 before deciding what to drop for space.

## Decided

- Associated types are kernel (2026-11-21): a trait whose result is a type, one entry per head, read at a type variable only in a declaration's types; its image is one more type parameter, supplied at each use (improvement). A monoid is named by its unit: the probe `unit_probe` is green. Next: Listing 1 in that form, then a look at the page, then Listings 2 to 4.

- Perspective: Art. Done in the header.
- FJlt is a value layer (two atoms, carriers, pairs, arrows, the conditional, one iterator) and a resolution layer (named types, shapes, traits, overloads).
- Jlt is FJlt plus derived forms. The Juliet Posture is the three rules, a heuristic and an outer bound.
- "Sugar" names derived forms only. Templates, concepts, traits and overloads are kernel.
- A monoid is a triple of types, `IsMonoid<T, Op, E>`. Compiled on CI as a probe, with strings.
- Carriers are opaque. Every value is `semiregular`; `regular` only where a shape asks.
- The written negation `!C` stays, read as "not known here".
- `then` is dropped as it stands.
- Value parameters leave the kernel. Done in Table 1; the rules go in step 2.
- The unit law is a listed axiom, an entry of a trait on `(T, Op, E)`, and the check at one point stays: it checks and does not prove (1 · 1 = 1).
- Three listings are the body's material, each on a function the reader knows, with code on the left, its tests on the right, and its image in System F below. Both are drafted in the introduction.
  - Listing 1, the monoid and `concat`: the shape as magma, unital magma, monoid; `concat1`, the loop with `+` and a default start hard-coded; `concat2`; three monoids and the minimal one; C++ asked whether `concat1` accepts the minimal monoid, answering yes.
  - Listing 2, the monoid and `power`: the concatenation of a repetition; repeated squaring; the power of the unit is the unit, whatever the count; a case that idempotence selects; a caller whose result depends on a law it never named.
  - Listing 3, what the compiler infers: monoids by name and the law that a function is a homomorphism; three axioms (lengths add, emptiness, De Morgan); the lemma that one homomorphism after another is one; the inferred fact about non-emptiness; the free theorem of `concat2` as a function and three calls of it.
  - The three are introduced as what a signature prevents, what a law permits, what the compiler infers, and closed by what the rest of the paper supplies. The abstract's promise follows the same three, then the list.
  - Parked, with code in the probe: the semiring and one product; the lemma that a power is a homomorphism where the operation commutes.
  - Further listings go to an appendix "for the curious".
  - Not written yet: the test that expects the compiler to refuse a call of `concat1` at the minimal monoid.
- Delete aggressively. The sequence case study (Appendix G, its listing, its sentences in the abstract) and the parallelism paragraph go with step 2.
- The abstract's promise stays in the third person.
- Where an axiom or a lemma may be declared is not a new rule: it is the second of the order conditions the kernel already has. They stay in Appendix B, with a reference from Rule 2, and the strict check is to report them.
- The check restricts its audience to recent Clang. The language stays portable.
- The kernel's surface stays close to ISO C++: calls, parameters and fields are sequences, and its iteration is the standard's `fold_left` over an `iota`. System F is reached by currying and products, which are textbook. Several parameters and records are therefore kernel, not derived forms.

## For you to decide

None at the moment.

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
- The sequence case study: Appendix G with its listing and measurements, the two sentences on it in the abstract, and "The same laws license parallelism" in Section 6.
- Value parameters: three grammar rows, T-Val, A-Val, A-Atom, A-Inst, the two "Value parameters" paragraphs, their proof rows.

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
- The first listing, in four panels as decided.
- Each listing beside its FJlt form, both compiled, with an assertion that they agree.

Shortens: "what was taken on trust comes out as a list", since a unit becomes a listed entry.

### 5. The derived forms, written down

One table in the body: each form the listings use, its rewrite, why it means the same (the standard says so, or equal values on a checked pair), and that it keeps the three rules.
Definition of "observable": the value of a closed term at an atom, at compile time and at run time, cost aside.

### 6. The strict check

Designed on paper with you first.

- Accepts: kernel constructs and listed derived forms. Refuses everything else, naming the construct.
- One command runs it on the listings and prints the axioms.
- A handful of files that each break one thing, each refused with a message.
- States what it does not see.
- Reports the three conditions on the order of declarations (Appendix B), conservatively, and across files by location: an entry in the file or module of its trait or of a type in its head. Flagged in Section 7 and in the linter's README.
- Reports two lists: axioms, and forms licensed beyond the paper's.

Shortens: "a check that a tool performs", and with it "a template means what its signature says".

State 2026-11-21: a first cut exists and is a statistic, not a gate: `tools/jlt-lint/admission.txt` classes the inventory's syntax kinds (kernel, derived, scaffolding, refused, open), `admission.py` sums them, Appendix G's Table 13. For the paper it is enough that the numbers can be collected; the user does not want to tune them now. Later, over a few PRs: decide the open kinds (six carry most: string literals, `size_t(x)` casts, `this`, enums, unary operators, value template parameters); improve the library under the posture; expect the rule set itself to change on the way. Adoption is file by file and rule by rule, so the same statistic on code not written under the posture measures the cost of adopting it: candidates Catch2 (already compiled in CI) and `std::ranges` (needs the tool to match in a header). Counted by file the picture is harsher than by construct (7 of 146 units with nothing refused or open on 8d0aac109); report both.

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

## Tool PR (next after #1209; agreed 2026-11-21)

- Rule 2 refuses `guessed_concat` (a body using `+` that a standard concept does not name) and `guessed_power` (cases told apart by `E`, not by the argument's type); fixtures in the listings' shapes; then gate `paper_listings` at the counts.
- `rule3-bounded-fold`: a `fold_left` over `iota` (the kernel's iterator) is not a finding.
- Admission by operator: `BinaryOperator` split, assignment refused (both cold readers).
- Copilot on #1209: the atom key of a constraint renders only a value argument's digits, so `C<T, 1>` and `C<T, 1u>` are one fact; add the canonical integral type and a fixture (tools/jlt-lint/tool/jlt_lint.cpp, near line 1548).

## Prose pass (after the tool PR; agreed 2026-11-21)

Order agreed: merge #1209, then the tool PR, then this. Inputs: the two cold reads of 2026-11-21 evening (`research_notes/Jlt cold reads/`), the rule "terms of art once, with the citation, then plain words" (most of the cut from 28 to 22 pages).

- The trait program as logic, one paragraph in Section 3, each term once with its citation:
  - an entry at a composite is a definite Horn clause (Horn 1951), an axiom a fact (empty body); what is taken on trust is the program's facts AND rules, one list (answers "lemmas are not on the list"); plain word in the prose: "a derived law" or "a conditional law", never "rule" (Rules 1 to 3) nor "lemma";
  - the laws a program holds are the least model, the least fixed point of the immediate-consequence operator (Tarski 1955; van Emden and Kowalski 1976);
  - Datalog: without function symbols Horn clauses terminate; ours have type constructors, but a body reads only proper parts of the head, so resolution descends on type size (Rule 3 at the level of types);
  - negation `!C<T>` is negation as failure, sound when stratified (Apt, Blair and Walker 1988); check first whether any law body negates; gives the image of a negated constraint the theoretician asked for;
  - closed world (Datalog: absent = false) against the posture's open world ("false means not known"); the compiler computes the least model, the posture refuses to read absence as refutation;
  - the standard's "models" is Tarski's satisfaction (one sentence at the satisfies/models footnote).
  Abiteboul, Hull and Vianu (in the bibliography) covers Datalog, fixed points and stratification. Verify every other citation before adding. Links to #908 (variance as a logic program).
- FROZEN 2026-11-21 (user): the kernel's traits give `bool`. (1) `true` = declared: trusted, on the list, the program's meaning conditional on it; (2) anything else means nothing, for now: `!` is negation as failure ("not derived"), the program stratified; (3) a case chosen on a law computes the same value as the general case wherever the law holds, so acting on a declaration only ever optimises. All three are in the paper already but scattered (2.2 p. 9, 6.1 "false means not known", 2.3 "both cases would agree"); gather them as three lines in the logic paragraph of Section 3, and state (3) as the condition, not as an observation. Framing: a law's state of knowledge is a value in K3 = {refuted < unknown < holds}; the kernel's trait is its alpha-cut at "holds" (the library's `alpha_cut`, order/runs.cppm), `!` the complement of that cut; strong negation ("refuted", by a witness) is the cut at "unknown", deferred to the follow-up paper with ternary values; both cuts free give Belnap's four values. The listings already comply: `power` chooses by subsumption, no negation; the one `!homomorphism<NonEmpty, Concat, All>` is a test that a law is not derived, so say "not derived", never "refuted".
- The theoretician's terms: "System F" once as "the prenex fragment"; "EDSL whose domain is System F" becomes "profile"; "structurally typed": the laws are nominal opt-ins; GHC comparison: instance contexts are ignored by GHC, compare with instance chains (Morris and Jones, in the bibliography).
- Prior work to verify and cite: Schupp et al. (Simplicissimus, CC 2001), Tang and Järvi (LCSD 2007), Gottschling and Lumsdaine (GPCE 2008), Dos Reis, Stroustrup and Merideth (N2887), Concepts Lite/TS (Sutton and Stroustrup), Siek (LNCS 7470), Jones (PEPM 1994, dictionary-free overloading by partial evaluation), Voigtländer (ICFP 2009).
- Appendix G: units fully admitted (the harsh count) beside the construct share; drop "so a zero is not vacuous"; state the library's shortfall up front.
- The abstract's promise (user's ledger): both readers read "a check that a tool performs", "comes out as a list" and "preserves types and evaluation" as contradicting Section 3.2.

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
