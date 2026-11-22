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

## Admit by type (agreed 2026-11-22, stacked on the tool PR)

**Core idea (user's words, 2026-11-22):** the linter today *refuses by name*; it should also *try to admit by type-check* what it does not refuse. It comes with a library of archetypes for that, part of the linter and shipped with it to its users, not part of dedekind. The paper's claims are updated to state the hardening, with summary statistics in the appendix. It is code in the linter, not prose in the paper.

- **Two stages, three outcomes, per definition.** (a) The filter: the deny-rules, cheap, on syntax and names; a finding is a witness that a definition is outside Jlt. (b) The check: instantiate each constrained template not refused at the archetypes of the shapes in its signature and compile with `-fsyntax-only`; a definition that type-checks is *admitted by type*. Outcomes: refused (a witness), admitted (a certificate), undecided (neither: no archetype could be built, or it failed with no deny-rule to say why; the posture's exercise looks there). This is the three-valued reading of 2026-11-21 (the second alpha-cut, refutation by witness) in the tool, while the kernel's traits stay Boolean.
- **Why the fragment makes it work.** Modular type checking (a definition checked once against its constraints, for every model) fails for C++ in general: a real type has more than the archetype (extra overloads, ADL, specialisations, conversions). The rules close each hole: operations are function objects reached through named types, Rule 2 decides every resolution, carriers are only `copyable` and required calls have `same_as` results, entries are declared above their uses. Conjecture to state and test, not prove: *under the rules, a definition admitted at its archetypes is well-typed at every model and resolves the same way.* The archetype generator is the System F image of Section 3.2 as C++: a shape becomes a record of exactly its calls and laws.
- **Archetypes are generated, not written per concept.** One rule per kind of piece in a shape (Figure 2): another shape (recurse), a law (`template <> constexpr bool associative<Arch> = true;`), an associated type through a trait (`op_of<Arch>::type`), a required call with a fixed result type, the unit (`E{}()`), the carrier (`copyable`). Effort grows with the grammar of shapes, not with the number of concepts. A shape with other pieces leaves its definitions undecided. Listing 1's `BareMonoid` is a hand-written archetype; the tool builds such types itself.
- **Shipped helpers:** `tools/jlt-lint/archetypes.hpp`: `carrier<Tag>` (only `copyable`), `nullary<C>`, `binary<C>`, `arrow<A, B>`, and the one hand-written archetype the standard needs for the listings, `sized_range_of<C>`. Precedent: Boost's `concept_archetype.hpp` (Siek). Laws and associated types are always generated, since they name the shapes' owner's traits.
- **Mechanics:** per source file, one generated translation unit: import the file's module, declare the archetypes and their trait entries, explicitly instantiate each constrained template at them, compile with the file's flags from the compile database, `-fsyntax-only`. A compile error in the original body is "not admitted" for that definition. About 150 extra syntax-only compiles in CI, in parallel, in the lint step.
- **Not the check's job:** overload choice stays with `rule2-decided-resolution` (at its archetype `guessed_power` type-checks; only its choice is undecided). Compile-time evaluation is out of reach (a stand-in has no values).
- **Spike first, on the four listings:** expected: `concat`, `power`, `named_power`, the homomorphism laws and `free_theorem` admitted; `guessed_concat` not admitted (its `+` has no promise); `guessed_power` admitted by type and refused by stage (a). Risks to try on day one: specialising the library's traits from another translation unit under modules; what `copyable` grants (it includes assignment, so `s = s + x` fails only on `+`; decide what a carrier's archetype may have); associated types through `decltype(E{}())`.
- **Then:** generalise to the library; the coverage report per definition (admitted / refused / undecided; files and lines summed from it), printed by CI and tabulated in Appendix G; Table 13's admission by syntax kind becomes the rough first view or goes; Section 4 and the opening say "refuses by name, admits by type", the conjecture stated with its hypotheses.
- **Order:** flip #1210; ratchet and summary; this spike and its generalisation; the posture's exercise (with findings by type to triage as well).
- **Decided 2026-11-22 (user), Appendix A's table of derived forms (old Appendix A trimmed):** lambdas (not generic, or constrained) are their closure class; counted `for` and range-`for` over a sized range are folds; assignment and `++` on locals are the fold's state (runST); `std::span<const T>` over storage declared constant (`constant_storage<S>`, an axiom) is a range; raw pointers excluded because their conditions (never null, never compared across objects) cannot be checked well, though their arithmetic is count arithmetic. **Linter tasks for the library run:** judge lambda bodies like function bodies (today exempt); recognise the counted loop shape (one unsigned type, body writes neither counter nor bound) so `rule3-no-loops` stops refusing it; regenerate Table 13 under the new classes. **Prose-pass notes from the trimmed text:** constraint inference as the road not taken for `guessed_power` (its resolution would become a parameter, and the free theorem would range over it), one sentence in Section 2.3; `std::reduce` and a parallel `concat` justified by the free monoid's universal property, for Section 5 (the practitioner asked for it); the bibliography has `launchbury1994st` and `launchbury1994lazy` for one paper.
- **Decided 2026-11-22 (user), for the library run:** `{ e } -> convertible_to<T>` is a DERIVED FORM: the call followed by `T(e)`, after which the kernel sees `same_as<T>`; it keeps values only where the result is used at `T`, which `rule2-converted-result` checks (by name today). Implicit conversions between atoms (int to bool, bool promoted to int; Listing 1's `true + true` is one) are the same derived form; count them in the `definitions` report by conversion kind. The generator gets an archetype `convertible<T>` (a fresh type with only `operator T()`), so a result used at another type fails the type check: the converted-result rule by type. Locked in Section 3.1 already; Table 14 gains its row when the generator implements it. Also decided: an integer conversion written as a call is kernel; a member call is a derived form (admission.txt).
- **#1211's deliverables (user, 2026-11-22):** the listings reach the highest level the approach gives (FJlt where no derived form is used, Jlt otherwise, the two planted counterexamples refused), each level produced by the linter and gated; and a prose pass on the paper stating that status as a generated table, how it was achieved, and what it means for the code (the guarantees, the conjecture, the counterexamples). Then local cold reads, one Copilot round, merge when no real bug remains.

## The good dialect (case by case, user, 2026-11-22)

The library and the listings speak two dialects; the first sample (`category`, 382 constrained function templates, all undecided) measured it. The user's rule: find the good dialect, whichever it is, then migrate towards it.

- **Case 1, the monoid: DECIDED, the listings' dialect.** A monoid is named by the type of its unit `E` (carrier `decltype(E{}())`, operation through the trait `op_of<E>`, laws as variable templates keyed on `E`). Taken from the library: totality as a law, `total<E>` (the library's honest rejection of `int` under `+`). Migration: a bridge `monoid_of<T, Op>` reads the library's pairs as an `E` (its `op_of` entry and laws from the pair's traits), new code is written in `E`, each concept migrates when next touched, the ratchet counts the remaining `(T, Op)` uses; a default unit per carrier (`default_monoid<T>`) keeps call sites like `concat(r)` short where a default is wanted. Checked against recency bias: families of carriers (partial specialisation of variable templates) and semirings (laws on two unit types) lose nothing; the one cost, inference at call sites, has the default as its answer.
- **Case 2, the arrow: DECIDED, the library's dialect** (user: "the library's semantics seem more mature"): `IsArrow`, `Domain` and `Codomain` members, `const`-invocable, the result `Presents` the codomain. Its syntax may need adjustment, and it is to be checked against the kernel and the derived forms (Table 7), which did not exist when the library was started. Listing 4 moves to it: its arrows declare their ends, and a composition replaces `Via<A, F, B, G, C>`. Order agreed: the generator reads the arrow dialect, the `category` sample is rerun, the arrow is checked against kernel and derived forms, Listing 4 is migrated (shown to the user first), then the monoid bridge.
- **Next cases:** the standard concepts the archetypes need (`regular`, `semiregular`, `invocable`, `integral`), and the 97 constraints that are operator calls (to be identified).

## The library run (next PR after #1211; agreed 2026-11-22)

Goal (user): firm the situation up with a decent number of samples. #1211 shows the second stage on the paper's own seven function templates; this PR runs it where its author did not write the idioms.

1. **Run the second stage on the library** (rules `rule2-by-type` and `admitted` over every module, report first) and on a control not written under the posture (Catch2, or a generic library such as a ranges or units library). Report admitted / not admitted / undecided, by definition and by line, with a histogram of the undecided reasons: that histogram is the list of generator rules worth writing next.
2. **A fixture per counterexample of the conjecture** (Appendix G.1): a model whose operation takes `C&` (value category); an unqualified call by name captured by ADL; two carriers that coincide (`std::get<T>` on `pair<T, T>`); a choice delegated to the caller (`named_power` at `Max`); each must come out undecided or be excluded by a deny-rule, and the hypothesis it maps to is named in the fixture.
3. **Generator coverage, as the histogram demands:** member and class templates (today skipped); the `convertible<T>` archetype (only `operator T()`); implicit conversions between atoms counted by kind; lambda bodies judged as function bodies; the counted-loop shape recognised so `rule3-no-loops` stops refusing it; more standard concepts (`totally_ordered`, `invocable`, `forward_range`); a whole-file re-parse per template is slow, batch per file when it matters.
4. **The ratchet and summary statistics** (agreed 2026-11-22): every reported count becomes an exact gate, `ratchet.py` rewrites the scope from a report, CI prints a coverage line (FJlt / Jlt / undecided / refused, by definition and by line), and Appendix G tabulates it; Table 13 regenerated under the current classes.
5. **The paper** says what the run found: Appendix G's numbers, and Section 4's sentence "on the library the second stage has yet to run" replaced by the result.

Decided along the way (2026-11-22, user):
- **Constraints that come with a type in the signature:** a parameter's class template contributes its constraints (Rust's implied bounds, RFC 2089); a result type's do not, and a template whose result asks more than its constraints is refused. Cited in the doxygen where applicable.
- **The standard's foundational concepts** (Palo Alto report N3351 3.3; Elements of Programming 1.5): copying is kernel; same_as is kernel (one carrier per class of equations); equality is Jlt (a call); a default value is Jlt (partially formed, local state written before read); regular is both. The library's arrow archetype has no default value unless asked.
- **convertible_to, as if same_as:** the result is a `converts_to<T>`, converted only from the fresh value, and only into a declared T or a return at T (Table 7's copy-initialisation); passed straight to a call it is refused (fixture `nested`), since a real type's overloads may resolve there differently. No new hypothesis for the conjecture.
- **Integer parameters, next:** check at the smallest and the largest of the standard's integer types that satisfy the template's constraints (bool where the constraint admits it; IsCountWord excludes it and every promoting type), signed and unsigned alike; no list per concept, no new hypothesis.
- **Signed integers** (user, 2026-11-22): allowed, as sugar, not kernel. The kernel's primitives are total, and signed + is not; a signed type is a carrier whose total operations (<=>, min, max) are declared calls, as the library has it (laws on ordering and lattices only, none on arithmetic). The kernel's atoms stay size_t and bool.
- **Integer parameters, implemented:** integral, signed_integral and unsigned_integral parameters are checked at the smallest and the largest of the standard's integer types the constraints admit, found by asking the constraints (archetypes.hpp: smallest_integer, largest_integer); an overloaded name keeps the single check at size_t for now. Fixtures summed (refused: at bool the sum is an int, braces do not narrow) and cast_sum (admitted).
- **Credit:** Boost's Concept Check Library and N3351 cited (introduction, Section 4, related work, Appendix G, archetypes.hpp); MP11 only as a contrast, in the prose pass.

## Prose pass (after the tool PR; agreed 2026-11-21)

Order agreed: merge #1209, then the tool PR, then this. Inputs: the two cold reads of 2026-11-21 evening (`research_notes/Jlt cold reads/`), the rule "terms of art once, with the citation, then plain words" (most of the cut from 28 to 22 pages).

- The trait program as logic, one paragraph in Section 3, each term once with its citation:
  - an entry at a composite is a definite Horn clause (Horn 1951), an axiom a fact (empty body); what is taken on trust is the program's facts AND rules, one list (answers "lemmas are not on the list"); plain word in the prose: "a derived law" or "a conditional law", never "rule" (Rules 1 to 3) nor "lemma";
  - the laws a program holds are the least model, the least fixed point of the immediate-consequence operator (Tarski 1955; van Emden and Kowalski 1976);
  - Datalog: without function symbols Horn clauses terminate; ours have type constructors, but a body reads only proper parts of the head, so resolution descends on type size (Rule 3 at the level of types);
  - negation `!C<T>` is negation as failure, sound when stratified (Apt, Blair and Walker 1988); check first whether any law body negates; gives the image of a negated constraint the theoretician asked for;
  - closed world (Datalog: absent = false) against the posture's open world ("false means not known"); the compiler computes the least model, the posture refuses to read absence as refutation;
  - the standard's "models" is Tarski's satisfaction (one sentence at the satisfies/models footnote).
  - contrast, one clause: the trait program computes no types, unlike a metaprogramming library (Boost.MP11, Hana; both already in references.bib as pdimov2017mp11 and the Hana entry, cited nowhere in Jlt); the Palo Alto report (stroustrup2012concept) claims the same distance for its concepts ("does not resemble template metaprogramming"). Credit is owed only for what is borrowed, and nothing is borrowed from MP11;
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
