# Loop citations: verification notes (2026-11-23)

Claim to support: a "counted" loop (bound known before the first step; counter and bound not assigned in the body; no break, return or goto inside) is sugar for a fold over 0..n-1 and always terminates; a loop without those conditions is not.

---

## 1. Meyer and Ritchie 1967: VERIFIED (from the paper itself)

Source read: scan with text layer, https://people.csail.mit.edu/meyer/meyer-ritchie.pdf (5 pages, running head "Proceedings A.C.M. National Meeting, 1967", page numbers 465 to 469 printed on the pages).
Metadata cross-checked with Crossref for DOI 10.1145/800196.806014: title "The complexity of loop programs", container "Proceedings of the 1967 22nd national conference", pages 465-469, ACM Press, event in Washington, D.C.
Affiliations on page 465: Meyer, IBM Watson Research Center; Ritchie, Harvard University.

LOOP semantics, quoted (p. 466):

> Specifically if P is a Loop program, and the integer in X is x, then "LOOP X, P, END" means that P is to be performed x times in succession before the next instruction, if any, after the END is executed; changes in the contents of X while P is being repeated do not alter the number of times P is to be repeated. The final clause is needed to ensure that executions of Loop programs always terminate.

Followed by the example `LOOP X / X = X + 1 / END`, which "is a program for doubling the contents of X, rather than an infinite loop".
Instructions are exactly five: `X = Y`, `X = X + 1`, `X = 0`, `LOOP X`, `END` (no jumps at all).

Termination, quoted (p. 467): "Corollary. If a Loop program computes a function f, then f is totally defined and is effectively computable. This amounts to saying that computations defined by Loop programs are always finite no matter what the initial contents of the registers may be, i.e., Loop programs always halt."

Primitive recursion, quoted: Introduction (p. 465): "The functions computable by Loop programs are, in fact, precisely the primitive recursive functions."
Theorem 1 (p. 466): "Every primitive recursive function is computed by some Loop program."
Theorem 7 (p. 469 by page order; final page): "The functions computable by Loop programs are precisely the primitive recursive functions."
(Theorem 7's page number is inferred from the order of the extracted text; check on the scan before citing a page.)

```bibtex
@inproceedings{MeyerRitchie1967,
  author    = {Meyer, Albert R. and Ritchie, Dennis M.},
  title     = {The Complexity of Loop Programs},
  booktitle = {Proceedings of the 22nd ACM National Conference},
  year      = {1967},
  pages     = {465--469},
  publisher = {ACM},
  address   = {Washington, D.C., USA},
  doi       = {10.1145/800196.806014}
}
```

What the claim should NOT say:
- The count is fixed at entry *by the semantics of LOOP* (the body may change X); it is not a restriction that the body leaves the counter alone. A C `for` gets the same effect only by the syntactic restriction (counter and bound not assigned). So: "the counted loop has Meyer and Ritchie's LOOP semantics", not "LOOP forbids assigning the counter".
- Meyer and Ritchie do not speak of folds; "sugar for a fold over 0..n-1" is the paper's own reading (bounded iteration = iteration of the body n times, i.e. the fold of the naturals). Cite them for termination and for "exactly primitive recursive", not for the fold.
- LOOP has no break/return/goto at all; the paper says nothing about them. The "no break" condition is ours (or OpenMP's).
- "Exactly the primitive recursive functions" holds for LOOP programs over naturals with those five instructions; it does not transfer to C++ counted loops over arbitrary bodies (bodies that call functions with unbounded loops, etc.) without argument.

---

## 2. OpenMP canonical loop nest form: VERIFIED (from the 6.0 and 5.2 PDFs)

Current spec: OpenMP API Version 6.0, November 2024.
PDF: https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-6-0.pdf
HTML: https://www.openmp.org/spec-html/6.0/openmp.html
Section 6.4.1 "Canonical Loop Nest Form", pp. 196-201 (Chapter 6 "Base Language Formats and Restrictions", Section 6.4 "Loop Concepts").
In 5.2 (Nov 2021, https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-5-2.pdf) the same text is Section 4.4.1 "Canonical Loop Nest Form", p. 85 ff.
(In 5.0 it was 2.9.1 "Canonical Loop Form"; in 5.1, 2.11.1.)

Quoted from 6.0, Section 6.4.1:

- Grammar: `for (init-expr; test-expr; incr-expr) loop-body`, `init-expr` is `var = lb` or `integer-type var = lb` (C also pointer type, C++ also random-access-iterator type); `test-expr` is `var relational-op ub` or `ub relational-op var` with relational-op one of `< <= > >= !=`; `incr-expr` is one of `++var`, `var++`, `--var`, `var--`, `var += incr`, `var -= incr`, `var = var + incr`, `var = incr + var`, `var = var - incr`.
- Counter: "The loop-iteration variable var must not be modified during the execution of intervening-code or loop-body in the loop." (p. 200)
- Bounds: lb, ub are "Expressions of a type compatible with the type of var that are loop invariant with respect to the outermost loop" (or affine in an outer loop's variable, for non-rectangular nests). (p. 200)
- Step: "a1, a2, incr: Integer expressions that are loop invariant with respect to the outermost loop of the loop nest. If the loop is associated with a directive, the expressions are evaluated before the construct formed from that directive." (p. 201)
- Direction: "If test-expr is of the form var relational-op b and relational-op is < or <= then incr-expr must cause var to increase on each iteration of the loop." (and symmetric cases); "If relational-op is != then incr-expr must cause var to always increase by 1 or always decrease by 1 and the increment must be a constant expression." (p. 201)
- Break: "final-loop-body must not contain any break statement that would cause the termination of the innermost loop." (p. 201); intervening code "must not contain iteration statements, continue statements or break statements that apply to the enclosing loop." (p. 198)
- Structured block: "A loop-nest must also be a structured block." (p. 202)
- Trip count: Section 6.4.3 "OpenMP Loop-Iteration Spaces and Vectors" (p. 203): "The canonical loop nest form allows the iteration count of all affected loops to be computed before executing the outermost loop."

Return/goto come in through Section 6.3 "Structured Blocks" (6.0): a structured block is "an executable statement, possibly compound, with a single entry at the top and a single exit at the bottom" (glossary), with restrictions "Entry to a structured block must not be the result of a branch." and "The point of exit cannot be a branch out of the structured block." (C++ adds throw, co_await, co_yield, co_return; C adds setjmp/longjmp.)

```bibtex
@manual{OpenMP60,
  author       = {{OpenMP Architecture Review Board}},
  title        = {{OpenMP} Application Programming Interface, Version 6.0},
  year         = {2024},
  month        = nov,
  note         = {Section 6.4.1, Canonical Loop Nest Form},
  url          = {https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-6-0.pdf}
}
```

What the claim should NOT say:
- OpenMP never says the loop "terminates" or is "a fold"; it says the iteration count can be computed before the outermost loop. Termination of the counted loop follows only if the body terminates. Cite OpenMP for "trip count computable before the first step", not for termination.
- OpenMP permits `!=` (with step plus or minus 1) and pointer / random-access-iterator counters, and non-rectangular bounds; the paper's counted loop may be narrower. Say "close to" or "a special case of" OpenMP's form if so.
- The ban on `return`/`goto` is not in 6.4.1 itself but in the structured-block rules (6.3); cite both.
- OpenMP 6.0 says unspecified behaviour if an intermediate result of the iteration-count computation overflows (6.4.3); do not claim the form alone excludes overflow.

---

## 3. MISRA C:2012 Rule 14.2 and MISRA C++: PARTLY VERIFIED (secondary sources only)

The MISRA documents were not reached; everything below is from vendor checker documentation.

MISRA C:2012 Rule 14.2, Required, headline "A for loop shall be well-formed".
Source: MathWorks Polyspace Bug Finder, https://www.mathworks.com/help/bugfinder/ref/misrac2012rule14.2.html (same number and headline in MISRA C:2023 and C:2025, pages misrac2023rule14.2 / misrac2025rule14.2).
The Polyspace page says it treats "the last assigned variable of the first expression" as the loop counter, and it flags: a first expression that does not initialise the counter or has another side effect; a second expression with no comparison on the counter, or that uses an object used in the body, or has a side effect; a third expression doing anything but updating the counter, or using an object used in the body; the counter modified in the loop body; more than one loop counter.
Exception (vendor summaries, consistent): a `for` with all three clauses empty, `for (;;)`, is allowed, so infinite loops are allowed.
MISRA forum (user post, not official): third clause "shall not use objects that are modified in the for loop body".

UNVERIFIED, from memory, of the MISRA amplification: there is one loop counter, of essentially integer type, not modified in the body; first clause empty, or assigns or defines and initialises the counter; second clause has no persistent side effects, uses the counter and optionally loop control flags (essentially Boolean), and uses no other object modified in the body; third clause's only persistent side effect is to modify the counter, and it uses no object modified in the body. Check against the document before quoting.

MISRA C++:2008 (headlines via MathWorks Polyspace pages misrac2008rule651..656, all Required):
- 6-5-1 "A for loop shall contain a single loop-counter which shall not have floating type."
- 6-5-2 "If loop-counter is not modified by -- or ++, then, within condition, the loop-counter shall only be used as an operand to <=, <, > or >=." (MathWorks truncates after "shall only be used..."; the end is from memory and its summary, unverified verbatim.)
- 6-5-3 "The loop-counter shall not be modified within condition or statement."
- 6-5-4 "The loop-counter shall be modified by one of: --, ++, -=n, or +=n; where n remains constant for the duration of the loop."
- 6-5-5 "A loop-control-variable other than the loop-counter shall not be modified within condition or expression."
- 6-5-6 "A loop-control-variable other than the loop-counter which is modified in statement shall have type bool."

MISRA C++:2023 Rule 9.5.1, Advisory, "Legacy for statements should be simple" (MathWorks, https://www.mathworks.com/help/bugfinder/ref/misracpp2023rule9.5.1.html).
Simple means: init-statement only declares and initialises a loop counter of integer type; condition compares counter to bound with `<, >, <=, >=`; counter modified only by incrementing or decrementing by a loop step in the increment-statement; bound and counter of the same type (or bound a constant expression in range); "The loop bound and loop step are constant expressions or variables that are not modified within the for statement"; counter, bound and step not bound to non-const references nor have their address taken.
MathWorks adds: "If using a simple for statement, make certain the loop progresses and terminates." (so the rule itself is not presented as guaranteeing termination).

```bibtex
@manual{MISRAC2012,
  author       = {{MISRA}},
  title        = {{MISRA C:2012}: Guidelines for the Use of the {C} Language in Critical Systems},
  organization = {HORIBA MIRA Limited},
  address      = {Nuneaton, UK},
  year         = {2013},
  month        = mar,
  note         = {Rule 14.2, ``A for loop shall be well-formed''}
}

@manual{MISRACpp2008,
  author       = {{MISRA}},
  title        = {{MISRA C++:2008}: Guidelines for the Use of the {C++} Language in Critical Systems},
  organization = {MIRA Limited},
  address      = {Nuneaton, UK},
  year         = {2008},
  note         = {Rules 6-5-1 to 6-5-6}
}

@manual{MISRACpp2023,
  author       = {{MISRA}},
  title        = {{MISRA C++:2023}: Guidelines for the Use of {C++17} in Critical Systems},
  organization = {The MISRA Consortium Limited},
  address      = {Norwich, UK},
  year         = {2023},
  month        = oct,
  note         = {Rule 9.5.1, ``Legacy for statements should be simple''}
}
```
(Publisher names, months and the C++:2023 subtitle are from memory: UNVERIFIED. MISRA C:2012 has later editions: Amendments 1 to 4, MISRA C:2023, MISRA C:2025; Rule 14.2 keeps its number.)

What the claim should NOT say:
- MISRA C:2012 14.2 does NOT forbid `break`, `return` or `goto`; those are separate rules (from memory, unverified: 15.1 goto advisory, 15.4 at most one break or goto per loop, 15.5 single point of exit advisory). Do not cite 14.2 for "no break/return/goto".
- 14.2 permits `for (;;)`, and MISRA C++:2023 9.5.1 is advisory and does not claim termination; neither says a well-formed loop terminates. Cite them for "practice restricts for loops this way", not for termination.
- MISRA C:2012 does not by itself require the bound to be invariant; it requires the second clause not to use objects modified in the body (close but not the same as "bound known before the first step"). MISRA C++:2023 9.5.1 is the one that states bound and step "not modified within the for statement".
- Do not quote the MISRA amplification verbatim from these notes; only the headlines are confirmed.
