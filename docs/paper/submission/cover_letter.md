# Cover letter (draft)

Dear Editors,

We submit *Rules on Buckets are Set* for consideration in
*Programming*.  The paper introduces the `dedekind` library, which
leverages C++23's concepts and non-type template parameters to
embed formal categorical structures into the type system at
translation time.

Our contribution is twofold.  First, a formalisation of the
*Computability Tier* lattice for compile-time sets, which gives a
precise vocabulary for "how much the compiler knows about a set"
and frames the paper's reduction theorems as monotone motion up
that lattice.  Second, mechanical evidence — via static_assert
witnesses and checked-in LLVM IR fixtures — that complex
algebraic reductions, including 2D linear-programming solvers and
4×4 matrix inversions, collapse into literal constants at
translation time, with the IR bit-identical across ARM and x86
targets.

The paper situates this discipline within the journal's
intersection of programming science and systems engineering, with
the embedded / edge-computing setting as the intended operating
point.

Best regards,

The `dedekind` Authors

---

*Refinement notes vs.\ the original draft:*

- Dropped "We believe this work aligns perfectly with the
  journal's focus": standard cover-letter register, but the
  paper's own voice avoids self-grading.  The technical
  description carries the alignment claim implicitly.
- Trimmed the standalone "extreme edge computing" coda; the body
  paragraph already names "embedded / edge-computing" as the
  intended operating point at the matching level of caution.
- Made the LP / matrix-inversion claim precise — "literal
  constants at translation time, with the IR bit-identical
  across ARM and x86 targets" — to mirror the paper's own §5.2
  / §5.6 wording rather than the more promotional "zero-
  instruction" framing.
