# Carrier-lattice lift unification — spec & design decision (#455)

**Status**: spec landed; minimal trait scaffold + one existential-proof
specialization (`ℕ → ℤ`); migration filed as follow-up.

**Decision**: **alias-only unification**, not categorical unification.
The unified surface is a discoverability trait, not a structural claim
that every lift is a monad/Kleisli morphism.

## Background

PR #451's carrier-lattice Figure 1 reified seven exported `arrow`
objects across the discrete-foundations slice. Their names are
bespoke per (from, to) pair:

```
lift_ℕ_ℤ_                       (variant-layer ℕ → ℤ)
embed_unsigned_Cardinality_     (machine → variant ℕ)
embed_int_SignedCardinality_    (machine → variant ℤ)
embed_ℕ_ℤ                       (machine sign reinterpretation)
embed_𝔹_ℕ                       (𝔹 → ℕ)
embed_𝔹_𝕂3                      (𝔹 → 𝕂3)
embed_K3_ℤ                       (𝕂3 → ℤ; skips machine row)
```

The user-surfaced question (#455): could / should these all be
unified under a single `monadic_lift<From, To>` API surface, indexed
by the lattice arrow they represent?

## Why "monadic" overstates the structure

Inspecting the seven arrows reveals three structurally distinct
families, NOT a single categorical concept:

1. **Set-theoretic mono inclusions** (no monad). Pure structural
   injections from a smaller set into a larger one. No partiality,
   no saturating sentinel, no Kleisli structure.
   - `embed_𝔹_ℕ`, `embed_𝔹_𝕂3`, `embed_K3_ℤ`, `lift_ℕ_ℤ_`.

2. **Machine → variant lifts** (partial under range). Each carries a
   `static_assert` guarding against `numeric_limits<size_t>::digits <
   numeric_limits<source>::digits`. On the safe range, these are
   structural injections; off the range they would silently truncate
   and break injectivity (which is why the guard exists).
   - `embed_unsigned_Cardinality_`, `embed_int_SignedCardinality_`.

3. **Machine sign reinterpretation**. The arrow `embed_ℕ_ℤ`
   (`arrow<unsigned, int>`) is `static_cast<int>(unsigned)` — a
   bit-pattern reinterpretation, not a value-preserving lift. UB on
   negative target if the source exceeds `INT_MAX`.
   - `embed_ℕ_ℤ` (machine).

A single `monadic_lift` API across all three would either:
- **Lie about the structure**: claim Kleisli / monadic content where
  none exists (most of these are pure mono inclusions).
- **Be cosmetic-only renaming**: a thin alias that adds no
  categorical guarantee and just shuffles names around.

## Decision: alias-only unification

The unification IS valuable — but for **discoverability and generic
code**, not for categorical content:

- `lift<From, To>(x)` as a function template that dispatches to the
  canonical bespoke arrow for each lattice pair via specialisation.
- The bespoke names (`embed_𝔹_ℕ`, `lift_ℕ_ℤ_`, etc.) remain canonical
  in the project; `lift<From, To>` is a discoverability alias on top.
- No claim of categorical unity. Documentation explicit that the
  unified surface dispatches to *whatever the canonical lift is* for
  each (From, To) — sometimes a Set-mono, sometimes a partial
  machine-to-variant lift with a range guard, sometimes a sign
  reinterpretation. The dispatching trait knows; the user doesn't
  have to.

### Architectural shape

```cpp
namespace dedekind::category {

// Function template: dispatches to the canonical lift for (From, To).
// Default: undefined (forces explicit specialisation).
template <typename From, typename To>
constexpr To lift(From const& x) = delete;

// Specialisations register the canonical lift for each lattice pair.
// Each specialisation reduces to a call to the bespoke arrow.
template <>
constexpr dedekind::sets::SignedCardinality
lift<dedekind::sets::Cardinality, dedekind::sets::SignedCardinality>(
    dedekind::sets::Cardinality const& n) {
  return dedekind::numbers::lift_ℕ_ℤ_(n);
}

// ... per-pair specialisations for the other six arrows.

}  // namespace dedekind::category
```

A reader who doesn't know the project's naming convention can write
`lift<ℕ, ℤ>(c)` and find the right embedding; the dispatch resolves
to the canonical bespoke (`lift_ℕ_ℤ_` here) at compile time. Generic
code that needs to lift across an unknown lattice pair binds to
`lift<From, To>(x)` and gets the right behaviour for whichever pair
the caller instantiates.

### What is NOT done

- No claim of compositionality at the type level. `lift<A, C>(x)` is
  NOT auto-derived from `lift<A, B>(lift<B, C>(x))` — each pair
  registers its own canonical lift, and the figure caption's
  "multiple paths commute up to canonical isomorphism" remains a
  prose claim, not a structural typeclass enforcement.
- No "Saturating-Maybe monad" framing. The machine→variant lifts
  carry partiality via `static_assert` guards, not via a monadic
  Maybe-style codomain. (If a future PR wants to expose the
  saturating semantics as a monad explicitly, that's its own concern;
  it doesn't belong in the discoverability alias.)
- No renaming of the bespoke arrows. They remain canonical;
  `lift<From, To>` is purely additive.

## Open questions left for the implementation PR

1. **Where does the trait live?** Probably `category:cartesian` (where
   relations on products and the lattice connectivity already
   reside) or a new sibling `category:lift` partition. Decide at
   implementation time.

2. **Which specialisations land first?** Recommendation: the **central
   variant-layer** lifts (`ℕ → ℤ`, `ℕ → ℝ` once available, etc.)
   first, since these are the ones a paper reader is most likely to
   try. Machine-layer specialisations second.

3. **Do we ship a `lift_t<From, To>` type alias too?** The
   function-template form covers value-level lookup; a sibling type
   alias exposing the canonical arrow's *type* (e.g. for use in
   `IsArrow` constraints) might be useful later. File as further
   follow-up if needed.

4. **Does `lift` co-exist with `monadic_lift` if a future PR genuinely
   reifies a Saturating-Maybe monad?** Yes — they would be different
   APIs (`lift` for discoverability dispatch; `monadic_lift` for the
   genuine Kleisli morphism). The naming distinction is preserved
   precisely to avoid the categorical-overclaim noted above.

## Sequencing

This spec PR ships:
- This design doc (the decision and rationale).
- The minimal trait scaffold + ONE existential-proof specialisation
  (`ℕ → ℤ` via `lift_ℕ_ℤ_`).
- A test case demonstrating the dispatch fires correctly.

The remaining six specialisations + paper-figure / report relabelling
land as follow-up under #455 (or a sibling issue if scope grows).

## References

- #451 — carrier-lattice Figure 1 + the seven bespoke arrows.
- #460 / PR #463 — function-as-relation primitive (`IsBinaryFunction`,
  parallel structural-classification taxonomy).
- #461 — paper-polish gate (this spec is one of its constituents).
- `feedback_paper_dense_exposition` — paper bias toward dense
  classification-table exposition; the lift-pair table fits naturally
  into a paper §"Carrier-lattice arrow taxonomy" subsection.
