# The carrier lattice: ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ at the C++ type level

## The question

Mathematically, ℕ is a *proper subset* of ℤ — every natural number is, literally, an integer. So `Cardinality` (the post-#402 ℕ-proxy) ought to behave as a subset of `SignedCardinality` (the ℤ-proxy), and operations that mix them — `5 < -3` ; `Set<ℕ> ∩ Set<ℤ>` — should compose without ceremony.

In a typed language, the textbook subset relation can be modelled in two distinct ways:

1. **Subset (relaxed).** Treat the smaller carrier as a literal subset; allow implicit conversions in both directions where the math admits them. `5 ∈ ℕ` *is* `5 ∈ ℤ`; the runtime representation may differ but the value is the same. C++ does this with arithmetic promotion (`unsigned` ↪ `int`, `int` ↪ `double`).
2. **Embedding (strict).** Treat the carriers as distinct types related by an explicit canonical arrow `ι : ℕ → ℤ`. Cross-carrier operations route through `ι`; equality and ordering are settled in the larger carrier; provenance is preserved at the type level.

The choice is not aesthetic. It controls (a) whether a function expecting `Set<ℤ>` silently accepts `Set<ℕ>`, (b) what `Set<ℕ> ∩ Set<ℤ>` returns, and (c) how much downstream code knows about the origin of a value.

## What this codebase does

**The embedding view at the type level.** `Cardinality` and `SignedCardinality` are distinct C++ types — different `std::variant<…>` instantiations, no implicit conversion between them. The arrows `embed_𝔹_ℕ`, `embed_ℕ_ℤ`, `embed_ℤ_ℚ`, `embed_ℚ_ℝ`, `embed_ℝ_ℂ` are the canonical injections; provenance is preserved.

**The subset view at the operator level.** Cross-carrier `==` / `<=>` *do* compose, routing through the canonical lift. `Cardinality{5} < SignedCardinality{-3}` returns `false` directly; `Cardinality{ℵ_0} == SignedCardinality{PositiveInfinity{}}` returns `true`. The relational surface honours the math without forcing call sites to spell `embed_ℕ_ℤ(card) < sc_value`.

The split is intentional: type-level distinction prevents accidental mixing (a `void f(Set<ℤ>)` doesn't silently accept a `Set<ℕ>` and lose the natural-number guarantee); operator-level cross-comparison reflects the math.

## Where the strict view holds without compromise

- **Function signatures.** A function declared on `Set<ℕ>` does not accept a `Set<ℤ>`. The user must be explicit if conversion is intended.
- **Algebraic concept witnesses.** `IsRig<Cardinality>` and `IsRing<SignedCardinality>` are pinned independently — flipping the carrier changes which structures are claimed.
- **NTTP wrappers.** `Singleton<V, L>::Domain = decltype(V)` carries the type of the literal verbatim; the variant carriers can't be NTTPs anyway, so nothing is lost.

## Where the relaxed view leaks back in (deliberately)

- **`<=>` and `==`** between any two carriers in the lattice that are operationally comparable (`Cardinality` ↔ `SignedCardinality`, both ↔ `std::integral`, both ↔ `std::floating_point` per #428).
- **Constructor implicit conversion** from `std::integral` into the variant alternatives' literal-constructor surface (`ExtensionalCardinal<>{5u}`, `SignedExtensionalCardinal<>{-3}`). This is the single direction where *value* lifting is allowed without ceremony — it's the call-site convenience that lets `Set{n % N | (n > 5u)}` look the way the math reads.

## Concrete pressure point: `Set<ℕ> ∩ Set<ℤ>`

The user-surfaced operational case. Two reasonable answers:

- **(a)** Forbid it. The user must lift `A : Set<ℕ>` into ℤ explicitly via `embed_ℕ_ℤ` then take the intersection in `Set<ℤ>`. Mathematically correct under the strict view; ergonomically heavy.
- **(b)** Allow it, with a *carrier-promotion* rule on the binary set operations:

  | Operation | Result carrier (mirrors the lattice) |
  |---|---|
  | `Set<X> ∩ Set<Y>` | the *smaller* of `X`, `Y` (intersection is contained in both) |
  | `Set<X> ∪ Set<Y>` | the *larger* of `X`, `Y` (union may include elements only in the larger) |
  | `Set<X> \ Set<Y>` | `X` (difference is contained in `X`) |

  The "smaller" and "larger" relations are defined by the embedding chain ℕ < ℤ < ℚ < ℝ < ℂ, with the embeddings doing the heavy lifting underneath.

Option **(b)** is what the math textbooks do silently. It composes; it preserves the type-level provenance (the result of `Set<ℕ> ∩ Set<ℤ>` is correctly typed `Set<ℕ>`); it doesn't require value-level implicit conversion *between* the carriers.

This codebase doesn't yet implement (b) — the binary set operations on the DSL currently require homogeneous carriers. Adding the carrier-promotion rule is a future PR.

## Recommendations

1. **Keep the type-level distinction.** The embedding view is the right default for a language with a strong type system; provenance is worth its ergonomic cost.
2. **Bridge with cross-type operators where the math is unambiguous.** Comparison (PR #423, #425, #428) is the canonical example.
3. **Add carrier-promotion rules to binary set operations** (a follow-up PR, not yet filed). This is the operational realisation of (b) above; without it, `Set<ℕ> ∩ Set<ℤ>` doesn't compile and the strict-vs-relaxed dichotomy leaks into the DSL surface.

## Pointers

- `Cardinality` / `SignedCardinality` carrier definitions: [`src/main/modules/dedekind/sets/cardinality.cppm`](../../src/main/modules/dedekind/sets/cardinality.cppm).
- The canonical embeddings: `embed_𝔹_ℕ` in [`numbers/naturals.cppm`](../../src/main/modules/dedekind/numbers/naturals.cppm), `embed_ℕ_ℤ` / `embed_K3_ℤ` in [`numbers/integer.cppm`](../../src/main/modules/dedekind/numbers/integer.cppm), `embed_ℤ_ℚ` / `embed_ℚ_ℝ` / `embed_ℝ_ℂ` in [`numbers/rational.cppm`](../../src/main/modules/dedekind/numbers/rational.cppm).
- Cross-type comparison ops: same `cardinality.cppm` file (the heterogeneous `<=>` / `==` overloads).
- Witnesses: [`src/main/modules/dedekind/numbers/cardinality.cppm`](../../src/main/modules/dedekind/numbers/cardinality.cppm) (`HasPartialOrderOperatorsWith<…>` static_asserts).

## Note on the Feynman / Grothendieck construction

The "subset vs. embedding" question is the operational shadow of how ℤ is *constructed* from ℕ in the math literature. Feynman's lectures (and any standard algebra text) build ℤ as the Grothendieck group of (ℕ, +) — formally adjoining additive inverses by introducing `operator-`. ℕ has no subtraction; ℤ is what you get when you force closure under it.

The carrier choice in this codebase mirrors this: `Cardinality` deliberately omits `operator-` (keeping ℕ honest as a rig); `SignedCardinality` adds it (the ℤ escape). The `embed_ℕ_ℤ` arrow is the relational shadow of the Grothendieck embedding. C++'s integer-promotion rules (`unsigned × signed → ?`) are the same phenomenon expressed at the standard-library level: closure under signedness is the structure-forcing axiom.
