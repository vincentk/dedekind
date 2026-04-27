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

## The structural pull this design resists

A common drift in production systems is to pick a single "Über-Carrier" — typically the *largest* member of an implicit lattice — and route every value through it: every numeric quantity becomes a `double`; every boundary value becomes a `string`. This isn't a community failing or an engineering mistake; it lies in the nature of things. The collapse is the local optimum: wire-format simplicity, ergonomic uniformity, fewer template parameters at every interface, "the runtime handles it." Nothing about the daily pressure of building software pulls *toward* preserving a finer carrier distinction; everything pulls *away* from it.

The structural cost is the same in every direction the collapse takes:

- **Promote-everything** collapses the lattice *upward*: ℕ becomes ℤ becomes ℝ becomes the runtime float. Provenance is destroyed; the smaller carrier's invariants (non-negativity, integrality, exactness) are gone the moment a value passes through the Über-Carrier. A function expecting an integer accepts the bit-pattern result of a floating-point computation.
- **Project-everything** collapses the lattice *downward through serialisation*: ℝ becomes a `string`, then back to ℝ on the other side, with the round-trip inheriting whichever loss the parser introduces. Type information has to be reconstructed at every boundary, and the reconstruction can silently disagree with the original.

The carrier-strength-reduction rule is the explicit counter-pull. Instead of *widening everything* to a common carrier, the meet operation *tightens to the smaller carrier* whenever the lattice admits it; the union widens to the larger only when the math actually requires it. Provenance is preserved at the type level; the structure-forcing step (subtraction → ℤ, division → ℚ, completion → ℝ) is named explicitly. The library spends some ergonomic budget here so that the type system has something left to enforce downstream.

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

This codebase ships option **(b)** as an **existential proof** on the variant pair `(Cardinality, SignedCardinality)` — a hand-coded overload pair on `Set::operator&` and `Set::operator|` that demonstrates the rule operationally:

```cpp
// Set<ℕ> & Set<ℤ> tightens to Set<ℕ>
const auto meet = positive_n & bounded_z;
STATIC_CHECK(std::same_as<typename decltype(meet)::Domain, ℕ>);

// Set<ℕ> | Set<ℤ> widens to Set<ℤ>  ({1} ∪ {-1} ⊂ ℤ)
const auto union_set = one_n | neg_one_z;
STATIC_CHECK(std::same_as<typename decltype(union_set)::Domain, SignedCardinality>);
```

The general framework — a `carrier_lattice_meet_t<T1, T2>` trait covering every pair in the lattice, including ℝ / ℂ / 𝔻 — is the scope of #362 proper, tracked as a Paper-3 blocker.

## Recommendations

1. **Keep the type-level distinction.** The embedding view is the right default for a language with a strong type system; provenance is worth its ergonomic cost.
2. **Bridge with cross-type operators where the math is unambiguous.** Comparison (PR #423, #425, #428) is the canonical example.
3. **Add carrier-promotion rules to binary set operations** (a follow-up PR, not yet filed). This is the operational realisation of (b) above; without it, `Set<ℕ> ∩ Set<ℤ>` doesn't compile and the strict-vs-relaxed dichotomy leaks into the DSL surface.

## Pointers

- `Cardinality` / `SignedCardinality` carrier definitions: [`src/main/modules/dedekind/sets/cardinality.cppm`](../../src/main/modules/dedekind/sets/cardinality.cppm).
- The canonical embeddings: `embed_𝔹_ℕ` in [`numbers/naturals.cppm`](../../src/main/modules/dedekind/numbers/naturals.cppm), `embed_ℕ_ℤ` / `embed_K3_ℤ` in [`numbers/integer.cppm`](../../src/main/modules/dedekind/numbers/integer.cppm), `embed_ℤ_ℚ` / `embed_ℚ_ℝ` / `embed_ℝ_ℂ` in [`numbers/rational.cppm`](../../src/main/modules/dedekind/numbers/rational.cppm).
- Cross-type comparison ops: same `cardinality.cppm` file (the heterogeneous `<=>` / `==` overloads).
- Witnesses: [`src/main/modules/dedekind/numbers/cardinality.cppm`](../../src/main/modules/dedekind/numbers/cardinality.cppm) (`HasPartialOrderOperatorsWith<…>` static_asserts).

## The honesty obligation

The carrier-lattice design is one local instance of a wider principle that runs through the project: the engineer carries the honesty obligation up front, and the compiler discharges everything that follows mechanically. Choosing the right carrier (ℕ for ℕ, not `unsigned int`; ℤ for ℤ, not `int`); spelling the canonical embeddings explicitly; populating the trait registry with claims that are constructively true of the carrier (`is_associative_v<T,Op>` only where the carrier supports it operationally); refraining from `reinterpret_cast` and undefined-behaviour escape hatches in the public surface — these are decisions the type system cannot make and cannot police. They are the load-bearing trust commitment of the library, and they live at a single layer (the trait registry, the embedding arrows, the carrier choices) rather than diffused across runtime checks.

Once those choices are honest, `clang++` does the rest: every `static_assert` discharged, every `requires`-clause checked, every NTTP-folded `constexpr` evaluation, every IR-level constant fold derived from the structural knowledge. The "poor man's" qualifier we attach to this use of the compiler is structural, not economic: `clang++` *is* a formal system, and a typing derivation it accepts is a proof in the propositional fragment. What it cannot do is decide whether the trait registry is honest in the first place — that is the engineer's job, and it is the same job the carrier-lattice design names explicitly.

The Über-Carrier collapse, in this framing, is what happens when the honesty obligation is silently waived: the engineer accepts a single carrier "for ergonomic reasons" and the trait registry's claims gradually drift away from what the carrier actually supports. The carrier-strength-reduction rule is the operational counter-pull — it spends a little ergonomic budget keeping the carriers distinct so the trait registry can stay honest, and the compiler can keep doing the mechanical half of its job.

## Note on the Feynman / Grothendieck construction

The "subset vs. embedding" question is the operational shadow of how ℤ is *constructed* from ℕ in the math literature. Feynman's lectures (and any standard algebra text) build ℤ as the Grothendieck group of (ℕ, +) — formally adjoining additive inverses by introducing `operator-`. ℕ has no subtraction; ℤ is what you get when you force closure under it.

The carrier choice in this codebase mirrors this: `Cardinality` deliberately omits `operator-` (keeping ℕ honest as a rig); `SignedCardinality` adds it (the ℤ escape). The `embed_ℕ_ℤ` arrow is the relational shadow of the Grothendieck embedding. C++'s integer-promotion rules (`unsigned × signed → ?`) are the same phenomenon expressed at the standard-library level: closure under signedness is the structure-forcing axiom.
