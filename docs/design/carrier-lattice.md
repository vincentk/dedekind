# The carrier lattice: ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ at the C++ type level

## The discrete-foundations slice

```
                       SignedCardinality (ℤ-Form: Initial Ring / Grothendieck of ℕ)
                      ╱     ↑
   embed_int_                │ lift_ℕ_ℤ_  (canonical ℕ ↪ ℤ embedding;
   SignedCardinality_       │              Grothendieck-construction unit
                  ╱         │              at object ℕ)
                int  ←──── Cardinality  (ℕ-Form: NNO)
                 ↑          ↑     ↑
        embed_K3_ℤ         │     │ embed_unsigned_Cardinality_
                 │   embed_ℕ_ℤ    │ (PR #441, the named monic arrow)
                 │   (machine)    │
              Ternary       unsigned int  (= ℤ/2^wℤ, the modular finite ring)
                              ↑
                              │ embed_𝔹_ℕ
                              │
                            bool (𝔹-Form: Ω, the Subobject Classifier)
```

Three layers: truth-value carriers (`bool`, `Ternary`); machine integer carriers (`unsigned int` = ℤ/2^wℤ; `int` = machine ℤ with UB-on-overflow); variant carriers inhabiting the textbook Forms (`Cardinality` = ℕ-as-NNO; `SignedCardinality` = ℤ-as-Initial-Ring / Grothendieck-of-ℕ). Each labelled arrow is an exported `arrow` object in `dedekind.numbers` registered as monic via `is_monic_arrow_v`; the carrier-lattice test case in `initial_ring_test.cpp` pins the `IsMonicArrow` concept-level facts. The dashed retraction `abs : SignedCardinality → Cardinality` (PR #437/#436) is the split-mono partner of `lift_ℕ_ℤ_` — `abs ∘ lift = id` on the non-negative fragment; it is not monic itself — `abs` folds sign (`abs(3) == abs(-3)`), so distinct ℤ-values are conflated. The closure-forcing operator `Cardinality - Cardinality → SignedCardinality` is the Grothendieck-construction *multiplication* (the construction map on operator pairs); the *unit* is the embedding `lift_ℕ_ℤ_` shown above. Multiple paths between extremes commute up to canonical isomorphism.

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

This codebase ships option **(b)** as an **existential proof** on the variant pair `(Cardinality, SignedCardinality)` — a hand-coded pair of free-function `operator&` / `operator|` overloads in `dedekind::sets` for that mixed-carrier case. Ordinary `a & b` / `a | b` syntax finds them via ADL, which demonstrates the rule operationally:

```cpp
// Set<ℕ> & Set<ℤ> tightens to Set<ℕ>
const auto meet = positive_n & bounded_z;
STATIC_CHECK(std::same_as<typename decltype(meet)::Domain, ℕ>);

// Set<ℕ> | Set<ℤ> widens to Set<ℤ>  ({1} ∪ {-1} ⊂ ℤ)
const auto union_set = one_n | neg_one_z;
STATIC_CHECK(std::same_as<typename decltype(union_set)::Domain, SignedCardinality>);
```

The general framework — a `carrier_lattice_meet_t<T1, T2>` trait covering every pair in the lattice, including ℝ / ℂ / 𝔻 — is the scope of #362 proper, tracked as a Paper-3 blocker.

## The elementwise dual: carrier-aware arithmetic

The same carrier-promotion logic applies one layer down — to elementwise arithmetic between values of different carriers. Mathematically, `n + z` with `n ∈ ℕ` and `z ∈ ℤ` is just ℤ-addition (the smaller operand lifts through the embedding); `n - m` with `n, m ∈ ℕ` *is* the Grothendieck-style structure-forcing step that produces an element of ℤ. C++'s integer-promotion rules (`unsigned + signed → unsigned` and friends) are the standard library's implicit version of this same idea, but they collapse in the **wrong direction** (smaller-carrier wins, with the famous wrap-on-negative trap).

Two distinct cases worth distinguishing:

- **Closed cross-carrier operations** — `Cardinality + SignedCardinality → SignedCardinality`. The smaller carrier lifts through the canonical embedding and the larger carrier's operator dispatches. The result type announces the math: the sum lives in the smaller closed carrier that contains both operands.
- **Closure-forcing operations** — `Cardinality - Cardinality → SignedCardinality`. The honest mathematical reading: unary `-` and binary `-` *are* well-defined on ℕ; the naturals simply aren't *closed* under them. The operator isn't missing — its codomain is wider than its source. The cross-carrier (or homogeneous-but-promoting) overload `n - m → ℤ` *is* the Grothendieck embedding made operational: the operator's existence with a wider return type is the structure-forcing axiom expressed in code, not a backstage promotion silently kept hidden. PR #425 deliberately omitted `operator-` from `Cardinality` to keep the rig surface clean; #432 adds it back with the correct (wider) codomain.

This is tracked under issue **#432** as the elementwise sibling of #362. Same staging: existential proof on the variant pair `(Cardinality, SignedCardinality)` first; generic `carrier_arithmetic_promote_t<Op, T1, T2>` trait covering the wider lattice second.

## Recommendations

1. **Keep the type-level distinction.** The embedding view is the right default for a language with a strong type system; provenance is worth its ergonomic cost.
2. **Bridge with cross-type operators where the math is unambiguous.** Comparison (PR #423, #425, #428) is the canonical example.
3. **Extend carrier-promotion rules in binary set operations beyond the current existential proof.** The `(Cardinality, SignedCardinality)` overloads for `&` / `|` now demonstrate the design works in practice: `Set<ℕ> ∩ Set<ℤ>` tightens to `Set<ℕ>` and `Set<ℕ> ∪ Set<ℤ>` widens to `Set<ℤ>` today. The remaining follow-up (#362) is to generalise that pair-specific result into a `carrier_lattice_meet_t<T1, T2>`-style framework across the wider lattice (ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ), so the strict-vs-relaxed dichotomy does not leak anywhere in the DSL surface.

4. **Bring the same carrier-awareness to elementwise arithmetic.** Tracked under #432 as the elementwise sibling of #362. The existential-proof scope mirrors this PR's: cross-carrier `+`, `-`, `*` between `Cardinality` and `SignedCardinality` first; generic `carrier_arithmetic_promote_t<Op, T1, T2>` trait second. The closure-forcing case (`ℕ - ℕ → ℤ`) is the operational statement of the Grothendieck construction.

## Pointers

- `Cardinality` / `SignedCardinality` carrier definitions: [`src/main/modules/dedekind/sets/cardinality.cppm`](../../src/main/modules/dedekind/sets/cardinality.cppm).
- The canonical embeddings: `embed_𝔹_ℕ` in [`numbers/naturals.cppm`](../../src/main/modules/dedekind/numbers/naturals.cppm), `embed_ℕ_ℤ` / `embed_K3_ℤ` in [`numbers/integer.cppm`](../../src/main/modules/dedekind/numbers/integer.cppm), `embed_ℤ_ℚ` / `embed_ℚ_ℝ` / `embed_ℝ_ℂ` in [`numbers/rational.cppm`](../../src/main/modules/dedekind/numbers/rational.cppm).
- Cross-type comparison ops: same `cardinality.cppm` file (the heterogeneous `<=>` / `==` overloads).
- Witnesses: [`src/main/modules/dedekind/numbers/cardinality.cppm`](../../src/main/modules/dedekind/numbers/cardinality.cppm) (`HasPartialOrderOperatorsWith<…>` static_asserts).

## The honesty obligation

The carrier-lattice design is one local instance of a wider principle that runs through the project: the engineer carries the honesty obligation up front, and the compiler discharges everything that follows mechanically. Choosing the right carrier (ℕ for ℕ, not `unsigned int`; ℤ for ℤ, not `int`); spelling the canonical embeddings explicitly; populating the trait registry with claims that are constructively true of the carrier (`is_associative_v<T,Op>` only where the carrier supports it operationally); refraining from `reinterpret_cast` and undefined-behaviour escape hatches in the public surface — these are decisions the type system cannot make and cannot police. They are the load-bearing trust commitment of the library, and they live at a single layer (the trait registry, the embedding arrows, the carrier choices) rather than diffused across runtime checks.

Once those choices are honest, `clang++` does the rest: every `static_assert` discharged, every `requires`-clause checked, every NTTP-folded `constexpr` evaluation, every IR-level constant fold derived from the structural knowledge. The "poor man's" qualifier we attach to this use of the compiler is structural, not economic: `clang++` *is* a formal system, and a typing derivation it accepts is a proof in the propositional fragment. What it cannot do is decide whether the trait registry is honest in the first place — that is the engineer's job, and it is the same job the carrier-lattice design names explicitly.

The Über-Carrier collapse, in this framing, is what happens when the honesty obligation is silently waived: the engineer accepts a single carrier "for ergonomic reasons" and the trait registry's claims gradually drift away from what the carrier actually supports. The carrier-strength-reduction rule is the operational counter-pull — it spends a little ergonomic budget keeping the carriers distinct so the trait registry can stay honest, and the compiler can keep doing the mechanical half of its job.

## A road not taken: truncated subtraction

Closure-forcing is not the only adjoint reading of `ℕ - ℕ`. An order-theoretic alternative — well-known in poset / lattice category theory — is **truncated subtraction**: `y − k = 0` if `y < k`, otherwise the usual difference. This *is* a Galois connection in `(ℕ, ≤)` (the universal property `x + k ≤ y ⟺ x ≤ y −̇ k` holds with `−̇` denoting truncated subtraction), and it keeps closure inside ℕ — at the cost of collapsing distinct deficits (`3 − 5` and `4 − 5` both become `0`).

The two adjoints sit at opposite ends of a tradeoff:

| Choice | Preserved | Lost |
|---|---|---|
| **Closure-forcing** (`ℕ - ℕ → ℤ`) | Information: `3 − 5 = −2` is the true answer | Closure: result type widens |
| **Truncated subtraction** (`ℕ - ℕ → ℕ`) | Closure: result stays in ℕ | Information: `3 − 5 = 4 − 5 = 0` |

Both are honest. This codebase chose closure-forcing because it preserves provenance — the *information* about the relative magnitude survives the operation, and the carrier announces the move into ℤ at the type level. Truncated subtraction is the right choice for a different design (e.g., "monotonic counters where deficits saturate to zero"); we don't ship it, but the design space has both options and naming the road-not-taken is the honest move.

The Galois-connection viewpoint is more general than this section reflects: every step in the carrier lattice ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ corresponds to a pair of adjoints at the order-theoretic level (in addition to the categorical adjunctions sketched above). Tracking the order-theoretic / Galois reading as a parallel framing is filed as a follow-up.

## Note on the Feynman / Grothendieck construction

The "subset vs. embedding" question is the operational shadow of how ℤ is *constructed* from ℕ in the math literature. Feynman's lectures (and any standard algebra text) build ℤ as the Grothendieck group of (ℕ, +) — formally adjoining additive inverses by introducing `operator-`. The phrasing "ℕ has no subtraction" is a slight oversimplification: subtraction *is* well-defined as a function ℕ × ℕ → ℤ; ℕ simply isn't closed under it, and ℤ is what you get when you make the codomain wide enough that the operation closes.

The carrier choice in this codebase mirrors this: `Cardinality` ships the operations under which ℕ is closed (`+`, `*`, `/`, `%` — see PR #425); `SignedCardinality` ships the operations under which ℤ is closed, including `operator-`. Cross-carrier overloads (#432) make the closure-forcing direction explicit at the type level: `Cardinality - Cardinality → SignedCardinality` says, in code, exactly what the Grothendieck construction says in prose. The `embed_ℕ_ℤ` arrow is the relational shadow of the same embedding. C++'s integer-promotion rules (`unsigned × signed → ?`) are the standard-library version going in the *wrong* direction (smaller carrier wins, with the wrap-on-negative trap) — the math-correct direction is what this design codifies.
