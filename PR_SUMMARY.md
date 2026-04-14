# feat(ieee): upstream IEEE layer grounded in category module

## Summary

This PR introduces the IEEE number system as a first-class upstream layer
in the module hierarchy, parallel to how `dedekind.category` provides
mathematical plumbing. The IEEE layer provides *physical* plumbing — the
explicit reification of hardware floating-point semantics — certified fully
against category concepts before any mathematical structure (sets, order,
algebra) is built on top.

**New module layer order:**
```
category → ieee → sets → order → topology → sequences → algebra → morphologies → geometry → numbers → analysis
```

The existing `dedekind.numbers.ieee` and `dedekind.numbers.approx` modules
are retained as thin compatibility bridges that add `machine_real_scalar`
defaults and the `assume_ieee` / `discharge_ieee` adapters dependent on
`Real<F>`.

## Key features

### `dedekind.ieee` (new upstream layer, category-only dependency)
- `IEEE<F>` wrapper: values explicitly certified under IEEE semantics
- Associativity and commutativity declared by policy (same opt-in boundary as the `import` statement itself)
- Monad combinators: `ieee_unit`, `ieee_map`, `ieee_bind`
- Operation tokens `IEEEAdd<F>` and `IEEEMul<F>` with **exact sensitivities**:
  - `IEEEAdd::sensitivity() → (1, 1)` — compile-time constants
  - `IEEEMul::sensitivity(a,b) → (b, a)` — operand reads, no new arithmetic
- `SpeciesTraits<IEEE<F>>` and all algebraic trait specialisations registered directly in `dedekind::category`

### `dedekind.ieee.approx` (new upstream submodule, category + ieee dependency)
- `NumericRegion` classification: Regular, NearZero (cancellation zone), Huge (overflow proximity), NonFinite
- `Approx<F>` struct: IEEE value + absolute/relative error bounds + region tag
- Three policies for error response:
  - **IgnoreErrorPolicy**: fast lane, no diagnostics
  - **ReportErrorPolicy**: wraps result in error envelope
  - **AdaptiveErrorPolicy**: returns `TernaryResult<Approx<F>>` — False/Unknown/True by region
- **`propagate<F,Op>(a, b)`** gated on `IsIEEEPropagationOp<Op,F>`, which is itself grounded in `IsAssociative<IEEE<F>, Op>` from `dedekind.category`:

$$\sigma_{\text{out}} \leq |\partial_a f| \cdot \sigma_a + |\partial_b f| \cdot \sigma_b + \varepsilon|f(a,b)|$$

### `dedekind.numbers.ieee` (bridge, unchanged API)
- Re-exports `IEEE<F>`, `IEEEAdd<F>`, `IEEEMul<F>`, `ieee_unit`, `ieee_map`, `ieee_bind` from `dedekind.ieee` with `machine_real_scalar` defaults
- Retains `assume_ieee(Real<F>)` / `discharge_ieee(IEEE<F>)` entry/exit adapters that remain downstream (depend on `Real<F>`)

### `dedekind.numbers.approx` (bridge, unchanged API)
- Re-exports all approximation types, concepts, and functions from `dedekind.ieee.approx` with `machine_real_scalar` defaults
- No logic duplication — all error propagation reasoning lives in the upstream layer

## Architectural resolution of bootstrapping
The sensitivities are declared by fiat (exactly like the algebraic laws in `IEEE<F>`), not derived. This avoids circular dependency:
- For addition: sensitivities are pure constants
- For multiplication: sensitivities read already-computed operand values

The `propagate()` function is therefore the ground level of an error accumulation chain without requiring arithmetic to estimate arithmetic errors. The `IsIEEEPropagationOp` concept enforces an `IsAssociative` gate from `dedekind.category`, making the category module the primary contract surface for the physical plumbing layer.

## Literature
All "clever" bits carry doxygen **@ref** citations:
- **Gauss (1823)** — foundational work on error propagation
- **Ku (1966)** — "Notes on the use of propagation of error formulas", NIST J. Research
- **Goodman (1960)** — "On the Exact Variance of Products", J. Amer. Statist. Assoc.
- **Higham (2002)** — "Accuracy and Stability of Numerical Algorithms" (2nd ed.), SIAM
- **Ogita–Rump–Oishi (2005)** — "Accurate Sum and Dot Product", SIAM J. Sci. Comput.
- **Rump (2010)** — "Verification methods: Rigorous results using floating-point arithmetic", Acta Numerica
- **Kleene (1952)** — "Introduction to Metamathematics" (three-valued logic)
- **Fitting (1985)** — "A Kripke-Kleene Semantics for Logic Programs", J. Logic Programming
- **IEEE 754-2019** — IEEE Standard for Floating-Point Arithmetic

## Files changed

### New files
- `src/main/modules/dedekind/ieee/ieee.cppm` — core IEEE module (category-only import)
- `src/main/modules/dedekind/ieee/approx.cppm` — upstream approximation policies
- `src/test/cpp/modules/dedekind/ieee/ieee_test.cpp` — direct tests for `IEEEAdd`/`IEEEMul` sensitivity
- `src/test/cpp/modules/dedekind/ieee/approx_test.cpp` — direct tests for `propagate` via `demo_propagate_add`

### Modified files
- `CMakeLists.txt` — inserted `ieee` layer; `sets` depends on `dedekind_ieee`; all aggregate targets updated; test creation guarded with `if(TEST_SOURCES)`
- `src/main/modules/dedekind/numbers/ieee.cppm` — converted to bridge (imports `dedekind.ieee`)
- `src/main/modules/dedekind/numbers/approx.cppm` — converted to bridge (imports `dedekind.ieee.approx`)
- `docs/paper/paper.tex` — updated module diagram and IEEE/approx sections to reflect upstream layer placement

## Test results
- All 11 test suites pass ✓ (category, ieee, sets, order, topology, sequences, algebra, morphologies, geometry, numbers, analysis)
- New upstream tests validate:
  - `IEEEAdd` / `IEEEMul` sensitivity values at representative inputs
  - First-order Gaussian error accumulation via `demo_propagate_add`
  - All prior `test_numbers` corner cases and reassociation improvements passing through bridges

## Future work (separate PRs)
- **Lipschitz-aware hooks**: per-operation bounds on error scaling as a trait
- **Stochastic rounding plugin**: `dedekind.numbers.stochastic` for confidence intervals via CESTAC / Verificarlo methods
- **Composed pipeline demo**: policy switching mid-computation, integrated with affine arithmetic or ball arithmetic

## Checklist
- [x] Code compiles without errors or warnings
- [x] All tests pass (11/11)
- [x] Clang-format applied
- [x] Doxygen references added with full citations
- [x] Module boundaries used for opt-in semantics (import = certification)
- [x] No circular dependencies in error estimation
- [x] Upstream layer certified against `dedekind.category` concepts before downstream math layers

**Ready for review and merge.**
