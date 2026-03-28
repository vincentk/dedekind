[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/vincentk/dedekind/graph/badge.svg)](https://codecov.io/vincentk/dedekind/)

# Dedekind 

[ 🌸 Don't panic. ]

### Computational Structuralism In Modern C++23

The `dedekind` library defines a domain specific language for symbolic math embedded in modern C++23. Starting from established axioms, dedekind provides mathematical theorems as type-checked proofs. Intensional sets are treated as (sometimes undecidable) rules, while extensional sets act as materialized bodies of structured data. By encoding mereology and algebra into the C++ type system, `dedekind` attempts to evaluate and prune symbolic operations at compile-time. The resulting machine code should be as lean as a 'naked' loop, but as rigorous as a formal proof. Dedekind treats the computational DAG as a reified proof of the underlying theorem.

*   Compile-time pruning: Because the library understands **Mereology**, it can prune symbolic trees (e.g., $A \cap \emptyset \to \emptyset$) before a single assembly instruction is generated.
*   Zero-Cost Abstractions: Algebraic checks happen during compilation. Once the compiler is satisfied, it emits the same optimized machine code as a "naked" loop.
*   Structural Optimization: Compile-time inspection of an operation's properties such as associativity or commutativity provides the structural proof for the compiler to reorder (and in principle also parallelise) calculations *safely* without guessing at side effects.

### Status: Embryonic

The following structural anchors are currently established in the registry:

*   **Extensional / Decidable**: **𝔹** (The Booleans) — *Materialized as a finite binary body.*
*   **Intentional / Undecidable**: **ℕ** (The Naturals) — *Represented as a symbolic rule-based species.*

[Note: ℤ (Integers) and ℚ (Rationals) are currently undergoing mereological integration.]

### Background

While formal verification tools (like Lean or Coq) provide mathematical rigor, they lack the hardware-level efficiency required for heavy computation. Conversely, standard numeric libraries often sacrifice mathematical "essence" for raw speed. `dedekind` uses C++23 concepts to try to bridge that gap. The API aims to be approachable. Extensive training using dedicated tools should not be required to make it accessible for everyday use. Its small default footprint should make it viable as an embedded library or in embedded applications. As a matter of fact, the runtime footprint is often non-existent, as structural truths are collapsed into direct machine instructions.

An AI assisted developing this project.

