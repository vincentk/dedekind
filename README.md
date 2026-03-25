# Dedekind 

[ 🌸 Don't panic. ]

### Categorical Set Theory in Modern C++23

`dedekind` is a structuralist C++23 library that transforms mathematical theorems into compile-time proofs. Sets are treated as **rules, not buckets**. These rules ensure that a numerical system is type-checked as soon as possible but evaluated only when logically required. This way, an efficient directed acyclic graph can be maintained from compile-time to the runtime.

`dedekind` expressly aims to bridge the gap between **High-Performance Computing (HPC)** and **Category Theory**. 

While formal verification tools (like Lean or Coq) provide the mathematical rigor, they lack the hardware-level efficiency required for heavy computation. Conversely, standard numeric libraries often sacrifice mathematical "essence" for raw speed. `dedekind` uses C++23 concepts to enforce categorical invariants at compile-time, ensuring that a directed acyclic graph of operations not only possesses the required mathematical structure but also zero-overhead at runtime. The API aims to be approachable. Extensive training using dedicated tools should not be required to make it accessible for everyday use. Its small default runtime footprint should make it viable as an embedded library or in embedded applications.

*   Compile-time pruning: Because the library understands **Mereology**, it can prune symbolic trees (e.g., $A \cap \emptyset \to \emptyset$) before a single assembly instruction is generated.
*   Zero-Cost Abstractions: Algebraic checks happen during compilation. Once the compiler is satisfied, it emits the same optimized machine code as a "naked" loop.
*   Structural Optimization: Compile-time inspection of an operation's properties such as associativity or commutativity allows the compiler to reorder (and in principle also parallelise) calculations *safely* without guessing at side effects.
