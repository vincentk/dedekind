# Dedekind 

[ 🌸 Don't panic. ]

### Categorical Set Theory in Modern C++23

`dedekind` is a structuralist C++20 library that transforms mathematical theorems into compile-time proofs. Sets are treated as **rules, not buckets**. These rules ensure that a numerical system is type-checked as soon as possible but evaluated only when logically required. This way, an efficient directed acyclic graph can be maintained from compile-time to the runtime.

`dedekind` expressly aims to bridge the gap between **High-Performance Computing (HPC)** and **Category Theory**. 

While formal verification tools (like Lean or Coq) provide the mathematical rigor, they lack the hardware-level efficiency required for heavy computation. Conversely, standard numeric libraries often sacrifice mathematical "essence" for raw speed. `dedekind` uses C++23 concepts to enforce categorical invariants at compile-time, ensuring that a directed acyclic graph of operations not only possesses the required mathematical structure but also zero-overhead at runtime. The API aims to be approachable. Extensive training using dedicated tools should not be required to make it accessible for everyday use. Its small default runtime footprint should make it viable as an embedded library or in embedded applications.

*   Compile-time pruning: Because the library understands **Mereology**, it can prune symbolic trees (e.g., $A \cap \emptyset \to \emptyset$) before a single assembly instruction is generated.
*   Zero-Cost Abstractions: Algebraic checks happen during compilation. Once the compiler is satisfied, it emits the same optimized machine code as a "naked" loop.
*   Structural Optimization: Compile-time inspection of an operation's properties such as associativity or commutativity allows the compiler to reorder and parallelise calculations safely without "guessing" at side effects.

`dedekind` does not use `consteval` just because it's somehow cool. The compiler may be able to reassociate a $(A + B) + C$ into a $A + (B + C)$ without worrying about floating-point drift, because we've proven the associativity. Similarly, the compiler can identify and optimize identities such as $(A \union (B \intersection \empty) = A)$. By using expression templates and a hierarchical morphism engine, `dedekind` allows you to animate the transitions extensional (`size_t`) $\rightarrow$ finite $\rightarrow$ countable ($\aleph_0$) $\rightarrow$ continuous ($2^{\aleph_0}$) using the same laws that govern the finite.

## Key Features
*   `ontology:logic`: The "Subobject Classifier" ($\Omega$). Defines the internal logic (Boolean, Ternary/Kleene, or Fuzzy) that governs membership rules. Supports point-free composition: `is_valid = is_even && is_pos`.
*   `ontology:category`: The skeletal foundation. Built on the categorical logic of semigroupoids and small categories. Defines the natural transformations ($\eta$) that bridge the "Machine Category" (C++ primitives) to the "Mathematical Category."
*   `ontology:mereology`: The study of parts and wholes. Operations like union ($|$) and intersection ($\&$) build a symbolic Directed Acyclic Graph (DAG). Because it understands identity ($A \cup \emptyset \to A$), the compiler prunes the tree before code generation.
*   `ontology:cardinalities`: An inductive ladder of infinity. By positing the Generalized Continuum Hypothesis ($\aleph_n = \beth_n$), the library promotes the Cantor Jump through a recursive `power_type` morphism: $2^{\aleph_n} = \aleph_{n+1}$.
*   `ontology:algebra`: The "Soul" of the system. A coordinate species is lifted to a higher algebraic rank (Monoid $\to$ Group $\to$ Field) once it satisfies the required axioms. 

> "Sets are rules, not buckets. Algebra is the category, not the helper. Logic is the build.

## Code Highlights

### 1. The "Such That" Filter ($\{ x \in \mathbb{Z} \mid x > 0 \land x \text{ is prime} \}$)
Using the `^` operator as the "such that" glyph, you can carve specific subsets out of infinite universes.
```cpp
// Dedekind one-liner:
auto PositivePrimes = Z ^ (is_positive && is_prime);

// Membership test via operator[]
bool result = PositivePrimes[7]; // Returns true 
```

### 2. Symbolic Difference and Identity ($\emptyset \setminus \mathbb{R} = \emptyset$)
The library understands monoidal identities. Subtracting the infinite Reals from an Empty set results in a set that is statically proven to be `Empty`.

```cpp
auto E = universes::EmptySet<double>();
auto Reals = universes::RealUniverse<double>();

auto result = E - Reals; // Internally: E & (!Reals)

// This check is a compile-time metadata lookup:
assert(result.cardinality() <= Cardinality::Empty{}); 
```

### 3. Cross-Universe Intersection

```markdown
### 3. Cross-Universe Intersection with Bounds Propagation
When you intersect a "Black Box" predicate with a "Small" universe, the result inherits the metadata of the smaller set, enabling optimizations.

```cpp
auto SmallSet = universes::ExtensionalSet<int>({1, 2, 3, 4, 5});
auto Evens = universes::Z ^ is_even;

// Intersecting Countable & Extensional results in an Extensional set
auto result = Evens & SmallSet; 

// result.size() is now available because the type system proved it's Extensional!
// Returns 5 (the mathematical upper bound for the intersection)
std::cout << "Max possible size: " << result.size() << std::endl; 
```

### Build Requirements and Footer

```markdown
---

## Build Requirements
*   **Compiler:** LLVM/Clang 16+ or GCC 13+ (Full C++20 Module support required).
*   **Build System:** CMake 3.28+.
*   **Testing:** Catch2 v3.

### Local Build (macOS/Ubuntu)
```bash
mkdir build && cd build
cmake .. -DCMAKE_CXX_COMPILER=clang++
cmake --build .
ctest --output-on-failure




