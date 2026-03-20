# Dedekind 

[ 🌸 Don't panic. ]

### Categorical Set Theory in Modern C++20

`dedekind` is a structuralist C++20 library that transforms mathematical theorems into compile-time proofs. by treating sets as **rules, not buckets**, we aim to ensure that a numerical system is type-checked as soon as possible but evaluated only when logically required, maintaining an efficient directed acyclic graph from the compiler to the runtime.

`dedekind` expressly aims to bridge the gap between **High-Performance Computing (HPC)** and **Category Theory**. 

While formal verification tools (like Lean or Coq) provide the mathematical rigor, they lack the hardware-level efficiency required for heavy computation. Conversely, standard numeric libraries often sacrifice mathematical "essence" for raw speed. `dedekind` uses C++20 concepts to enforce categorical invariants at compile-time, ensuring that a directed acyclic graph of operations not only possesses the required mathematical structure but also zero-overhead at runtime.

*   Compile-time pruning: Because the library understands **Mereology**, it can prune symbolic trees (e.g., $A \cap \emptyset \to \emptyset$) before a single assembly instruction is generated.
*   Zero-Cost Abstractions: Your algebraic checks happen during compilation. Once the compiler is satisfied, it emits the same optimized machine code as a "naked" loop.
*   Structural Optimization: Compile-time inspection of an operation's properties such as associativity or commutativity allows the compiler to reorder and parallelise calculations safely without "guessing" at side effects.

In practice, `dedekind` does not just use `consteval` because it's somehow cool. The compiler may be able to reassociate a $(A + B) + C$ into a $A + (B + C)$ without worrying about floating-point drift, because we've proven the associativity.


## Core Intent: Structuralism over Mereology
Many libraries treat sets as "bags of data." `dedekind` treats them as algebraic objects within a strictly defined category. By using expression templates and a hierarchical morphism engine, `dedekind` allows you to animate the transitions extensional (`size_t`) $\rightarrow$ finite $\rightarrow$ countable ($\aleph_0$) $\rightarrow$ continuous ($2^{\aleph_0}$) using the same laws that govern the finite.

## Key Features
*   `ontology:category`: built on the categorical logic of semigroupoids and small categories. we define the laws of composition and identity before a single element is ever "contained."
*   `ontology:cardinalities`: a unified, inductive ladder of infinity. by positing the generalized continuum hypothesis. The $\aleph$ and $\beth$ hierarchies are collapsed as follows:
    $$\forall n: \aleph_n = \beth_n \implies 2^{\aleph_n} = \aleph_{n+1}$$
    the library promotes the cantor jump through a recursive `power_type` morphism.
*   `ontology:mereology`: operations like union ($|$) and intersection ($\&$) build a symbolic tree which can be optimized at compile time. The cardinality algebra promotes results at compile-time (e.g., $\text{uncountable} \cap \text{finite} \to \text{finite}$).
*   `ontology:algebra`: a raw coordinate species is lifted to a higher algebraic rank once it satisfies the required axioms: `Monoid_ℕ \subset Group_ℤ \subset Field_ℚ \subset Continuum_ℝ`.

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




