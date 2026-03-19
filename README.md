# Dedekind 

[ 🌸 Don't panic. ]

### Symbolic Set Theory in Modern C++20

By implementing this in Modern C++20, we turn mathematical theorems into compile-time proofs. The result is a system where the "Soul" of a number is only evaluated when needed, ensuring the build remains a clean, efficient Directed Acyclic Graph.

Because our numbers are built through structural recursion, this DAG architecture extends from the compiler to the runtime. We don't just hope the math is right; we often ensure that every calculation stays within the limits of what is actually computable, providing a guaranteed path to termination without infinite loops.

"Sets are Rules, not buckets. Logic is the build, not the helper."


**Dedekind** is a modular C++20 library designed to model abstract set theory. Named after Richard Dedekind, the library treats sets not as containers, but as **symbolic expressions** governed by a strictly typed, sealed cardinality algebra.

## Core Intent
Most libraries treat sets as "bags of data." **Dedekind** treats them as **logical rules**. By using **Expression Templates** and a **Sealed Cardinality Hierarchy**, Dedekind allows you to perform operations on infinite sets (the Reals), countable sets (the Integers), and finite sets (the Booleans) using a terse, "mathy" syntax.

### Key Features
*   **Lazy Evaluation:** Operations like Union (`|`) and Intersection (`&`) build a symbolic tree; no logic is executed until a membership test is performed.
*   **Cardinality Algebra:** The type system "proves" the size of results. Intersecting an `Uncountable` set with a `Finite` set automatically promotes the result to `Finite` at compile-time.
*   **Handle Pattern:** `SetHandle<T>` provides a unified, safe interface using `std::shared_ptr` to manage expression lifetimes.
*   **C++20 Modules:** Built from the ground up using C++20 modules for clean boundaries and fast compilation.

---

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




