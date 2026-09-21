[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)

# Dedekind 

[ 🌸 Don't panic. ]

> **Project status:** pre-release research prototype.
> Public APIs and module boundaries may change until a stable `v1.0.0` release tag.

### Computational Structuralism in Modern C++23

The `dedekind` library translates a coherent slice of mathematical concepts into modern high-efficiency `C++`.
It defines an embedded domain-specific language (eDSL) for mathematics with the following goals:

- **Efficiency:** default runtime requirements stay modest. This keeps embeddings in higher-level languages and constrained environments open. The software dependencies are the C++ and Python toolchains.
- **Recognizability:** Code should be intuitive to both mathematicians and `C++` programmers. The DSL mimics set-builder notation and common algebraic idioms.
- **Versatility:** The DSL expresses both infinite (intensional, symbolic) and finite (extensional, data) structures. It is extensible to allow the specification of further algebraic laws and identities. It allows mixing numerical and symbolic codes.
- **Abstraction First:** Intensional and symbolic development should stay in abstract mathematical carriers; choose a concrete runtime representation only when explicitly realizing or evaluating numerically.
- **Optimization:** The library performs *mathematically motivated* optimizations (e.g., identity laws) at compile time, through theorem search and term reduction. Whatever cannot fold is emitted as a specialized runtime residual.

```cpp
// Cardinality-1 reduction: an intensional meet of two halfspaces over
// a transfinite carrier collapses to a named extensional Singleton, at
// compile time, with no lambdas and no predicate erasure.
constexpr auto gt_3 = ℕ | (χ > fix(3_c));          // { x ∈ ℕ | x > 3 }
constexpr auto lt_5 = ℕ | (χ < fix(5_c));          // { x ∈ ℕ | x < 5 }

constexpr Singleton<4> in_between = gt_3 & lt_5;   // ≡ {4}, at compile time
static_assert(in_between == Singleton<4>{});

// Decidability is not the contrast: a halfspace on ℕ decides membership
// by a comparison, so both parents and the result are decidable.  The
// collapse gains finiteness and extensionality.  The intersection IS the
// theorem: { x ∈ ℕ | x > 3 } ∩ { x ∈ ℕ | x < 5 } = {4}.
static_assert(HasDecidableMembership<decltype(gt_3)>);
static_assert(!IsFiniteSet<decltype(gt_3)>);
static_assert(!IsExtensional<decltype(gt_3)>);

static_assert(HasDecidableMembership<decltype(in_between)>);
static_assert(IsFiniteSet<decltype(in_between)>);
static_assert(IsExtensional<decltype(in_between)>);
```

The full set of IR-verified showcases lives under
[`src/test/cpp/modules/dedekind/python/`](src/test/cpp/modules/dedekind/python/).
The example above is showcase 4.

### Quickstart
```bash
# Clone the repository
git clone https://github.com/vincentk/dedekind && cd dedekind

# Run a clean build and verification (test) suite
make clean compile test
```

For full build targets and contributor workflow helpers, see [CONTRIBUTING.md](CONTRIBUTING.md).

### Architecture

The project rests on two pillars: **C++23** and **Category Theory**.

 - **Why C++?** It is a mainstream language with many features which is suitable for performance-optimized code. Its flexible type system allows for the simulation of functional concepts—such as higher-kinded and dependent types—via template metaprogramming, concepts, and traits.
 - **Why Category Theory?** It bridges the gap from the mathematical end: typed λ-calculus can be expressed directly in the terminology of established mathematical textbooks.

In this approach:
1. **Verbatim Lifting**: Mathematical concepts are translated into `C++` `concept`s with minimal adjustments. The flow from axioms to theorems is enforced by the `module` build order, the `import` graph and `C++` declaration order rules.
2. **Language Conformity**: Modifications required to satisfy the host language (`C++`) are kept as non-intrusive as possible. Textbook naming conventions using `UTF-8` are preferred, i.e. `ℝ` instead of `IsRealNumberSet`.
3. **Bi-directional Fidelity**: Once a `concept` compiles, its behaviour is checked by mapping `C++` invariants back to their mathematical counterparts within the test suite.
4. **Co-Domain Anchoring**: Where the C++ standard library provides native support for algebraic concepts, this anchoring is made explicit (e.g. `bool`, `std::pair`, `std::variant`, ...) and documented via translation tables.

### Engineering honesty and mechanical sympathy

The library asks the engineer to claim only what is mathematically true of a type, and lets the compiler check the consequences mechanically. The trade is an **honesty obligation up front for a mechanical guarantee afterward**; that asymmetry, not the cost, is what makes the technique worth practising.


### Further reading:

* **Theory**: The accompanying draft paper: [Symbolic Algebra in Standard C++ Verified at Compile Time](https://vincentk.github.io/dedekind/paper.pdf).
* **Build**: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* **Documentation**: the [Doxygen API Reference](https://vincentk.github.io/dedekind/), for quick lookups against the source tree (a work in progress).
* **Python Bindings (MVP)**: see [docs/python/README.md](docs/python/README.md) and [docs/python/release-checklist-v0.1.md](docs/python/release-checklist-v0.1.md).

_AI assistance is used during the development of this project._
