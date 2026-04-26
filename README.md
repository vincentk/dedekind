[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)

# Dedekind 

[ 🌸 Don't panic. ]

> **Project status:** pre-release research prototype.
> Public APIs and module boundaries may change until a stable `v1.0.0` release tag.

### Computational Structuralism in Modern C++23

The `dedekind` library intends to be a faithful translation of mathematical concepts into modern `C++`. 
It defines an embedded domain-specific language (eDSL) for mathematics with the following goals:

- **Recognizability:** Code should be intuitive to both mathematicians and `C++` programmers.
- **Versatility:** The DSL expresses both infinite (intensional, symbolic) and finite (extensional, data) structures.
- **Abstraction First:** Intensional and symbolic development should stay in abstract mathematical carriers; choose a concrete runtime representation only when explicitly realizing or evaluating numerically.
- **Optimization:** The library performs *mathematically motivated* optimizations (e.g., identity laws) at compile-time.

```cpp
// If it satisfies the requirements of a Dedekind-complete ordered field,
// it is treated as a representation of ℝ, regardless of its name.
static_assert( SmellsLike<ℝ, DedekindCompleteField> );
```

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
3. **Bi-directional Fidelity**: Once a concept compiles, its fidelity is verified by checking that `C++` invariants map correctly back to their mathematical counterparts within the test suite.
4. **Co-Domain Anchoring**: Where the C++ standard library provides native support for algebraic concepts, this anchoring is made explicit (e.g. `bool`, `std::pair`, `std::variant`, ...) and documented via translation tables.

_AI assistance is used during the development of this project._

### Further reading:

* **Build**: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* **Documentation** (three views of the same project; all works in progress):
  * [Doxygen API Reference](https://vincentk.github.io/dedekind/) — for quick lookups against the source tree.
  * [Draft Paper](https://vincentk.github.io/dedekind/paper.pdf) — a high-level overview with the theoretical motivation.
  * [Draft Report](https://vincentk.github.io/dedekind/report.pdf) — a reference manual for the project's current state.
* **Python Bindings (MVP)**: see [docs/python/README.md](docs/python/README.md) and [docs/python/release-checklist-v0.1.md](docs/python/release-checklist-v0.1.md).
