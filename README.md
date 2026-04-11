[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)

# Dedekind 

[ 🌸 Don't panic. ]

### Computational Structuralism in Modern C++23

The `dedekind` library is a faithful translation of mathematical concepts into modern C++. It defines an embedded domain-specific language (eDSL) for mathematics with the following goals:

- **Recognizability:** Code should be intuitive to both mathematicians and C++ programmers.
- **Versatility:** The DSL expresses both infinite (intensional, symbolic) and finite (extensional, data) structures.
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

# Build and run the formal verification (test) suite
cmake -B build -G Ninja
cmake --build build
ctest --test-dir build --output-on-failure
```

### Architecture

The project rests on two pillars: *C++23* and *Category Theory*.

 - *Why C++?* Its flexible type system and status as a mainstream systems language allow for the simulation of functional concepts—such as higher-kinded and dependent types—via template metaprogramming, concepts, and traits.
 - *Why Category Theory?* It bridges the gap from the mathematical end: typed -calculus can be expressed directly in the terminology of established mathematical textbooks.

In this approach;
1. *Verbatim Lifting*: Mathematical concepts are translated into C++ `concept`s with minimal adjustments.
2. *Language Conformity*: Modifications required to satisfy the host language (C++) are kept as non-intrusive as possible.
3. *Bi-directional Fidelity*: Once a concept compiles, its fidelity is verified by checking that C++ invariants map correctly back to their mathematical counterparts within the test suite.

_AI assistance is used during the development of this project._

### Further reading:

* *Build*: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* *Documentation*: [Doxygen API Reference](https://vincentk.github.io/dedekind/)
* *Theory*: See the [Draft Paper](docs/paper/paper.pdf) for the theoretical foundations. _Note: Just like much of the code base, this is a work-in-progress document._
