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
// Cardinality-1 reduction: an intensional set over a transfinite
// carrier collapses to a named extensional Singleton — at compile
// time, with no lambdas, no predicate erasure.
constexpr auto n        = var<ℕ>;
constexpr auto gt_three = Set{n % N | (n > bound<3>)};
constexpr auto lt_five  = Set{n % N | (n < bound<5>)};

constexpr Singleton<4> in_between = gt_three & lt_five;   // ≡ {4}
static_assert(in_between == Singleton<4>{});

// The parent Sets carry NONE of the three computability tiers; the
// reduced Singleton carries ALL THREE.  The intersection IS the
// theorem: { n ∈ ℕ | n > 3 } ∩ { n ∈ ℕ | n < 5 } = {4}.
static_assert(!HasDecidableMembership<decltype(gt_three)>);
static_assert(!IsFiniteSet<decltype(gt_three)>);
static_assert(!IsCompileTimeEnumerable<decltype(gt_three)>);

static_assert(HasDecidableMembership<decltype(in_between)>);
static_assert(IsFiniteSet<decltype(in_between)>);
static_assert(IsCompileTimeEnumerable<decltype(in_between)>);
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
3. **Bi-directional Fidelity**: Once a `concept` compiles, its fidelity is verified by checking that `C++` invariants map correctly back to their mathematical counterparts within the test suite.
4. **Co-Domain Anchoring**: Where the C++ standard library provides native support for algebraic concepts, this anchoring is made explicit (e.g. `bool`, `std::pair`, `std::variant`, ...) and documented via translation tables.

### Engineering honesty and mechanical sympathy

Two related disciplines anchor the technique:

- **Engineering honesty.** The trait registry asserts only what is constructively true of the carrier — `is_associative_v<T, Op>` is registered when `Op` actually associates on `T`, never as a convenience. Concept names match the math: `unsigned int` is rejected as a witness for ℕ because it claims *more* structure than ℕ has (additive inverses via mod wrap), not less. Read in the spirit of the engineering profession's truthfulness canons, this is a **public statement** at the type-system level — visible, peer-reviewable, and machine-checkable against fixed inference rules.
- **Mechanical sympathy.** Once the math is honest, the rest is mechanical: `static_assert`s discharge the derivation, concept-driven dispatch routes calls without runtime overhead, NTTP-folded `constexpr` evaluation carries witnesses through to the IR. The compiler does not care which agent typed the code; it cares that the typing rules check.

The trade is therefore an honesty obligation up front for a mechanical guarantee afterward — that asymmetry, not the cost, is what makes the technique worth practising. See the paper's *§Axiomatic Systems Programming* for the full argument.

_AI assistance is used during the development of this project._

### Further reading:

* **Build**: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* **Documentation** (three views of the same project; all works in progress):
  * [Doxygen API Reference](https://vincentk.github.io/dedekind/) — for quick lookups against the source tree.
  * [Draft Paper](https://vincentk.github.io/dedekind/paper.pdf) — a high-level overview with the theoretical motivation.
  * [Draft Report](https://vincentk.github.io/dedekind/report.pdf) — a reference manual for the project's current state.
* **Python Bindings (MVP)**: see [docs/python/README.md](docs/python/README.md) and [docs/python/release-checklist-v0.1.md](docs/python/release-checklist-v0.1.md).
