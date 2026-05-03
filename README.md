[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)

# Dedekind 

[ 🌸 Don't panic. ]

> **Project status:** pre-release research prototype.
> Public APIs and module boundaries may change until a stable `v1.0.0` release tag.

### Computational Structuralism in Modern C++23

The `dedekind` library translates a coherent slice of mathematical concepts into modern `C++`. 
It defines an embedded domain-specific language (eDSL) for mathematics with the following goals:

- **Recognizability:** Code should be intuitive to both mathematicians and `C++` programmers.
- **Versatility:** The DSL expresses both infinite (intensional, symbolic) and finite (extensional, data) structures.
- **Abstraction First:** Intensional and symbolic development should stay in abstract mathematical carriers; choose a concrete runtime representation only when explicitly realizing or evaluating numerically.
- **Optimization:** The library performs *mathematically motivated* optimizations (e.g., identity laws) at compile-time.

```cpp
// Cardinality-1 reduction: an intensional set over a transfinite
// carrier collapses to a named extensional Singleton — at compile
// time, with no lambdas, no predicate erasure.
constexpr auto n    = element<Ω<ℕ>>;
constexpr auto gt_3 = Set{n | n > bound<3>};
constexpr auto lt_5 = Set{n | n < bound<5>};

constexpr Singleton<4> in_between = gt_3 & lt_5;   // ≡ {4}
static_assert(in_between == Singleton<4>{});

// The parent Sets carry NONE of the three computability tiers; the
// reduced Singleton carries ALL THREE.  The intersection IS the
// theorem: { n ∈ ℕ | n > 3 } ∩ { n ∈ ℕ | n < 5 } = {4}.
static_assert(!HasDecidableMembership<decltype(gt_3)>);
static_assert(!IsFiniteSet<decltype(gt_3)>);
static_assert(!IsCompileTimeEnumerable<decltype(gt_3)>);

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
3. **Bi-directional Fidelity**: Once a `concept` compiles, its behaviour is checked by mapping `C++` invariants back to their mathematical counterparts within the test suite.
4. **Co-Domain Anchoring**: Where the C++ standard library provides native support for algebraic concepts, this anchoring is made explicit (e.g. `bool`, `std::pair`, `std::variant`, ...) and documented via translation tables.

### Engineering honesty and mechanical sympathy

The library asks the engineer to claim only what is mathematically true of a type, and lets the compiler check the consequences mechanically. The trade is an **honesty obligation up front for a mechanical guarantee afterward**; that asymmetry, not the cost, is what makes the technique worth practising.

**Background reading:**
- *Axiomatic Systems Programming* (with the *Honest Rejection* policy and the *Old mathematics, newly accessible* discussion) in the [paper](https://vincentk.github.io/dedekind/paper.pdf).
- *C++23 ↔ STLC ↔ System F* — the paper's §2 translation table draws an explicit equivalence between the C++23 surface (`template`, `concept`, `requires`, `static_assert`, NTTP-lambda, `constexpr`) and a fragment of System F, which translates via the Lambek–Scott correspondence to STLC enriched with a subobject classifier (a topos). The active research line on extending System F with modern type-system disciplines is well-published; representative anchors include Amin, Grütter, Odersky, Rompf, and Stucki, [*The Essence of Dependent Object Types*](https://doi.org/10.1007/978-3-319-30936-1_14) (LNCS 9600, 2016) — the DOT calculus underlying Scala 3 — and Boruch-Gruszecki, Brachthäuser, Lee, Lelièvre, and Odersky, [*Capturing Types*](https://doi.org/10.1145/3618003) (TOPLAS, 2023). The dedekind library is one practitioner-side realisation of the same identification, restricted to the propositional fragment that the C++ type checker can discharge mechanically.
- Curry–Howard reading of type-system-driven verification: Wadler, [*Propositions as Types*](https://doi.org/10.1145/2699407), CACM 2015.
- Professional grounding: NSPE [*Code of Ethics for Engineers*](https://www.nspe.org/sites/default/files/resources/pdfs/Ethics/CodeofEthics/NSPECodeofEthicsforEngineers.pdf) — canons III (truthful public statements) and IV (faithful agency).
- Categorical foundations: Lawvere, *An elementary theory of the category of sets* (PNAS, 1964); Mac Lane, *Categories for the Working Mathematician* (1971); for the programmer-accessible bridge, Bartosz Milewski, [*Category Theory for Programmers*](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/) (blog series and book); for an even friendlier working-engineer on-ramp, the [Abuse of Notation](https://abuseofnotation.github.io/) blog.
- *Mechanical sympathy* as a software-engineering discipline — the term Jackie Stewart used about race-car drivers and Martin Thompson popularised for low-latency systems work; see the [Mechanical Sympathy blog](https://mechanical-sympathy.blogspot.com/) and the LMAX Disruptor literature.

_AI assistance is used during the development of this project._

### Further reading:

* **Build**: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* **Documentation** (two views of the same project; both works in progress):
  * [Doxygen API Reference](https://vincentk.github.io/dedekind/) — for quick lookups against the source tree.
  * [Draft Paper](https://vincentk.github.io/dedekind/paper.pdf) — a high-level overview with the theoretical motivation.
* **Python Bindings (MVP)**: see [docs/python/README.md](docs/python/README.md) and [docs/python/release-checklist-v0.1.md](docs/python/release-checklist-v0.1.md).
