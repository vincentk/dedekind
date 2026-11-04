[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23697076.svg)](https://doi.org/10.5281/zenodo.23697076)

# Dedekind 

[ 🌸 Don't panic. ]

> **Project status:** pre-release research prototype.
> Public APIs and module boundaries may change until a stable `v1.0.0` release tag.

### Abstract

<!-- The same text as CITATION.cff's abstract, verbatim; change both together. -->
A symbolic-algebra library in standard C++23 that uses compile-time evaluation for static analysis, verification and optimisation. Its foundation is the Juliet Posture (Jlt), a fragment of C++23 under three rules: values are copyable data and functions are pure and constexpr; generic code varies with a type only through that type's concepts and traits; recursion is structural, on a type or on a natural number that counts down. On it the library builds an embedded, lazy language in layers: lattices of truth values from the Boolean and Kleene chains on (Pst); intensional sets, given by a membership test rather than a list (Lwv); a point-free relational calculus over cartesian products (Trsk); and symbolic algebra over the canonical sets (Ddk). Algebraic laws are stated as types and as constexpr values, so the compiler checks them and reduces expressions at build time, leaving what cannot fold as a specialised run-time residual. A linter in tools/jlt-lint holds the core of the library to the three rules in continuous integration.

### Computational Structuralism in Modern C++23

The `dedekind` library translates a coherent slice of mathematical concepts into modern high-efficiency `C++`.
It defines an embedded domain-specific language (eDSL) for mathematics with the following goals:

- **Efficiency:** the default runtime footprint stays modest. This keeps embeddings in higher-level languages and constrained environments open. The build itself uses the standard C++ toolchain with CMake and Ninja (and fetches Catch2 for the tests).
- **Recognizability:** Code should be intuitive to both mathematicians and `C++` programmers. The DSL mimics set-builder notation and common algebraic idioms.
- **Versatility:** The DSL expresses both infinite (intensional, symbolic) and finite (extensional, data) structures. It is extensible to allow the specification of further algebraic laws and identities. It allows mixing numerical and symbolic codes.
- **Abstraction First:** Intensional and symbolic development should stay in abstract mathematical carriers; choose a concrete runtime representation only when explicitly realizing or evaluating numerically.
- **Optimization:** The library performs *mathematically motivated* optimizations (e.g., identity laws) at compile time, through theorem search and term reduction. Whatever cannot fold is emitted as a specialized runtime residual.

### The exhibit: restriction sites, two ways

Listing 5 of the Jlt paper, in C++ and in Python through `pynzli`: open either.
A restriction site is a word equal to its reverse complement, as EcoRI's `GAATTC`; an enzyme cuts a genome where one occurs.
The first counts the cuts in *all* genomes of length `N`, never building one: the count is a product of factors, a constant at compile time.
At `N = 32` the count is `27 · 2⁵⁸`, while the `4³² = 2⁶⁴` genomes it stands for are more than a 64-bit word can enumerate.
The second scans *one* sequence: each window tested, or the matcher specialised to the sites.

#### All genomes, counted

<details open><summary><b>C++ (Jlt)</b></summary>

<!-- listing5:all-genomes:cpp -->

```cpp
// Σ, the alphabet :
constexpr Alphabet bases = abc("ACGT");
// Σ* = ⊔ₙ Σⁿ :
constexpr Words oligos = words(bases);
// Σ⁶, read at every offset :
constexpr auto hexamers = windows(6);
// ~rev, the reverse complement: CCTA ↦ TAGG
// { w ∈ Σ⁶ | ~rev w = w }, the palindromic hexamers :
constexpr auto sites = oligos & hexamers & fixed_by(~rev);
// { (s, i) ∈ Σᴺ × ℕ | s[i, i+6) ∈ sites } :
constexpr auto cuts(size_t N) { return (oligos & len(N)) >> hexamers & sites; }
static_assert(count(cuts(12)) == 1'835'008uz && count(cuts(32)) == 27uz << 58);
```

</details>
<details><summary><b>Python (<code>pynzli</code>)</b></summary>

<!-- listing5:all-genomes:python -->

```pycon
>>> bases = abc("ACGT")
>>> oligos = words(bases)
>>> hexamers = windows(6)
>>> sites = oligos & hexamers & fixed_by(~rev)
>>> def cuts(N):
...     return (oligos & len(N)) >> hexamers & sites
>>> count(cuts(12)), count(cuts(32)) == 27 << 58
(1835008, True)
```

</details>

#### One genome, scanned

<details open><summary><b>C++ (Jlt)</b></summary>

<!-- listing5:one-genome:cpp -->

```cpp
// a sequence, N a base not read :
constexpr auto dna = bytes("TTGAATTCNNNNNNNAAGCTTA");
// { i | s[i, i+6) ∈ sites }, each window tested :
constexpr auto cut_sites(std::span<const std::byte> s) {
  return s >> hexamers & sites;
}
static_assert(count(cut_sites(dna)) == 2uz);
// one site, its table built by the compiler :
constexpr auto ecori = automaton(site(bases, "GAATTC"));
static_assert(count(dna >> matches(ecori)) == 1uz);
// all 64, by the same function; the table past the budget, built at run time :
constexpr auto by_the_plan(std::span<const std::byte> s) {
  return count(s >> matches(automaton(sites)));
}
```

</details>
<details><summary><b>Python (<code>pynzli</code>)</b></summary>

<!-- listing5:one-genome:python -->

```pycon
>>> dna = b"TTGAATTCNNNNNNNAAGCTTA"
>>> def cut_sites(s):
...     return s >> hexamers & sites
>>> count(cut_sites(dna))
2
>>> ecori = automaton(site(bases, "GAATTC"))
>>> count(dna >> matches(ecori))
1
>>> def by_the_plan(s):
...     return count(s >> matches(automaton(sites)))
>>> by_the_plan(dna)
2
```

</details>

The sequence holds two sites, EcoRI's `GAATTC` and HindIII's `AAGCTT`, with unread bases between them; a byte outside the alphabet is no site.
The query tests each window against the sites, at compile time here.
One site's matcher is Knuth, Morris and Pratt's table, a compile-time constant; the matcher of all 64 sites, the minimal automaton of their occurrences (277 states), exceeds the evaluator's budget and is built at run time by the same function, as Python builds it.

Both languages are checked sources: the C++ compiles in [`tensor_exhibit_test.cpp`](src/test/cpp/modules/dedekind/category/tensor_exhibit_test.cpp), the Python runs as a doctest of [`pynzli.jlt`](python/pynzli/jlt.py), and [`test_readme_listing5.py`](src/test/python/test_readme_listing5.py) checks that this page shows them verbatim.

### Another exhibit: a set collapsed to a point

```cpp
// Cardinality-1 reduction: an intensional meet of two halfspaces over
// a transfinite carrier collapses to the point {4}, an interval value,
// at compile time, with no lambdas and no predicate erasure.
constexpr auto gt_3 = ℕ & (π > 3);          // { x ∈ ℕ | x > 3 }
constexpr auto lt_5 = ℕ & (π < 5);          // { x ∈ ℕ | x < 5 }

constexpr auto in_between = gt_3 & lt_5;            // ≡ {4}: the point, as a VALUE
static_assert(in_between(4) && !in_between(5));

// Decidability is not the contrast: a halfspace on ℕ decides membership
// by a comparison, so both parents and the result are decidable.  The
// collapse gains a known element.  The intersection IS the theorem:
// { x ∈ ℕ | x > 3 } ∩ { x ∈ ℕ | x < 5 } = {4}.
static_assert(HasDecidableMembership<decltype(gt_3)>);
static_assert(in_between == Interval<Cardinality>::point(finite_cardinality(4)));
```

The example above is showcase 4, checked in as a `STATIC_CHECK` in
[`pruning_showcases_test.cpp`](src/test/cpp/modules/dedekind/analysis/pruning_showcases_test.cpp).
The IR-fixture demonstrations (clang emitting the collapsed form at `-O2`)
live under [`src/test/cpp/modules/dedekind/python/`](src/test/cpp/modules/dedekind/python/).

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

* **Theory**: the Jlt paper, [The Juliet Posture: Structurally Typed Functional Programming in Standard C++23](https://vincentk.github.io/dedekind/jlt.pdf) (a draft), and the earlier, broader draft, [Symbolic Algebra in Standard C++ Verified at Compile Time](https://vincentk.github.io/dedekind/paper.pdf).
* **Build**: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* **Documentation**: the [Doxygen API Reference](https://vincentk.github.io/dedekind/), for quick lookups against the source tree (a work in progress).
* **Python Bindings (MVP)**: see [docs/python/README.md](docs/python/README.md) and [docs/python/release-checklist-v0.1.md](docs/python/release-checklist-v0.1.md).

_AI assistance is used during the development of this project._
