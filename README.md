[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23697076.svg)](https://doi.org/10.5281/zenodo.23697076)

# Dedekind 

[ 🌸 Don't panic. ]

> **Project status:** pre-release research prototype.
> Public APIs and module boundaries may change until a stable `v1.0.0` release tag.

### Abstract

<!-- The same text as CITATION.cff's abstract, verbatim; change both together. -->
A symbolic-algebra library in standard C++23 that uses compile-time evaluation for static analysis, verification and optimisation. Its foundation is the Juliet Posture (Jlt), a fragment of C++23 under three rules: values are copyable data and functions are pure and constexpr; generic code varies with a type only through that type's concepts and traits; recursion is structural, on a type or on a natural number that counts down. On it the library builds an embedded, lazy language in layers: lattices of truth values from the Boolean and Kleene chains on (Pst); intensional sets, given by a membership test rather than a list (Lwv); a point-free relational calculus over cartesian products (Trsk); and symbolic algebra over the canonical sets (Ddk). Algebraic laws are declared as traits and witnessed by constexpr values: the compiler checks the witnesses, trusts the declarations, and reduces expressions at build time by them, leaving what cannot fold as a specialised run-time residual. A linter in tools/jlt-lint holds the category layer to the three rules in continuous integration, and the layers above to recorded counts.

### What it is for

An embedded language for mathematics in C++: it reads like set-builder notation, and it handles infinite sets given by a predicate as well as finite data.
You state laws about your types.
The compiler checks what it can at build time and uses them to simplify, and what can't be folded runs as ordinary code.
It needs only a standard C++23 toolchain, and the tests fetch Catch2.

### The exhibit: restriction sites, two ways

Listing 5 of the [Jlt paper](https://vincentk.github.io/dedekind/jlt.pdf), in C++ and in Python through `pynzli`.
A restriction site is a word equal to its reverse complement, as EcoRI's `GAATTC`.

#### All sequences, counted

<details open><summary><b>C++ (Jlt)</b></summary>

<!-- listing5:all-genomes:cpp -->

```cpp
// Σ, the alphabet :
constexpr Alphabet bases = abc("ACGT");
// Σ* = ⊔ₙ Σⁿ :
constexpr Words oligos = words(bases);
// ~rev, the reverse complement: CCTA ↦ TAGG
// { w ∈ Σ⁶ | ~rev w = w }, the palindromic hexamers :
constexpr auto pali6 = oligos & len(6) & fixed_by(~rev);
// { (s, i) ∈ Σᴺ × ℕ | s[i, i+6) ∈ pali6 } :
constexpr auto cuts(size_t N) { return (oligos & len(N)) >> pali6; }
static_assert(count(cuts(12)) == 1'835'008uz && count(cuts(32)) == 27uz << 58);
```

</details>
<details><summary><b>Python (<code>pynzli</code>)</b></summary>

<!-- listing5:all-genomes:python -->

```pycon
>>> bases = abc("ACGT")
>>> oligos = words(bases)
>>> pali6 = oligos & len(6) & fixed_by(~rev)
>>> def cuts(N):
...     return (oligos & len(N)) >> pali6
>>> count(cuts(12)), count(cuts(32)) == 27 << 58
(1835008, True)
```

</details>

No sequence is built: at `N = 32` there are more than a 64-bit word can enumerate.

#### One sequence, scanned

<details open><summary><b>C++ (Jlt)</b></summary>

<!-- listing5:one-genome:cpp -->

```cpp
// those of pali6 beginning GA :
constexpr auto sites = pali6 & prefix("GA");
// the table, computed once by the compiler, a function of any s :
constexpr auto scanner = *automaton(sites);
static_assert(states(scanner) == 11uz);
// and only now a sequence :
constexpr auto dna = bytes(
    "GAATTCGAGCTCGGTACCCGGGGATCC"
    "TCTAGAGTCGACCTGCAGGCATGCAAGCTT");
// ten by definition, each window tested :
static_assert(count(dna >> pali6) == 10uz);
// two by the table, in one pass : EcoRI's and SacI's
static_assert(count(dna >> scanner) == 2uz && sites(bytes("GAATTC")) &&
              sites(bytes("GAGCTC")));
```

</details>
<details><summary><b>Python (<code>pynzli</code>)</b></summary>

<!-- listing5:one-genome:python -->

```pycon
>>> sites = pali6 & prefix("GA")
>>> scanner = automaton(sites)
>>> states(scanner)
11
>>> dna = b"GAATTCGAGCTCGGTACCCGGGGATCCTCTAGAGTCGACCTGCAGGCATGCAAGCTT"
>>> count(dna >> pali6)
10
>>> count(dna >> scanner), sites(b"GAATTC"), sites(b"GAGCTC")
(2, True, True)
```

</details>

The table exists before any sequence does, and what remains is one pass over the caller's bytes; here the sequence is the polylinker of the plasmid pUC19.

The C++ compiles in [a test](src/test/cpp/modules/dedekind/category/tensor_exhibit_test.cpp), the Python runs as [a doctest](python/pynzli/jlt.py), and [another test](src/test/python/test_readme_listing5.py) holds this page and the paper to them.

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

Two choices shape the code: C++23, a mainstream language with a compiler that evaluates programs, and category theory, the shared vocabulary between types and textbook mathematics.
C++ templates, concepts and traits give what functional languages call type constructors over type constructors, and types indexed by compile-time values, within what the standard allows.

1. **Concepts follow the textbook.** A mathematical definition becomes a `concept` with few changes, and modules are built in the order the theory depends on them, so a theorem can't be imported before its axioms.
2. **Textbook names.** `ℕ` over `IsNaturalNumberSet`, in `UTF-8`, where the language allows.
3. **Tests map back.** A concept that compiles isn't yet a correct one, so the test suite checks its behaviour against the mathematics.
4. **Standard types where they fit.** `bool`, `std::pair` and `std::variant` serve as the truth values, products and sums they already are.

### What you have to get right

A declared law is trusted: the library checks it on the values it tries, and cannot prove it.
Declare a false one, and the compiler will use it to give you wrong answers.
In return, what follows from true declarations is derived for you and checked at build time.


### Further reading:

* **Theory**: the Jlt paper, [The Juliet Posture: Structurally Typed Functional Programming in Standard C++23](https://vincentk.github.io/dedekind/jlt.pdf) (a draft), and the earlier, broader draft, [Symbolic Algebra in Standard C++ Verified at Compile Time](https://vincentk.github.io/dedekind/paper.pdf).
* **Build**: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* **Documentation**: the [C++ API reference](https://vincentk.github.io/dedekind/) (Doxygen), for quick lookups against the source tree, and the [Python API reference](https://vincentk.github.io/dedekind/python/) (`pynzli`, with usage examples the test suite checks); both are works in progress.
* **Python Bindings (MVP)**: see [docs/python/README.md](docs/python/README.md) and [docs/python/release-checklist-v0.1.md](docs/python/release-checklist-v0.1.md).

_AI assistance is used during the development of this project._
