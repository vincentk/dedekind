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
make compile
make test
```

> **Note:** The `Makefile` wraps CMake/Ninja and selects the correct compiler and flags automatically.
> It is the **preferred** interface for local development and is also used in CI.
> Run `make` (or `make all`) to see available targets:
>
> | Target | Description |
> |---|---|
> | `make compile` | Configure (if needed) and build all targets |
> | `make test` | Build then run the full test suite via CTest |
> | `make format` | Auto-format all `*.cpp` / `*.cppm` sources with `clang-format` |
> | `make format-check` | Verify formatting without modifying files |
> | `make coverage` | Build, run tests, and produce an LLVM coverage report |
> | `make doxygen` | Build Doxygen API docs into `build/docs/` |
> | `make report` | Fast report compile check (`docs/report` `ci-check`) |
> | `make clean` | Remove the `build/` directory |
> | `make install-hooks` | Install the pre-push git hook |
>
> For contributor workflow helpers (`make ci-main`, `make pr-status`,
> `make pr-checks`, `make pr-watch`, `make pr-sync`), see `CONTRIBUTING.md`.

### Architecture

The project rests on two pillars: **C++23** and **Category Theory**.

 - **Why C++?** It is a mainstream language with many features which is suitable for performance-optimized code. Its flexible type system allows for the simulation of functional concepts—such as higher-kinded and dependent types—via template metaprogramming, concepts, and traits.
 - **Why Category Theory?** It bridges the gap from the mathematical end: typed λ-calculus can be expressed directly in the terminology of established mathematical textbooks.

In this approach;
1. **Verbatim Lifting**: Mathematical concepts are translated into `C++` `concept`s with minimal adjustments. The flow from axioms to theorems is enforced by the `module` build order, the `import` graph and `C++` declaration order rules.
2. **Language Conformity**: Modifications required to satisfy the host language (`C++`) are kept as non-intrusive as possible. Textbook naming conventions using `UTF-8` are preferred, i.e. ℝ instead of `IsRealNumberSet`.
3. **Bi-directional Fidelity**: Once a concept compiles, its fidelity is verified by checking that `C++` invariants map correctly back to their mathematical counterparts within the test suite.

_AI assistance is used during the development of this project._

### Interoperability Notes (MVP)

`dedekind::interop` provides explicit type-boundary adapters between
Dedekind extensional sets and standard set containers:

- `from_std(std::set<T>)` / `from_std(std::unordered_set<T>)`
- `to_std<std::set<T>>(ext)` / `to_std<std::unordered_set<T>>(ext)`

Example:

```cpp
import dedekind.sets;

std::set<int> ordered{1, 2, 3};
auto ext = dedekind::interop::from_std(ordered);
auto back = dedekind::interop::to_std<std::set<int>>(ext);
```

Semantic caveats:

- Conversions are explicit on purpose; there are no implicit container bridges.
- Interop materializes a finite runtime carrier (`FiniteExtensionalSet<T>`), so
	the conversion boundary is auditable and does not silently reinterpret
	intensional expressions.
- Membership meaning is preserved when container equivalence agrees with
	ordinary value equality, including the supported MVP paths through
	`std::set<T>` with the default comparator and `std::unordered_set<T, Hash, Equal>`.
	Custom `std::set<T, Compare>` comparators are intentionally rejected in this
	phase because they may encode a different notion of uniqueness.
- Extensional equality is checked via round-trip tests within those limits.

Performance notes:

- `from_std` and `to_std` are linear in the number of elements (`O(n)`).
- `std::unordered_set` targets generally provide expected `O(1)` lookup,
	while `std::set` targets provide `O(log n)` lookup.

### Further reading:

* **Build**: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* **Documentation**: [Doxygen API Reference](https://vincentk.github.io/dedekind/)
* **Theory**: See the [Draft Report](https://vincentk.github.io/dedekind/report.pdf) for the theoretical foundations. _Note: Just like much of the code base, this is a work-in-progress document._
