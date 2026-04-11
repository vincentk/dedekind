[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)

# Dedekind 

[ 🌸 Don't panic. ]

### Computational Structuralism In Modern C++23

The `dedekind` library aims to be a faithful translation of concepts from math taxtbooks to modern C++. 
The intention is to define an embedded domain specific programming language for math. 

The overall architecture rests on two pillars: the C++ language on the one hand, and category theory on the other hand.
 - The choice of C++ is motivated by its strong but quite flexible type system and its status as a mainstream systems programming language.
It allows for the simulation of functional programming language concepts (lambdas, higher-kinded types, dependent types) via template metaprogramming techniques such as concepts and traits.
 - The choice of category theory is motivated by the observation that it bridges the gap from the other end: typed $\lambda$-calculus can be expressed in terms of established math textbook terminology. 

In this approach, 
1. Math concepts can often be lifted almost verbatim from a textbook and checked into the code base as a C++ `concept`.
2. The adjustments required to conform to the constraints of the host language are deliberately minimized and are often quite small.
3. Once a translation of a math concept to C++ compiles, the translation's fidelity can then be tested by checking the reverse translation from C++ invariants to math invariants as part of the test suite.

The hope is that if the translation from math to C++ (and, for validation, the other way around) results in a system which is easily recognizable by a mathematician as well as a C++ programmer.

AI assistance is used during the development of this project.

### Futher reading:

* Build: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* Doxygen: https://vincentk.github.io/dedekind/
