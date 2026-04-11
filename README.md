[![C++ CI](https://github.com/vincentk/dedekind/actions/workflows/cmake.yml/badge.svg)](https://github.com/vincentk/dedekind/actions)
[![codecov](https://codecov.io/github/vincentk/dedekind/graph/badge.svg?token=NO83DSL8GZ)](https://codecov.io/github/vincentk/dedekind)

# Dedekind 

[ 🌸 Don't panic. ]

### Computational Structuralism In Modern C++23

The `dedekind` library is an attempt at faithful translation of concepts from math taxtbooks to modern C++ `concept`s, with the intention to define an embedded domain specific language for math. 

The overall architecture rests on two pillars: the C++ language on the one hand, and category theory on the other hand.
 - The choice of C++ is motivated by its strong but quite flexible type system and its status as a mainstream systems programming language.
Specifically, recent versions of C++ allow for the simulation of functional programming languages (higher-kinded types, dependent types) via template metaprogramming techniques such as concepts and traits.
 - The choice of category theory is motivated by the observation that it bridges the gap from the other end: typed $\lambda$-calculus can be expressed in terms of established math textbook terminology.

The hope is that if the translation from math to C++ (and, for validation, the other way around) results in a system which is easily recognizable by a mathematician, a C++ programmer.
In this context, the C++ compiler and a strict build system are used both as a type-checker and optimizer for the resulting code.

An AI assisted during the development of this project.

### Futher reading:

* Build: the build instructions are available through the [CMakeLists.txt](CMakeLists.txt) and controlled through the [build action](.github/workflows/cmake.yml).
* Doxygen: https://vincentk.github.io/dedekind/
