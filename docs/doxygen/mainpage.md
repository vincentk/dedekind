# Dedekind: the C++ API reference {#mainpage}

An embedded language for mathematics in C++: it reads like set-builder notation, and it handles infinite sets given by a predicate as well as finite data.
You state laws about your types.
The compiler checks what it can at build time and uses them to simplify, and what can't be folded runs as ordinary code.

This site is the reference for the C++ modules, generated from the comments in the sources, for quick lookups against the source tree.
It is a work in progress, and the public API may change until a stable release.

## Where to go

- [The project's README](https://github.com/vincentk/dedekind#readme): what the library is for, with its exhibits and a quickstart.
- [The Jlt paper](https://vincentk.github.io/dedekind/jlt.pdf): the fragment of C++23 the library's core is written in, and what it guarantees (a draft).
- [The Python API reference](https://vincentk.github.io/dedekind/python/): `pynzli`, the Python handles over this library, with usage examples checked by the test suite.
- [The source](https://github.com/vincentk/dedekind).

## Reading this reference

The modules live under `src/main/modules/dedekind`, one directory per layer, built in the order the theory depends on them, so a theorem can't be imported before its axioms.
The core the paper reads is the namespace dedekind::category; its concepts follow the textbook's definitions, under the textbook's names.
The layers above it are algebra, order, sets, numbers, relational, sequences, topology, analysis, geometry, linear_algebra, optimization, morphologies and ieee.

Use the *Namespaces* and *Concepts* tabs above to browse, or the search box for a name.
