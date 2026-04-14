# Contributing to Dedekind

Thank you for your interest in contributing!

## How to contribute

1. **Fork** the repository on GitHub and clone your fork locally.
2. **Pick an issue** from the [issue tracker](https://github.com/vincentk/dedekind/issues).
   If you have a new idea, open an issue first so scope and direction can be agreed on before
   you invest time in an implementation.
3. **Create a branch** named after the issue(s) you are addressing, e.g.
   `feat/issues-40-121`.
4. **Implement and test** your changes.  The project builds with CMake + Ninja:
   ```bash
   cmake -B build -G Ninja
   cmake --build build
   ctest --test-dir build --output-on-failure
   ```
5. **Format** before committing — the CI will reject unformatted code:
   ```bash
   make format   # runs clang-format-21 on all *.cpp / *.cppm files
   ```
6. **Open a pull request** against `main` from your fork branch.  Draft PRs are welcome
   early; mark it ready for review once CI is green.

## Alignment with the codebase

- Follow the module/partition layout described in `CMakeLists.txt` and the existing
  `main` partitions.
- Mirror the doxygen header style used in neighbouring files (brief, partition summary,
  copyright notice, a quote).
- Keep concepts and naming aligned with the textbook literature cited in
  `docs/report/references.bib`.  UTF-8 mathematical symbols (ℤ, ℝ, …) are preferred
  over verbose ASCII alternatives.
- Add or extend tests in the corresponding `src/test/cpp/modules/…` file.

## Questions

Open an issue or leave a comment on an existing one — contributions of any size are
welcome.
