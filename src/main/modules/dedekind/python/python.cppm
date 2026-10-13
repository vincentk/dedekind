/**
 * @file dedekind/python/python.cppm
 * @brief Curated binding facade for external runtimes.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section python__Description
 * The last layer of the build chain: what the Python extensions share, one
 * partition per fragment of the paper, and nothing the library does not
 * already say.  Each extension's bindings live in it (@c nanobind_jlt.cpp,
 * @c nanobind_lwv.cpp, @c nanobind_pst.cpp).  Reduction stays in C++.
 *
 * @note "Le vrai n'est pas le tout, mais le tout dans sa structure."
 *       -- Gaston Bachelard, paraphrase
 *       [Trans: "Truth is not the whole, but the whole in its structure."]
 */

export module dedekind.python;

export import :jlt;  // the erased arrow
export import :lwv;  // materialise: the stored form of a set term
export import :pst;  // Python's int, a bounded chain: the window and its ends
