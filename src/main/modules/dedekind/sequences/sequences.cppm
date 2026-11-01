/**
 * @file dedekind/sequences/sequences.cppm
 * @brief Sequences, paths, and (co-)Kleisli triples over directed sets.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Lo que no se sabe expresar no se sabe."
 *       — Miguel de Unamuno (España), Spanish Wikiquote.
 *       [Trans: "What one cannot express, one does not know."]
 *
 * @section sequences__Roadmap Roadmap (work in progress)
 * @li <a href="https://github.com/vincentk/dedekind/issues/286">#286
 *     undergraduate calculus</a>: limits, series, summability and monotone
 *     or bounded sequences; still open are the ratio, root and comparison
 *     tests and power series.
 * @li <a href="https://github.com/vincentk/dedekind/issues/330">#330
 *     undergraduate algorithms and data structures</a>: finite sequences;
 *     still open are stacks, queues, heaps and trees.
 */

export module dedekind.sequences;

export import :net;
export import :convergence;
export import :fold;     // fold(xs, init, op) — the catamorphism
export import :relprod;  // R;S over a finite ℕ-prefix middle (bounded >>, #795)
export import :finite;   // Net / FiniteSeq — compile-time realizations
export import :limits;
export import :path;
export import :samples;
export import :tail;
export import :curve;
export import :ranges;
export import :pst;  // the Pst fragment decided by one fold along the chain
                     // (#975)
