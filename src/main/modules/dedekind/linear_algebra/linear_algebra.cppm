/**
 * @file linear_algebra.cppm
 * @brief Backend-agnostic linear operator contracts.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section linear_algebra__Description
 * This layer introduces compile-time contracts for linear operators and backend
 * capability witnesses, without committing to a concrete execution engine.
 *
 * @note "La matematica e l'arte di dare lo stesso nome a cose diverse."
 *       -- Henri Poincare, paraphrase
 *       [Trans: "Mathematics is the art of giving the same name to different
 * things."]
 */
export module dedekind.linear_algebra;

export import :backends;
export import :contracts;
export import :basis;
export import :graphblas;
export import :vec2;      // homogeneous fixed-rank-2 (Vec2V<T> = T²)
export import :matrix;    // n×m matrix aggregator (today: re-exports :mat2x2)
export import :diagonal;  // Diagonal<D, F> + OuterProduct<U, V> at any D incl.
                          // ℵ_0 (#372)
export import :embeddings;
