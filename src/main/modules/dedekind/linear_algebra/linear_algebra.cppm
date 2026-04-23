/**
 * @file linear_algebra.cppm
 * @module dedekind.linear_algebra
 * @brief Level 12.5: Backend-agnostic linear operator contracts.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
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
export import :graphblas;
export import :tuple;
export import :matrix;
export import :embeddings;
