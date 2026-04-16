/**
 * @file dedekind/analysis/analysis.cppm
 * @brief Level 11: Analysis -- kernels, forms, exterior calculus and
 * Hamiltonian structure.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * Public facade for the current analysis layer.
 *
 * Exported partitions:
 * - :forms    -- differential one-forms and covector conversion.
 * - :exterior -- wedge products and alternating two-forms.
 * - :hamilton -- Poisson brackets and Hamiltonian witnesses.
 * - :kernels  -- Gaussian and reproducing-kernel primitives.
 *
 * @quote
 * "L'analyse, ce n'est pas seulement calculer; c'est organiser les structures
 *  qui rendent le calcul intelligible."
 * ("Analysis is not only about calculating; it is about organizing the
 *  structures that make calculation intelligible.")
 * -- Laurent Schwartz, paraphrase
 *
 * @note "Нельзя быть математиком, не будучи в то же время поэтом в душе."
 *       ("It is impossible to be a mathematician without being a poet in
 * soul.")
 *       -- Софья Ковалевская (Sofya Kovalevskaya)
 */

export module dedekind.analysis;

export import :exterior;
export import :forms;
export import :hamilton;
export import :kernels;
