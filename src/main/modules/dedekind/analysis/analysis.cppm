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
 */

export module dedekind.analysis;

export import :exterior;
export import :forms;
export import :hamilton;
export import :kernels;
