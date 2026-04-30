/**
 * @file dedekind/analysis/analysis.cppm
 * @brief Level 11: kernels, forms, exterior calculus, Hamiltonian flow.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * Public facade for the current analysis layer.
 *
 * Exported partitions:
 * - :dual     -- Dual numbers 𝔻 = a + bε (ε² = 0); forward-mode AD
 *                carrier (moved from :numbers at PR #513).
 * - :forms    -- differential one-forms and covector conversion.
 * - :exterior -- wedge products and alternating two-forms.
 * - :hamilton -- Poisson brackets and Hamiltonian witnesses.
 * - :kernels  -- Gaussian and reproducing-kernel primitives.
 * - :ftc      -- Fundamental Theorem of Calculus bridge hooks.

 *
 * @note "L'analyse, ce n'est pas seulement calculer; c'est organiser les
 *  structures
 *  qui rendent le calcul intelligible."
 * ("Analysis is not only about calculating; it is about organizing the
 *  structures that make calculation intelligible.")
 * -- Laurent Schwartz, paraphrase
 */

export module dedekind.analysis;

export import :dual;  // Dual numbers 𝔻 = a + bε; forward-mode AD carrier
                      // (relocated from :numbers at PR #513).
export import :exterior;
export import :ftc;
export import :forms;
export import :hamilton;
export import :kernels;
