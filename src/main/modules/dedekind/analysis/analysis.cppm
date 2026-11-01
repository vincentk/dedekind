/**
 * @file dedekind/analysis/analysis.cppm
 * @brief kernels, forms, exterior calculus, Hamiltonian flow.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section analysis__Description
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
 *
 * @section analysis__Roadmap Roadmap (work in progress)
 * @li <a href="https://github.com/vincentk/dedekind/issues/286">#286
 *     undergraduate calculus</a>: present in part are sequence limits and
 *     series, the central-difference derivative, the trapezoidal integral
 *     and the FTC as numerical hypotheses; still open are limits and
 *     continuity of functions, a differentiability concept, Taylor series,
 *     the integral theorems of vector calculus, initial-value problems and
 *     the differential algebra.
 * @li <a href="https://github.com/vincentk/dedekind/issues/272">#272
 *     Denotational Automatic Differentiation</a>: forward-mode AD by @c Dual
 *     (a ring, ε² = 0); still open are @c DualVector with a directional
 *     seed, Jacobians computed from it and their chain rule, the @c OneForm
 *     bridge and nonsmooth derivatives.
 * @li <a href="https://github.com/vincentk/dedekind/issues/273">#273
 *     Kernel Functions and Feature Maps</a>: the Gaussian kernel; still open
 *     are the positive-definiteness witness, the Gabor kernel and the kernel
 *     rig (sums and products).
 */

export module dedekind.analysis;

export import :dual;  // Dual numbers 𝔻 = a + bε; forward-mode AD carrier
                      // (relocated from :numbers at PR #513).
export import :exterior;
export import :ftc;
export import :forms;
export import :hamilton;
export import :kernels;
