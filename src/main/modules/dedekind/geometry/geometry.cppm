/**
 * @file geometry.cppm
 * @module dedekind.geometry
 * @brief Level 9: Geometric Species (Affine ⊂ InnerProduct ⊂ Euclidean ⊂
 * Hilbert).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Structural_Geometry
 * "Geometrie ist nicht die Lehre von den Dingen, sondern von der Art und Weise,
 *  wie sie koexistieren. Raum ist eine relationale Ordnung koexistierender
 * Objekte." (Geometry is not the study of things, but of the way in which they
 * coexist. Space is a relational order of coexisting objects.) — Bernulf
 * Kanitscheider, 'Geometrie und Wirklichkeit' (1971).
 *
 * @section Geometric_Taxonomy
 * Following the Erlangen Program and ETCS foundations, we treat geometry as
 * the study of invariants under specific groups of morphisms:
 *
 * - **Affine (@ref affine)**: The study of parallelisms and ratios.
 *   Defined by the action of a Vector Space (Module) on a set of Points.
 *
 * - **Metric (@ref inner_product, @ref euclidean)**: The study of congruence.
 *   Introduces the Bilinear Form ⟨u, v⟩ to measure angles and the Norm ||v||
 *   to measure the "Magnitude of Being."
 *
 * - **Analytic (@ref hilbert)**: The study of infinite convergence.
 *   Completes the metric space, allowing for Fourier analysis and
 *   Quantum Mechanical state-representations.
 *
 * @see Wikipedia: [Erlangen Program](https://wikipedia.org),
 *                 [Hilbert Space](https://wikipedia.org)
 * @see Kanitscheider, B. (1971). Geometrie und Wirklichkeit.
 */

export module dedekind.geometry;

/** @section Level_9.1: The Relational Foundation */
export import :affine;         // The Point-Vector Duality
export import :inner_product;  // The Bilinear Mapping ⟨-, -⟩

/** @section Level_9.2: The Metric Completion */
export import :euclidean;  // The Normed Space (L2-Topology)
export import :hilbert;    // The Complete Unitary Space (L2-Convergence)

/**
 * @note The build order is critical:
 * :affine must be scanned before :euclidean, as an Euclidean space
 * is an Affine space equipped with a specific Inner Product.
 */
