/**
 * @file geometry.cppm
 * @brief Affine ⊂ InnerProduct ⊂ Euclidean ⊂ Hilbert.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section geometry__Structural_Geometry
 * "Geometrie ist nicht die Lehre von den Dingen, sondern von der Art und Weise,
 *  wie sie koexistieren. Raum ist eine relationale Ordnung koexistierender
 * Objekte." (Geometry is not the study of things, but of the way in which they
 * coexist. Space is a relational order of coexisting objects.) — Bernulf
 * Kanitscheider, 'Geometrie und Wirklichkeit' (1971).
 *
 * @section geometry__Geometric_Taxonomy
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
 *
 * @note "Throughout my whole life as a mathematician, the possibility of making
 * explicit, elegant computations has always come out by itself, as a byproduct
 * of a thorough conceptual understanding of what was going on." — Alexander
 * Grothendieck, letter to Ronald Brown (April 12, 1983).
 */

export module dedekind.geometry;

/** @section geometry__Relational_Foundation */
export import :affine;         // The Point-Vector Duality
export import :linear_map;     // Concrete finite-dimensional linear maps
export import :inner_product;  // The Bilinear Mapping ⟨-, -⟩
export import :outer_product;  // The Dyadic Mapping u ⊗ v (rank-1 / tensor) (#535)
export import :function;  // Function spaces T^D as infinite-dim vector spaces
                          // (#537)

/** @section geometry__Metric_Completion */
export import :euclidean;  // The Normed Space (L2-Topology)
export import :hilbert;    // The Complete Unitary Space (L2-Convergence)

/** @section geometry__Geometric_Lattices */
export import :lattice;  // Discretizations of continuous spaces (ℤⁿ ↪ ℝⁿ)

/** @section geometry__Tangent_Bundle_Concepts_Flat */
export import :tangent;  // IsTangentBundle (flat case); canonical witness
                         // Dual<F> lives in :analysis:dual.  Bundle structure
                         // on non-flat manifolds tracked under #185.

/**
 * @note The build order is critical:
 * :affine must be scanned before :euclidean, as an Euclidean space
 * is an Affine space equipped with a specific Inner Product.
 */
