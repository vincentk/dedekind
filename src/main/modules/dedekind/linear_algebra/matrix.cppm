/**
 * @file dedekind/linear_algebra/matrix.cppm
 * @partition :matrix
 * @brief Matrix-shape carriers — generic n×m matrix aggregator.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section matrix__Scope
 *
 * Aggregates the n×m matrix carrier family.  Today it re-exports
 * the worked first-class instance @c :mat2x2 (Matrix2x2V<T>,
 * Invertible2x2, Identity2x2, Zero2x2, the 2×2 NTTP family);
 * higher-rank specialisations (Mat3x3V<T>, Mat4x4V<T>, … MatNxMV<T>)
 * slot in here as siblings when they ship.  Consumers can either
 * @c import @c :matrix for the whole family, or @c import @c :mat2x2
 * directly when they only need the 2×2 case.
 *
 * The internal-hom @c End_R(V) = M_n(R) is reified for the worked
 * n=2 case via @c is_endomorphism_ring_v<Matrix2x2V<T>, Vec2V<T>>
 * in @c :basis; higher-rank witnesses follow the same shape.
 *
 * @section matrix__CT_Framing
 *
 * In CT lingo a @b matrix is the @b homogeneous @b specialisation of
 * a @b categorical @b table:
 *
 *   - @b Table (heterogeneous): a dependent product
 *     @c Π_{(r,c) @c ∈ @c R×C} @c T_{r,c} — entries indexed by
 *     row×column positions, with possibly distinct entry-types per
 *     position.
 *   - @b Matrix (homogeneous): the exponential / power object
 *     @c T^(R×C) when every entry-type collapses to a single
 *     carrier @c T.
 *
 * This places matrices in Pierce-land's CCC primitives
 * (@c dedekind.category:cartesian: products, exponentials), with
 * the homogeneous-entry constraint as the algebraic refinement.
 * The heterogeneous-block-decomposition view (@c BlockUpperTriangular
 * etc.) is the dependent-product reading; today's family stays
 * homogeneous by convention.
 */
export module dedekind.linear_algebra:matrix;

export import :mat2x2;
