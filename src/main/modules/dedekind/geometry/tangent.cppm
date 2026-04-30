/**
 * @file dedekind/geometry/tangent.cppm
 * @partition :tangent
 * @brief Tangent-bundle concepts (flat case).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Scope_And_Extension
 * Hosts the @b flat-case (element-wise) tangent-bundle concept,
 * sitting next to the trivial-case @c TangentVector / @c CotangentVector
 * aliases in @c :linear_map.  The non-flat manifold-bundle structure
 * (charts, atlases, fibre bundles, transition maps, smoothness) is
 * tracked under issue~\#185 and will land alongside this partition
 * when the manifold abstraction matures, naturally pulling in
 * @c dedekind.topology (manifold-as-topological-space) and
 * @c dedekind.geometry (smooth structure).
 *
 * @section Coherence
 * The canonical witness for @c IsTangentBundle is @c Dual<F> in
 * @c dedekind.analysis:dual --- forward-mode automatic differentiation
 * on @c Dual<F> is exactly arithmetic in the tangent-bundle ring
 * @c T(Spec F) (Hartshorne, @em Algebraic @em Geometry, Ex.\ II.2.8;
 * Eisenbud, @em Commutative @em Algebra, §16.5).  The witness
 * @c static_assert(IsTangentBundle<Dual<F>>) is co-located with the
 * @c Dual definition in @c :analysis (downstream of @c :geometry),
 * not here, so that @c :tangent stays a pure concept partition and
 * downstream carriers can register themselves without depending on
 * each other.
 *
 * @note "Tangent vectors are infinitesimal arrows."
 *       — Folk lemma, attributed to many authors; the formalisation
 *       runs through Hartshorne (above) and Mac Lane,
 *       @em Categories @em for @em the @em Working @em Mathematician,
 *       Springer GTM 5 (2nd ed.\ 1971), §VII.
 */
module;

#include <concepts>

export module dedekind.geometry:tangent;

import dedekind.algebra; // HasRingOperators (ring-shape closure on the bundle ring)

namespace dedekind::geometry {

/**
 * @concept IsTangentBundle
 * @brief A @b flat-case (element-wise) tangent-bundle carrier over a
 *        base scalar: a pair (point, tangent vector) with ring-shaped
 *        arithmetic.
 *
 * @details Following the Hartshorne reading
 *          (@em Algebraic @em Geometry, Ex.\ II.2.8): the affine line
 *          over a commutative ring R has tangent bundle
 *          @f$T(\mathrm{Spec}\,R) =
 * \mathrm{Spec}(R[\varepsilon]/(\varepsilon^2))@f$, and an element of T(Spec R)
 * is a pair (point, tangent vector) decomposed as @c (value, derivative).  Any
 * carrier T that exposes
 *
 *            - a base scalar @c T::value_type,
 *            - accessors @c .value() and @c .derivative() returning the
 *              primal and tangent components, and
 *            - ring-operator closure (the tangent-bundle is itself a
 *              ring under the dual-number arithmetic),
 *
 *          satisfies @c IsTangentBundle.  @c Dual<F> in
 *          @c dedekind.analysis:dual is the canonical witness:
 *          forward-mode automatic differentiation is exactly arithmetic
 *          in the tangent-bundle ring T(Spec F).
 *
 * @section Scope_And_Extension
 * This concept captures the @b flat case --- the carrier alone, no
 * underlying manifold structure --- because over the affine line the
 * tangent bundle is canonically isomorphic to the @em total @em space
 * @c F^2.  Two extensions sit one layer up and are explicitly
 * out of scope here:
 *
 *   1. @b Bundle @b structure on a non-flat manifold: an
 *      @c IsTangentBundleStructure<TM, M> concept would pair a total
 *      space @c TM with its base manifold @c M, plus the projection
 *      @c π: TM → M, transition maps, and smoothness conditions.  This
 *      requires the @b manifold @b abstraction (charts, atlases, fibre
 *      bundles, smooth structure) which is tracked under issue~\#185
 *      and would naturally live partly in @c dedekind.topology
 *      (manifold-as-topological-space) and partly in
 *      @c dedekind.geometry / @c dedekind.analysis (smooth structure +
 *      tangent-bundle structure).
 *   2. @b Higher-order jets and @b finite-difference duals: jet spaces
 *      @f$J^k(M, R)@f$ for higher-order AD; finite-difference duals
 *      @f$R[\Delta]/(\Delta^{k+1})@f$ for the discrete-side reading
 *      (combinatorial derivative, generating-function calculus).  When
 *      either lands, this partition gains a refined hierarchy.
 *
 * @c TangentVector / @c CotangentVector aliases for the trivial flat
 * case already exist in @c :linear_map (semantic aliases for
 * @c Vector<F, N> with the FIXME pointing at \#185 for non-flat
 * lifting).  This concept is the algebraic counterpart at the
 * tangent-bundle ring level.
 *
 * @see Hartshorne, @em Algebraic @em Geometry (Springer GTM 52, 1977),
 *      Exercise II.2.8 --- the algebraic-geometric tangent bundle as
 *      Spec(R[ε]/(ε²)).
 * @see Eisenbud, @em Commutative @em Algebra @em with @em a @em View
 *      @em Toward @em Algebraic @em Geometry (Springer GTM 150, 1995),
 *      §16.5 --- universal first-order infinitesimal deformations.
 * @see Issue \#185 (manifold abstraction: charts, atlas, fibre bundles)
 *      --- the upstream abstraction needed to lift this flat-case
 *      concept to non-trivial manifolds.
 */
export template <typename T>
concept IsTangentBundle =
    std::regular<T> && dedekind::algebra::HasRingOperators<T> && requires(T t) {
      typename T::value_type;
      { t.value() } -> std::same_as<typename T::value_type>;
      { t.derivative() } -> std::same_as<typename T::value_type>;
    };

}  // namespace dedekind::geometry
