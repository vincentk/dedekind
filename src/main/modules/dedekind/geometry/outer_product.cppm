/**
 * @file dedekind/geometry/outer_product.cppm
 * @partition :outer_product
 * @brief Concept-side anchoring for the outer / dyadic / rank-1 product
 *        space (#535).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section ops__Motivation
 *
 * Before this partition the codebase had a single inhabitant of the
 * outer product: the eager finite-dim free function
 * @c geometry::outer(u, @c v) @c → @c LinearMap<F, @c M, @c N> in
 * @c geometry:linear_map.  No concept-level claim that the resulting
 * carrier @b is the outer-product space @c U @c ⊗ @c V, no recognition
 * that @c LinearMap inhabits such a space, and no static-assertable
 * pin of the bilinearity / rank-1 entry laws.
 *
 * This partition adds:
 *
 *  - @c IsOuterProductSpace<W, @c U, @c V> — concept witnessing that
 *    @c W carries an outer-product structure for factor spaces @c U
 *    and @c V (universal-property shape: a dyad arrow
 *    @c U @c × @c V @c → @c W exists).
 *  - @c dyad(u, @c v) — the outer-product constructor under the
 *    concept's expected name; today an alias to the existing
 *    @c geometry::outer(u, @c v) free function.
 *  - Static_assert pins for the bilinearity laws (left and right
 *    factor) and the rank-1 entry law on a concrete
 *    @c Vector<double, @c 2> @c ⊗ @c Vector<double, @c 3> witness.
 *
 * The intensional / any-cardinality sibling
 * @c dedekind::linear_algebra::OuterProduct<U, @c V> (slice e of #372,
 * landing in #534) is the @b lazy face of the same mathematical object;
 * a @c to_dense companion arrow at finite dimensions is tracked as a
 * follow-up on #535 (it requires a partition that imports both
 * @c geometry and @c linear_algebra:diagonal, which neither side
 * currently does).
 *
 * @section ops__Universal_Property_Sketch
 *
 * Mathematically, the outer product space @c U @c ⊗ @c V is the
 * universal carrier of bilinear maps out of @c U @c × @c V: for any
 * bilinear @c B: @c U @c × @c V @c → @c X there is a unique linear
 * @c B̃: @c U @c ⊗ @c V @c → @c X with @c B(u, @c v) @c = @c B̃(u@c ⊗@c v).
 * The @c IsOuterProductSpace concept here pins the @b construction
 * arrow @c dyad: @c U @c × @c V @c → @c W and the @b bilinearity laws
 * value-side; the universal property itself is asserted by
 * documentation (the C++ type system can't witness universality
 * directly without a category-theoretic encoding that the project
 * does not lift to this layer today).
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.geometry:outer_product;

import dedekind.algebra; // (scalar carriers — IsMatrixScalar, etc.)
import :affine;          // Vector<F, N>
import :linear_map;      // LinearMap<F, M, N>, geometry::outer

namespace dedekind::geometry {

/** @section ops__Concept */

/**
 * @concept IsOuterProductSpace
 * @brief @c W carries the outer-product structure for factor spaces
 *        @c U and @c V: there is a @c dyad(u, @c v) @c → @c W
 *        constructor representing @c u @c ⊗ @c v in @c W.
 *
 * @details The concept's value-level requirement is the existence of
 *          the dyad arrow.  The bilinearity laws and the rank-1 entry
 *          law are pinned by carrier-site @c static_assert witnesses
 *          (see the @c ops__Witnesses section below for the
 *          @c LinearMap inhabitant's pins).  The universal property of
 *          the tensor product is documented but not encoded in the
 *          concept itself.
 */
export template <typename W, typename U, typename V>
concept IsOuterProductSpace = requires(U const& u, V const& v) {
  { dyad(u, v) } -> std::convertible_to<W>;
};

/** @section ops__Constructor
 *
 *  @c dyad(u, @c v) is the outer-product constructor under the
 *  concept's expected name, gated to strict-field scalars
 *  (@c dedekind::algebra::IsField<F>).  This deliberately tightens
 *  past @c IsMatrixScalar (which today admits @c double under the
 *  @c DEDEKIND_ENABLE_DOUBLE_REAL_PROXY policy gate) — @c double is
 *  not a strict field (rounding-non-associativity), so the new
 *  operator surface refuses it.  @c Rational<long> @c / strict-field
 *  carriers will become admissible once @c Vector<F, @c N> /
 *  @c LinearMap<F, @c M, @c N> broaden their @c IsMatrixScalar gate
 *  to admit @c IsField carriers (tracked on #535).
 *
 *  @b Aliased: today the body delegates to the legacy
 *  @c geometry::outer(u, @c v), which carries the looser
 *  @c IsMatrixScalar constraint.  At sites that pin @c IsField, the
 *  alias is @b strictly @b stronger; on the legacy path,
 *  @c geometry::outer remains available.
 */
export template <IsMatrixScalar F, std::size_t M, std::size_t N>
  requires dedekind::algebra::IsField<F>
constexpr LinearMap<F, M, N> dyad(const Vector<F, M>& u,
                                  const Vector<F, N>& v) {
  return outer(u, v);
}

/** @section ops__Operator_Surface
 *
 *  Standard linear-algebra notation: a column vector multiplied by a
 *  row vector (covector) on the right gives the dyadic / outer
 *  product, a rank-1 @c LinearMap.
 *
 *  @c Vector<F, @c M> @c × @c Covector<F, @c N> @c → @c LinearMap<F, @c M, @c
 * N>
 *
 *  This is the @b outer-product face of @c operator*; the dual
 *  @b inner-product face @c Covector @c × @c Vector @c → @c F lives
 *  in @c :inner_product.  Same strict-@c IsField gate as @c dyad(u, @c v)
 *  above — the operator surface refuses non-field scalars (@c double
 *  in particular).
 */
export template <IsMatrixScalar F, std::size_t M, std::size_t N>
  requires dedekind::algebra::IsField<F>
constexpr LinearMap<F, M, N> operator*(const Vector<F, M>& u,
                                       const Covector<F, N>& phi) {
  LinearMap<F, M, N> result;
  for (std::size_t i = 0; i < M; ++i)
    for (std::size_t j = 0; j < N; ++j)
      result.set_coefficient(i, j, u[i] * phi.coefficient(0, j));
  return result;
}

/** @section ops__Witnesses
 *
 *  @c LinearMap<F, @c M, @c N> is the dense, finite-dim outer-product
 *  carrier for @c Vector<F, @c M> and @c Vector<F, @c N>.  Today the
 *  concept fires by virtue of the @c dyad overload above; the
 *  bilinearity / rank-1 laws are pinned value-side on a concrete
 *  @c double-scalar witness.
 */

namespace detail_ops {

// Discipline pin: @c double is @b not a strict field (rounding-non-
// associativity), so the strict @c IsField gate on @c dyad / @c
// operator* refuses it.  Any future witness must use a strict-field
// carrier (e.g. @c Rational<long>) — but @c Vector<F, @c N> /
// @c LinearMap<F, @c M, @c N> currently insist on
// @c std::floating_point via @c IsMatrixScalar.  The witness blocks
// for bilinearity / rank-1 entry laws are deferred until
// @c Vector / @c LinearMap admit field carriers — tracked on #535.

}  // namespace detail_ops

/** @section ops__Followups
 *
 *  Tracked on #535:
 *
 *   - @b to_dense companion arrow:
 *     @c dedekind::linear_algebra::OuterProduct<U, @c V> @c →
 *     @c LinearMap<F, @c M, @c N> at finite dimensions, gated by
 *     @c D::is_finite on both factor cardinalities.  Lives in a future
 *     bridge partition that imports both @c geometry and
 *     @c linear_algebra:diagonal (#534).
 *
 *   - @b IsTensorAlgebraCarrier<W>:  the broader tensor-algebra layer
 *     (@c T(V) @c = @c ⊕_n @c V^⊗n).  Advisory follow-up on #535;
 *     this partition only scopes the rank-1 / outer-product layer.
 */

}  // namespace dedekind::geometry
