/**
 * @file dedekind/geometry/inner_product.cppm
 * @partition :inner_product
 * @brief Pre-Hilbert (inner-product) spaces over ℝ or ℂ.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "The principal aim of mathematical education is to develop certain
 * faculties of the mind, and among these intuition is not the least precious."
 *       -- Henri Poincare, Science and Method (1908)
 */

module;
#include <cmath>
#include <concepts>
#include <cstddef>

export module dedekind.geometry:inner_product;

import :affine;
import :linear_map;  // Covector<F, N> = LinearMap<F, 1, N> for the operator* (φ, v) overload
import dedekind.algebra;

namespace dedekind::geometry {

using namespace dedekind::algebra;

/**
 * @concept HasInnerProduct
 * @brief Carrier exposing a scalar-valued inner-product form and norm.
 */
export template <typename V, typename F>
concept HasInnerProduct = requires(V u, V v) {
  /** @brief The Inner Product: Maps two vectors to a scalar. */
  { dot(u, v) } -> std::same_as<F>;
  /** @brief The Norm: Induced by the inner product ||v|| = sqrt(<v, v>). */
  { norm(u) } -> std::convertible_to<F>;
};

/**
 * @concept IsInnerProductSemimodule
 * @brief A semimodule equipped with an inner-product-like scalar form.
 */
export template <typename M, typename S>
concept IsInnerProductSemimodule = IsSemimodule<M, S> && HasInnerProduct<M, S>;

/**
 * @concept IsInnerProductModule
 * @brief A module equipped with an inner-product-like scalar form.
 */
export template <typename M, typename R>
concept IsInnerProductModule = IsModule<M, R> && HasInnerProduct<M, R>;

/**
 * @concept IsInnerProductSpace
 * @brief A field-like vector carrier equipped with an inner product morphism.
 * @details Uses the operational `HasFieldOperators` and
 *          `HasVectorSpaceOperators` witnesses so IEEE-backed floating carriers
 * can participate under the active numerical policy, even when the stricter
 * categorical `IsVectorSpace` proof is intentionally withheld.
 */
export template <typename V, typename F>
concept IsInnerProductSpace =
    HasVectorSpaceOperators<V, F> && HasInnerProduct<V, F>;

/** @section inner_product__The_Standard_Dot_Product */

export template <std::floating_point F, std::size_t N>
constexpr F dot(const Vector<F, N>& u, const Vector<F, N>& v) {
  F res{};
  for (std::size_t i = 0; i < N; ++i) res = res + (u[i] * v[i]);
  return res;
}

/**
 * @brief Squared Euclidean norm induced by the inner product: <v, v>.
 */
export template <typename V>
  requires requires(const V& v) { dot(v, v); }
constexpr auto euclidean_norm_squared(const V& v) {
  return dot(v, v);
}

/** @brief Induced Norm for Real Species. */
export template <std::floating_point F, std::size_t N>
constexpr F norm(const Vector<F, N>& v) {
  return std::sqrt(dot(v, v));
}

/** @section inner_product__Operator_Surface
 *
 *  Standard linear-algebra notation: a covector applied to a vector
 *  contracts the inner index, giving a scalar.  Equivalently the dual
 *  pairing @c ⟨φ, @c v⟩ written multiplicatively.
 *
 *  @c Covector<F, @c N> @c × @c Vector<F, @c N> @c → @c F
 *
 *  This is the @b inner-product / contraction face of @c operator*;
 *  the dual @b outer-product face @c Vector @c × @c Covector @c →
 *  @c LinearMap lives in @c :outer_product.  Strict-@c IsField gate
 *  on the scalar @c F: @c double is @b not a strict field
 *  (rounding-non-associativity) and is rejected at this surface.
 *  @c Rational<long> @c / strict-field carriers will become
 *  admissible once @c Vector / @c LinearMap broaden their
 *  @c IsMatrixScalar gate (#535).
 */
export template <typename F, std::size_t N>
  requires dedekind::algebra::IsField<F>
constexpr F operator*(const Covector<F, N>& phi, const Vector<F, N>& v) {
  F res{};
  for (std::size_t i = 0; i < N; ++i) res = res + phi.coefficient(0, i) * v[i];
  return res;
}

/** @section inner_product__Formal_Verification */

// Vector<F,N> with the standard Euclidean inner product forms an inner product
// space over the scalar field F.
static_assert(HasInnerProduct<Vector<double, 3>, double>,
              "Vector<double,3> must satisfy HasInnerProduct (dot + norm).");

static_assert(IsInnerProductSpace<Vector<double, 3>, double>,
              "Vector<double,3> must be an inner product space over double.");

// operator*(Covector, Vector) is the inner-product face of *: a covector
// applied to a vector contracts the inner index, yielding a scalar.
// Witnesses on a concrete strict-field carrier (e.g. Rational<long>) are
// deferred until Vector<F, N> / LinearMap<F, M, N> admit IsField scalars
// — today they gate at IsMatrixScalar = std::floating_point. Tracked on
// #535.

}  // namespace dedekind::geometry
