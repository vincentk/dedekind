/**
 * @file dedekind/geometry/inner_product.cppm
 * @partition :inner_product
 * @brief Level 10.1: Pre-Hilbert (inner-product) spaces over ℝ or ℂ.
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
 * @details Uses the operational `IsFieldLikeScalar` and
 *          `IsVectorSpaceLike` witnesses so IEEE-backed floating carriers can
 *          participate under the active numerical policy, even when the
 *          stricter categorical `IsVectorSpace` proof is intentionally
 *          withheld.
 */
export template <typename V, typename F>
concept IsInnerProductSpace = IsVectorSpaceLike<V, F> && HasInnerProduct<V, F>;

/** @section The_Standard_Dot_Product */

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

/** @section Formal_Verification */

// Vector<F,N> with the standard Euclidean inner product forms an inner product
// space over the scalar field F.
static_assert(HasInnerProduct<Vector<double, 3>, double>,
              "Vector<double,3> must satisfy HasInnerProduct (dot + norm).");

static_assert(IsInnerProductSpace<Vector<double, 3>, double>,
              "Vector<double,3> must be an inner product space over double.");

}  // namespace dedekind::geometry
