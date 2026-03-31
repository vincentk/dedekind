/**
 * @file dedekind/geometry/inner_product.cppm
 * @partition :inner_product
 * @brief Level 10.1: The Metric of Angles (Pre-Hilbert Spaces).
 */

export module dedekind.geometry:inner_product;

import :affine;
import dedekind.algebra;

namespace dedekind::geometry {

using namespace dedekind::algebra;

/**
 * @concept IsInnerProductSpace
 * @brief A vector space equipped with an inner product morphism.
 */
export template <typename V, typename F>
concept IsInnerProductSpace = IsVectorSpace<V, F> && requires(V u, V v) {
  /** @brief The Inner Product: Maps two vectors to a scalar. */
  { dot(u, v) } -> std::same_as<F>;
  /** @brief The Norm: Induced by the inner product ||v|| = sqrt(<v, v>). */
  { norm(u) } -> std::convertible_to<F>;
};

/** @section The_Standard_Dot_Product */

template <IsField F, std::size_t N>
constexpr F dot(const Vector<F, N>& u, const Vector<F, N>& v) {
  F res = identity_v<F, std::plus<F>>;
  for (std::size_t i = 0; i < N; ++i) res = res + (u[i] * v[i]);
  return res;
}

/** @brief Induced Norm for Real Species. */
template <IsField F, std::size_t N>
  requires std::floating_point<F>
constexpr F norm(const Vector<F, N>& v) {
  return std::sqrt(dot(v, v));
}

}  // namespace dedekind::geometry
