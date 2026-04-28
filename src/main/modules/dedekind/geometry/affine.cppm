/**
 * @file dedekind/geometry/affine.cppm
 * @partition :affine
 * @brief Level 10: Affine spaces — points, vectors, dimension witnesses
 *        (Finite / Countably-Infinite); base of the geometry hierarchy.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Space and time are commonly regarded as the forms of existence of the
 * real world, matter as its substance."
 *       -- Hermann Weyl, Space-Time-Matter (1918)
 */

module;
#include <algorithm>
#include <array>
#include <concepts>
#include <cstddef>
#include <initializer_list>
#include <limits>
#include <type_traits>
#include <utility>

export module dedekind.geometry:affine;

import dedekind.category;
import dedekind.algebra;

namespace dedekind::geometry {

using namespace dedekind::category;
using namespace dedekind::algebra;

/**
 * @section Dimension_Cardinality
 * Tags for the cardinality of a vector's index set (its dimension).
 * Mirrors dedekind::sets::Finite / ℵ_0 semantics in a geometry-local form,
 * avoiding a cross-layer import while keeping terminology consistent.
 */
export struct FiniteDim {
  static constexpr bool is_finite = true;
  static constexpr bool is_countable = true;
};

export struct CountablyInfiniteDim {
  static constexpr bool is_finite = false;
  static constexpr bool is_countable = true;
};

/**
 * @concept HasFiniteDimension
 * @brief A vector type whose coordinate index set is finite.
 */
export template <typename V>
concept HasFiniteDimension = requires {
  { V::dimension } -> std::convertible_to<std::size_t>;
} && (V::dimension != std::numeric_limits<std::size_t>::max());

/**
 * @concept HasCountableDimension
 * @brief A vector type whose coordinate index set is at most countably
 * infinite.
 */
export template <typename V>
concept HasCountableDimension = requires {
  { V::dimension } -> std::convertible_to<std::size_t>;
};

/**
 * @concept HasDimension
 * @brief A vector type with a specific compile-time dimension N.
 */
export template <typename V, std::size_t N>
concept HasDimension = HasFiniteDimension<V> && (V::dimension == N);

/**
 * @concept IsAffine
 * @brief Affine-space witness over scalar carrier S.
 */
export template <typename V, typename S>
concept IsAffine = requires(const V a, const V b, const S s) {
  { a + b } -> std::same_as<V>;
  { a - b } -> std::same_as<V>;
  { a * s } -> std::same_as<V>;
  { s * a } -> std::same_as<V>;
  { V::dimension };
};

/**
 * @class Vector
 * @brief An element of a Coordinate Space over a Field F.
 * @tparam F The scalar field (Rational, Real, or Complex).
 * @tparam N The dimension of the space.
 */
export template <IsFloatingScalar F, std::size_t N>
class Vector {
 public:
  using scalar_type = F;
  static constexpr std::size_t dimension = N;
  using dimension_cardinality = FiniteDim;

  constexpr Vector() = default;

  // Initializer list construction: {1, 2, 3}
  constexpr Vector(std::initializer_list<F> l) : coords_{} {
    auto it = l.begin();
    for (std::size_t i = 0; i < N && it != l.end(); ++i, ++it) coords_[i] = *it;
  }

  friend constexpr bool operator==(const Vector& lhs, const Vector& rhs) {
    for (std::size_t i = 0; i < N; ++i) {
      if (!(lhs.coords_[i] == rhs.coords_[i])) {
        return false;
      }
    }
    return true;
  }

  /** @section The_Scaling_Morphism: Vector * Scalar */
  friend constexpr Vector operator*(const Vector& v, const F& s) {
    Vector res = v;
    for (auto& c : res.coords_) c = c * s;
    return res;
  }

  friend constexpr Vector operator*(const F& s, const Vector& v) {
    return v * s;
  }

  /** @section Vector_Addition: The Translation */
  friend constexpr Vector operator+(const Vector& a, const Vector& b) {
    Vector res = a;
    for (std::size_t i = 0; i < N; ++i)
      res.coords_[i] = res.coords_[i] + b.coords_[i];
    return res;
  }

  friend constexpr Vector operator-(const Vector& a, const Vector& b) {
    Vector res = a;
    for (std::size_t i = 0; i < N; ++i)
      res.coords_[i] = res.coords_[i] - b.coords_[i];
    return res;
  }

  friend constexpr Vector operator-(const Vector& v) {
    Vector res = v;
    for (auto& c : res.coords_) c = -c;
    return res;
  }

  constexpr F& operator[](std::size_t i) { return coords_[i]; }
  constexpr const F& operator[](std::size_t i) const { return coords_[i]; }

 private:
  std::array<F, N> coords_{};
};

/**
 * @class FunctionalVector
 * @brief A vector-like object defined by coordinate evaluation x[i].
 *
 * This supports function-based vectors, including potentially
 * infinite-dimensional vectors where coordinates are computed lazily.
 *
 * The only required operation is indexed evaluation.
 */
export template <IsFloatingScalar F, typename Fn>
  requires requires(const Fn& fn, std::size_t i) {
    { fn(i) } -> std::convertible_to<F>;
  }
class FunctionalVector {
 public:
  using scalar_type = F;
  static constexpr std::size_t dimension =
      std::numeric_limits<std::size_t>::max();
  using dimension_cardinality = CountablyInfiniteDim;

  constexpr explicit FunctionalVector(Fn fn) : fn_(std::move(fn)) {}

  constexpr F operator[](std::size_t i) const { return static_cast<F>(fn_(i)); }

 private:
  Fn fn_;
};

/**
 * @brief Factory helper for function-based vectors.
 */
export template <IsFloatingScalar F, typename Fn>
  requires requires(const Fn& fn, std::size_t i) {
    { fn(i) } -> std::convertible_to<F>;
  }
constexpr auto functional_vector(Fn&& fn) {
  using FnType = std::decay_t<Fn>;
  return FunctionalVector<F, FnType>{std::forward<Fn>(fn)};
}

/**
 * @brief Alias highlighting intent for potentially infinite-dimensional
 * vectors.
 */
export template <IsFloatingScalar F, typename Fn>
using InfiniteVector = FunctionalVector<F, Fn>;

/**
 * @brief Alias for well-behaved infinite sequences viewed as vectors.
 *
 * A sequence (x_n) with coordinate access x[n] is treated as an
 * infinite-dimensional vector in the same carrier.
 */
export template <IsFloatingScalar F, typename Fn>
using SequenceVector = FunctionalVector<F, Fn>;

/**
 * @brief Factory helper emphasizing sequence semantics.
 */
export template <IsFloatingScalar F, typename Fn>
  requires requires(const Fn& fn, std::size_t i) {
    { fn(i) } -> std::convertible_to<F>;
  }
constexpr auto sequence_vector(Fn&& fn) {
  return functional_vector<F>(std::forward<Fn>(fn));
}

/**
 * @brief Embed a scalar as a 1-dimensional vector.
 *
 * Formalises the view that ℝ (or any floating scalar) is a
 * 1-dimensional vector space over itself.  Combined with the tower
 * embeddings (bool → ℕ → ℤ → ℚ → ℝ), any number in the tower can
 * be lifted to a Vector<F,1>.
 */
export template <IsFloatingScalar F>
constexpr Vector<F, 1> as_vector(const F& x) {
  return {x};
}

/** @section Formal_Verification */

// Vector<F, N> exposes a finite, statically-known dimension N and the
// affine surface (+, -, scalar action) required by IsAffine.
static_assert(HasFiniteDimension<Vector<double, 3>>,
              "Vector<double, 3> must satisfy HasFiniteDimension.");
static_assert(HasDimension<Vector<double, 3>, 3>,
              "Vector<double, 3>::dimension must be 3.");
static_assert(IsAffine<Vector<double, 3>, double>,
              "Vector<double, 3> must satisfy IsAffine over double "
              "(closed under +, -, and left/right scalar action).");

// FunctionalVector exposes a countable but generally infinite dimension
// — the typical inhabitant of l² / sequence-space ambients.
static_assert(
    HasCountableDimension<
        FunctionalVector<double, decltype([](std::size_t) { return 0.0; })>>,
    "FunctionalVector<double, Fn> must satisfy HasCountableDimension.");

}  // namespace dedekind::geometry
