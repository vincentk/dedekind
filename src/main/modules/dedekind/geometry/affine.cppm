module;
#include <algorithm>
#include <array>
#include <concepts>
#include <cstddef>
#include <initializer_list>

/**
 * @file dedekind/geometry/affine.cppm
 * @partition :affine
 * @brief Level 10: The Synthesis of Space and Magnitude.
 */

export module dedekind.geometry:affine;

import dedekind.category;
import dedekind.algebra;

namespace dedekind::geometry {

using namespace dedekind::category;
using namespace dedekind::algebra;

/**
 * @class Vector
 * @brief An element of a Coordinate Space over a Field F.
 * @tparam F The scalar field (Rational, Real, or Complex).
 * @tparam N The dimension of the space.
 */
export template <std::floating_point F, std::size_t N>
class Vector {
 public:
  using scalar_type = F;
  static constexpr std::size_t dimension = N;

  constexpr Vector() = default;

  // Initializer list construction: {1, 2, 3}
  constexpr Vector(std::initializer_list<F> l) : coords_{} {
    auto it = l.begin();
    for (std::size_t i = 0; i < N && it != l.end(); ++i, ++it) coords_[i] = *it;
  }

  friend constexpr bool operator==(const Vector&, const Vector&) = default;

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

  constexpr F& operator[](std::size_t i) { return coords_[i]; }
  constexpr const F& operator[](std::size_t i) const { return coords_[i]; }

 private:
  std::array<F, N> coords_{};
};

/** @section Formal_Verification */

// Deferred while vector-space witnesses are being retargeted.

}  // namespace dedekind::geometry
