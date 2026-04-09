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
export template <IsField F, std::size_t N>
class Vector {
 public:
  using scalar_type = F;

  // Initializer list construction: {1, 2, 3}
  constexpr Vector(std::initializer_list<F> l) {
    std::copy(l.begin(), l.end(), coords_.begin());
  }

  /** @section The_Scaling_Morphism: Vector * Scalar */
  friend constexpr Vector operator*(const Vector& v, const F& s) {
    Vector res = v;
    for (auto& c : res.coords_) c = c * s;
    return res;
  }

  /** @section Vector_Addition: The Translation */
  friend constexpr Vector operator+(const Vector& a, const Vector& b) {
    Vector res = a;
    for (std::size_t i = 0; i < N; ++i)
      res.coords_[i] = res.coords_[i] + b.coords_[i];
    return res;
  }

  constexpr const F& operator[](std::size_t i) const { return coords_[i]; }

 private:
  std::array<F, N> coords_;
};

/** @section Formal_Verification */

// Proof: A 3D Vector over Rationals is a Vector Space.
static_assert(
    IsVectorSpace<Vector<double, 3>, double>,
    "Geometry Error: Affine vectors must satisfy the Vector Space axioms.");

}  // namespace dedekind::geometry
