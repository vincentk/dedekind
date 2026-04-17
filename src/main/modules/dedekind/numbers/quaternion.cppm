/**
 * @file dedekind/numbers/quaternion.cppm
 * @partition :quaternion
 * @brief Level 9: The Quaternion Division Ring (ℍ).
 *
 * @section Algebraic_Structure
 * The quaternions ℍ form a non-commutative division ring (a skew-field):
 *   - (ℍ, +) is an abelian group
 *   - (ℍ \ {0}, ×) is a group (non-abelian)
 *   - ij = k, jk = i, ki = j,  ji = -k, kj = -i, ik = -j
 *   - i² = j² = k² = ijk = -1
 *
 * @section Vector_Space_Embedding
 * Quaternions embed into ℝ⁴ via q = a + bi + cj + dk ↦ (a, b, c, d).
 * This identifies ℍ as a 4-dimensional real vector space.
 *
 * @section Matrix_Representation
 * The left-multiplication map L_q : ℍ → ℍ, p ↦ q×p, is an ℝ-linear map.
 * In the basis {1, i, j, k} its matrix is:
 *   L_q =  ⎡ a  -b  -c  -d ⎤
 *           ⎢ b   a  -d   c ⎥
 *           ⎢ c   d   a  -b ⎥
 *           ⎣ d  -c   b   a ⎦
 *
 * This satisfies: as_matrix(q) * as_vector(p) == as_vector(q * p).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "I think I have already said somewhere that mathematics is the art of
 * giving the same name to different things."
 *       -- Henri Poincare, Science and Method (1908)
 */
module;
#include <concepts>

export module dedekind.numbers:quaternion;

import dedekind.category;
import dedekind.geometry;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::geometry;

/**
 * @concept IsQuaternionScalar
 * @brief Scalar type suitable as quaternion components.
 */
export template <typename R>
concept IsQuaternionScalar = requires(R a, R b) {
  R{};
  { a + b } -> std::same_as<R>;
  { a - b } -> std::same_as<R>;
  { a * b } -> std::same_as<R>;
  { -a } -> std::same_as<R>;
} && std::equality_comparable<R>;

/**
 * @class Quaternion
 * @brief Hamilton's hypercomplex numbers q = a + bi + cj + dk.
 *
 * @tparam R  The scalar component type (e.g. double, float).
 *
 * Multiplication is non-commutative: ij = k but ji = -k.
 */
export template <typename R>
  requires IsQuaternionScalar<R>
class Quaternion {
 public:
  using scalar_type = R;

  constexpr Quaternion(R a = R{}, R b = R{}, R c = R{}, R d = R{})
      : a_(a), b_(b), c_(c), d_(d) {}

  constexpr R w() const { return a_; }  // real part
  constexpr R x() const { return b_; }  // i component
  constexpr R y() const { return c_; }  // j component
  constexpr R z() const { return d_; }  // k component

  friend constexpr bool operator==(const Quaternion&,
                                   const Quaternion&) = default;

  /** @brief q + p component-wise. */
  friend constexpr Quaternion operator+(const Quaternion& q,
                                        const Quaternion& p) {
    return {q.a_ + p.a_, q.b_ + p.b_, q.c_ + p.c_, q.d_ + p.d_};
  }

  /** @brief q - p component-wise. */
  friend constexpr Quaternion operator-(const Quaternion& q,
                                        const Quaternion& p) {
    return {q.a_ - p.a_, q.b_ - p.b_, q.c_ - p.c_, q.d_ - p.d_};
  }

  /** @brief Scalar left-multiplication. */
  friend constexpr Quaternion operator*(const R& s, const Quaternion& q) {
    return {s * q.a_, s * q.b_, s * q.c_, s * q.d_};
  }

  /** @brief Scalar right-multiplication. */
  friend constexpr Quaternion operator*(const Quaternion& q, const R& s) {
    return s * q;
  }

  /**
   * @brief Quaternion multiplication q * p (non-commutative).
   *
   * Using the Hamilton product formula:
   *   (a1 + b1 i + c1 j + d1 k)(a2 + b2 i + c2 j + d2 k)
   */
  friend constexpr Quaternion operator*(const Quaternion& q,
                                        const Quaternion& p) {
    return {
        q.a_ * p.a_ - q.b_ * p.b_ - q.c_ * p.c_ - q.d_ * p.d_,  // w
        q.a_ * p.b_ + q.b_ * p.a_ + q.c_ * p.d_ - q.d_ * p.c_,  // x
        q.a_ * p.c_ - q.b_ * p.d_ + q.c_ * p.a_ + q.d_ * p.b_,  // y
        q.a_ * p.d_ + q.b_ * p.c_ - q.c_ * p.b_ + q.d_ * p.a_   // z
    };
  }

  /** @brief Additive inverse. */
  constexpr Quaternion operator-() const { return {-a_, -b_, -c_, -d_}; }

  /**
   * @brief Conjugate q* = a - bi - cj - dk.
   * Satisfies q * conj(q) = |q|^2.
   */
  constexpr Quaternion conj() const { return {a_, -b_, -c_, -d_}; }

  /**
   * @brief Squared Euclidean norm |q|^2 = a^2 + b^2 + c^2 + d^2.
   */
  constexpr R norm_squared() const {
    return a_ * a_ + b_ * b_ + c_ * c_ + d_ * d_;
  }

 private:
  R a_, b_, c_, d_;
};

/**
 * @brief Embed a quaternion as a 4-dimensional real vector.
 *
 * Formalises the identification ℍ ≅ ℝ⁴ via
 *   q = a + bi + cj + dk  ↦  (a, b, c, d).
 *
 * The dimension HasDimension<Vector<R,4>, 4> is enforced at compile time.
 */
export template <typename R>
  requires IsQuaternionScalar<R> && std::floating_point<R>
constexpr Vector<R, 4> as_vector(const Quaternion<R>& q) {
  return {q.w(), q.x(), q.y(), q.z()};
}

/**
 * @brief Embed a quaternion into its 4×4 left-multiplication matrix.
 *
 * The left-multiplication map L_q : ℍ → ℍ, p ↦ q × p, is ℝ-linear.
 * In the ordered basis {1, i, j, k} the matrix is:
 *
 *   L_q = ⎡ w  -x  -y  -z ⎤
 *          ⎢ x   w  -z   y ⎥
 *          ⎢ y   z   w  -x ⎥
 *          ⎣ z  -y   x   w ⎦
 *
 * This satisfies: as_matrix(q) * as_vector(p) == as_vector(q * p).
 *
 * The map q ↦ L_q is an injective ring homomorphism ℍ ↪ M₄(ℝ).
 */
export template <typename R>
  requires IsQuaternionScalar<R> && std::floating_point<R>
constexpr LinearMap<R, 4, 4> as_matrix(const Quaternion<R>& q) {
  const R w = q.w(), x = q.x(), y = q.y(), z = q.z();
  return {{{w, -x, -y, -z},  // row 0
           {x, w, -z, y},    // row 1
           {y, z, w, -x},    // row 2
           {z, -y, x, w}}};  // row 3
}

}  // namespace dedekind::numbers
