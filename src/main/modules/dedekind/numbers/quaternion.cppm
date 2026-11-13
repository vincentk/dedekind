/**
 * @file dedekind/numbers/quaternion.cppm
 * @partition :quaternion
 * @brief The Quaternion Division Ring (ℍ).
 *
 * @section quaternion__Algebraic_Structure
 * The quaternions ℍ form a non-commutative division ring (a skew-field):
 *   - (ℍ, +) is an abelian group
 *   - (ℍ \ {0}, ×) is a group (non-abelian)
 *   - ij = k, jk = i, ki = j,  ji = -k, kj = -i, ik = -j
 *   - i² = j² = k² = ijk = -1
 *
 * @section quaternion__Vector_Space_Embedding
 * Quaternions embed into ℝ⁴ via q = a + bi + cj + dk ↦ (a, b, c, d).
 * This identifies ℍ as a 4-dimensional real vector space.
 *
 * @section quaternion__Matrix_Representation
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
import dedekind.sets; // canonical: the normalisation point of a stored part
import :rational;     // the exact witnesses below

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::geometry;

/**
 * @concept IsQuaternionScalar
 * @brief Scalar type suitable as quaternion components.
 */
export template <typename R>
concept IsQuaternionScalar =
    requires(R a, R b) {
      R{};
      { a + b } -> std::same_as<R>;
      { a - b } -> std::same_as<R>;
      { a * b } -> std::same_as<R>;
      { -a } -> std::same_as<R>;
    } && std::equality_comparable<R> &&
    dedekind::sets::HasCanonical<R> &&  // each result stored canonical
    !std::floating_point<R>;  // ℍ over raw floats is not a ring (IEEE)
static_assert(!IsQuaternionScalar<double> && IsQuaternionScalar<int>,
              "IsQuaternionScalar rejects raw floats.");

/**
 * @class Quaternion
 * @brief Hamilton's hypercomplex numbers q = a + bi + cj + dk.
 *
 * @tparam R  The scalar component type (an exact ring, e.g. @c RatExpr).
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
    return made(q.a_ + p.a_, q.b_ + p.b_, q.c_ + p.c_, q.d_ + p.d_);
  }

  /** @brief q - p component-wise. */
  friend constexpr Quaternion operator-(const Quaternion& q,
                                        const Quaternion& p) {
    return made(q.a_ - p.a_, q.b_ - p.b_, q.c_ - p.c_, q.d_ - p.d_);
  }

  /** @brief Scalar left-multiplication. */
  friend constexpr Quaternion operator*(const R& s, const Quaternion& q) {
    return made(s * q.a_, s * q.b_, s * q.c_, s * q.d_);
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
    return made(q.a_ * p.a_ - q.b_ * p.b_ - q.c_ * p.c_ - q.d_ * p.d_,   // w
                q.a_ * p.b_ + q.b_ * p.a_ + q.c_ * p.d_ - q.d_ * p.c_,   // x
                q.a_ * p.c_ - q.b_ * p.d_ + q.c_ * p.a_ + q.d_ * p.b_,   // y
                q.a_ * p.d_ + q.b_ * p.c_ - q.c_ * p.b_ + q.d_ * p.a_);  // z
  }

  /** @brief Additive inverse. */
  constexpr Quaternion operator-() const { return made(-a_, -b_, -c_, -d_); }

  /**
   * @brief Conjugate q* = a - bi - cj - dk.
   * Satisfies q * conj(q) = |q|^2.
   */
  constexpr Quaternion conj() const { return made(a_, -b_, -c_, -d_); }

  /**
   * @brief Squared Euclidean norm |q|^2 = a^2 + b^2 + c^2 + d^2.
   */
  constexpr R norm_squared() const {
    using dedekind::sets::canonical;
    return canonical(a_ * a_ + b_ * b_ + c_ * c_ + d_ * d_);
  }

  /** @brief The canonical form, part by part.  @param q the quaternion. */
  friend constexpr Quaternion canonical(const Quaternion& q) {
    return made(q.a_, q.b_, q.c_, q.d_);
  }

 private:
  /** @brief A result stored part by part, each part's canonical form: the
   *  explicit normalisation point of every operation above. */
  static constexpr Quaternion made(const R& a, const R& b, const R& c,
                                   const R& d) {
    using dedekind::sets::canonical;
    return {canonical(a), canonical(b), canonical(c), canonical(d)};
  }
  R a_, b_, c_, d_;
};

/** @section quaternion__Formal_Verification
 *
 * The Hamilton relations i²=j²=k²=ijk=-1 and non-commutativity ij≠ji
 * are not just documentation — they are compile-time proofs that the
 * Quaternion multiplication table is correct.
 */

// Basis quaternions over the exact ℚ
using ℍ_ℚ = Quaternion<RatExpr<>>;
inline constexpr ℍ_ℚ q_i{RatExpr<>{0}, RatExpr<>{1}, RatExpr<>{0},
                         RatExpr<>{0}};
inline constexpr ℍ_ℚ q_j{RatExpr<>{0}, RatExpr<>{0}, RatExpr<>{1},
                         RatExpr<>{0}};
inline constexpr ℍ_ℚ q_k{RatExpr<>{0}, RatExpr<>{0}, RatExpr<>{0},
                         RatExpr<>{1}};
inline constexpr ℍ_ℚ q_neg1{RatExpr<>{-1}, RatExpr<>{0}, RatExpr<>{0},
                            RatExpr<>{0}};

// i² = j² = k² = -1
// A carrier that is not a term instantiates: its canonical form is the
// identity, here the word cardinal ℤ/2^64.
using W_ = dedekind::sets::ExtensionalCardinal<>;
static_assert(IsQuaternionScalar<W_> &&
                  Quaternion<W_>{W_{1u}, W_{2u}}.norm_squared() == W_{5u},
              "ℍ over a word cardinal: |1 + 2i|² = 5");
static_assert(q_i * q_i == q_neg1, "i² = -1");
static_assert(q_j * q_j == q_neg1, "j² = -1");
static_assert(q_k * q_k == q_neg1, "k² = -1");

// ij = k,  jk = i,  ki = j
static_assert(q_i * q_j == q_k, "ij = k");
static_assert(q_j * q_k == q_i, "jk = i");
static_assert(q_k * q_i == q_j, "ki = j");

// ji = -k,  kj = -i,  ik = -j  (non-commutativity: the sign flips)
static_assert(q_j * q_i == -q_k, "ji = -k");
static_assert(q_k * q_j == -q_i, "kj = -i");
static_assert(q_i * q_k == -q_j, "ik = -j");

// Anchor: multiplication is non-commutative.
static_assert(q_i * q_j != q_j * q_i,
              "Quaternion multiplication is non-commutative: ij ≠ ji.");

}  // namespace dedekind::numbers
