/**
 * @file dedekind/linear_algebra/embeddings.cppm
 * @partition :embeddings
 * @brief Level 12.5b: Canonical regular representations ℂ ↪ M₂, 𝔻 ↪ M₂.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Overview
 * Two faithful ring homomorphisms, both spelled over the same 2×2 carrier:
 *
 *   ℂ ↪ M₂(T):   a + b·i  ↦  [[ a, -b ],
 *                              [ b,  a ]]
 *
 *   𝔻 ↪ M₂(T):   a + b·ε  ↦  [[ a,  b ],
 *                              [ 0,  a ]]
 *
 * The complex embedding is the classical regular representation of ℂ as
 * rotation-and-scaling matrices; the dual embedding is the upper-triangular
 * regular representation of 𝔻 = T[ε]/(ε²), with the nilpotent ε ↦ strict
 * upper-triangular block.
 *
 * Paper-facing carrier is `T = Rational<long>` (ℚ as an exact proxy for ℝ
 * per the #364 / #366 narrowing). The two embeddings are witnessed by
 * `static_assert`s that they preserve the additive and multiplicative units
 * and commute with + and ·. This is the "canonical mappings between ℂ/𝔻 over
 * ℚ and the 2×2 rational matrices" asked for alongside #366.
 *
 * @section Value_Level_vs_Type_Level
 * `Matrix2x2<T, a, b, c, d>` (in `:invertible2x2`) carries entries as NTTPs
 * — it is a type-level object whose equality is type equality. `Matrix2x2V<T>`
 * here is its value-level companion: a plain aggregate with four fields. The
 * embeddings live at the value level because `Complex<T>` and `Dual<T>` are
 * runtime (value) objects; the witnesses below use `constexpr` instances so
 * the verdict is still a compile-time static_assert.
 *
 * Wikipedia: Regular representation, Dual numbers, Automatic differentiation
 *
 * @note "Tout est nombre." — Pythagoras (attrib., via Aristotle, Metaphysics
 * A.5) [Trans: "All is number."]
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.linear_algebra:embeddings;

import dedekind.numbers; // Complex<T>, Dual<T>, Rational<Z>
import :invertible2x2;   // shared namespace + doc references
import :contracts;       // ColumnOrientation, RowOrientation, matrix concepts

namespace dedekind::linear_algebra {

using dedekind::numbers::Complex;
using dedekind::numbers::Dual;

/**
 * @brief Value-level column vector of length 2: the column_type of
 *        `Matrix2x2V`.
 *
 * Carries a `scalar_type`, a compile-time `dimension`, and a
 * `ColumnOrientation` tag so it models the `IsColumnVector` concept in
 * `:contracts`. Arithmetic is component-wise, with a left and a right scalar
 * action.
 */
export template <typename T>
struct Vec2V {
  using scalar_type = T;
  using orientation = ColumnOrientation;
  static constexpr std::size_t dimension = 2;

  T x{};
  T y{};

  friend constexpr bool operator==(const Vec2V&, const Vec2V&) = default;

  friend constexpr Vec2V operator+(const Vec2V& a, const Vec2V& b) {
    return {a.x + b.x, a.y + b.y};
  }
  friend constexpr Vec2V operator-(const Vec2V& a, const Vec2V& b) {
    return {a.x - b.x, a.y - b.y};
  }
  friend constexpr Vec2V operator-(const Vec2V& a) { return {-a.x, -a.y}; }
  friend constexpr Vec2V operator*(const T& s, const Vec2V& a) {
    return {s * a.x, s * a.y};
  }
  friend constexpr Vec2V operator*(const Vec2V& a, const T& s) {
    return {a.x * s, a.y * s};
  }
};

/**
 * @brief Value-level row vector (covector) of length 2: the row_type of
 *        `Matrix2x2V`.
 *
 * Distinguished from `Vec2V` by the `RowOrientation` tag — structurally
 * identical otherwise. The distinction lets the type system keep the
 * horizontal and vertical concatenation views of a matrix separate, even
 * though both views share the same underlying scalar layout.
 */
export template <typename T>
struct Covec2V {
  using scalar_type = T;
  using orientation = RowOrientation;
  static constexpr std::size_t dimension = 2;

  T x{};
  T y{};

  friend constexpr bool operator==(const Covec2V&, const Covec2V&) = default;

  friend constexpr Covec2V operator+(const Covec2V& a, const Covec2V& b) {
    return {a.x + b.x, a.y + b.y};
  }
  friend constexpr Covec2V operator-(const Covec2V& a, const Covec2V& b) {
    return {a.x - b.x, a.y - b.y};
  }
  friend constexpr Covec2V operator-(const Covec2V& a) { return {-a.x, -a.y}; }
  friend constexpr Covec2V operator*(const T& s, const Covec2V& a) {
    return {s * a.x, s * a.y};
  }
  friend constexpr Covec2V operator*(const Covec2V& a, const T& s) {
    return {a.x * s, a.y * s};
  }
};

/**
 * @brief Value-level 2×2 matrix: the runtime companion to NTTP `Matrix2x2`.
 *
 * Stores entries as ordinary constexpr-friendly fields. Provided so that
 * value-carrying maps (e.g. from a `Complex<T>` value) have a natural target
 * without forcing NTTP promotion. Row-major layout matching `Matrix2x2`:
 *
 *     [[ m11, m12 ],
 *      [ m21, m22 ]]
 *
 * Satisfies `IsMatrix<M>` and `IsMatrixOverFieldRingLike<M, T>` from
 * `:contracts`: exposes `row_count`/`column_count`, `column_type`/`row_type`,
 * both decompositions, and the full ring-plus-transpose surface.
 */
export template <typename T>
struct Matrix2x2V {
  using scalar_type = T;
  using column_type = Vec2V<T>;
  using row_type = Covec2V<T>;
  static constexpr std::size_t row_count = 2;
  static constexpr std::size_t column_count = 2;

  T m11{};
  T m12{};
  T m21{};
  T m22{};

  friend constexpr bool operator==(const Matrix2x2V&,
                                   const Matrix2x2V&) = default;

  friend constexpr Matrix2x2V operator+(const Matrix2x2V& A,
                                        const Matrix2x2V& B) {
    return {A.m11 + B.m11, A.m12 + B.m12, A.m21 + B.m21, A.m22 + B.m22};
  }

  friend constexpr Matrix2x2V operator-(const Matrix2x2V& A,
                                        const Matrix2x2V& B) {
    return {A.m11 - B.m11, A.m12 - B.m12, A.m21 - B.m21, A.m22 - B.m22};
  }

  friend constexpr Matrix2x2V operator-(const Matrix2x2V& A) {
    return {-A.m11, -A.m12, -A.m21, -A.m22};
  }

  friend constexpr Matrix2x2V operator*(const T& s, const Matrix2x2V& A) {
    return {s * A.m11, s * A.m12, s * A.m21, s * A.m22};
  }

  friend constexpr Matrix2x2V operator*(const Matrix2x2V& A, const T& s) {
    return {A.m11 * s, A.m12 * s, A.m21 * s, A.m22 * s};
  }

  friend constexpr Matrix2x2V operator*(const Matrix2x2V& A,
                                        const Matrix2x2V& B) {
    return {A.m11 * B.m11 + A.m12 * B.m21, A.m11 * B.m12 + A.m12 * B.m22,
            A.m21 * B.m11 + A.m22 * B.m21, A.m21 * B.m12 + A.m22 * B.m22};
  }

  /** @brief The i-th column (0-based). Out-of-range yields the zero column. */
  constexpr column_type column(std::size_t i) const {
    if (i == 0) return {m11, m21};
    if (i == 1) return {m12, m22};
    return {};
  }

  /** @brief The i-th row (0-based). Out-of-range yields the zero row. */
  constexpr row_type row(std::size_t i) const {
    if (i == 0) return {m11, m12};
    if (i == 1) return {m21, m22};
    return {};
  }

  /** @brief Matrix transpose: reflect across the main diagonal. */
  constexpr Matrix2x2V transpose() const { return {m11, m21, m12, m22}; }
};

/** @brief Value-level identity: `[[1, 0], [0, 1]]`. */
export template <typename T>
inline constexpr Matrix2x2V<T> identity_matrix2x2_v{T{1}, T{0}, T{0}, T{1}};

/** @brief Value-level zero: `[[0, 0], [0, 0]]`. */
export template <typename T>
inline constexpr Matrix2x2V<T> zero_matrix2x2_v{T{0}, T{0}, T{0}, T{0}};

/** @section Canonical_Embedding_ℂ_↪_M₂ */

/**
 * @brief Regular representation of ℂ inside M₂(T):
 *        `a + b·i ↦ [[a, -b], [b, a]]`.
 *
 * This is an injective ring homomorphism: preserves +, ·, 0, and 1. The image
 * is exactly the subring of `rotation-and-scaling` 2×2 matrices — matrices
 * commuting with the canonical `J = [[0, -1], [1, 0]]` and closed under
 * matrix addition and multiplication. Over `T = Rational<Z>` the map is
 * exact and invertible on its image via `complex_from_matrix2x2`.
 *
 * @tparam T A carrier supporting `+`, `-` (unary and binary), and `*` in the
 *           sense of `IsComplexScalar`.
 */
export template <typename T>
constexpr Matrix2x2V<T> as_matrix2x2(const Complex<T>& z) {
  return {z.real(), -z.imag(), z.imag(), z.real()};
}

/**
 * @brief Left inverse of `as_matrix2x2` on matrices of complex form.
 * @details Recovers `(a, b)` from `[[a, -b], [b, a]]` by reading the first
 *          column. Called a "left inverse" because
 *          `complex_from_matrix2x2(as_matrix2x2(z)) == z` for every `z`, but
 *          the opposite direction is only true for M in the image of ℂ ↪
 *          M₂(T). The caller is responsible for that precondition; no
 *          runtime check is issued.
 */
export template <typename T>
constexpr Complex<T> complex_from_matrix2x2(const Matrix2x2V<T>& M) {
  return Complex<T>{M.m11, M.m21};
}

/** @section Canonical_Embedding_𝔻_↪_M₂ */

/**
 * @brief Regular representation of 𝔻 inside M₂(T):
 *        `a + b·ε ↦ [[a, b], [0, a]]`.
 *
 * This is an injective ring homomorphism from the dual numbers 𝔻 = T[ε]/(ε²)
 * into upper-triangular 2×2 matrices with equal diagonal. The nilpotent
 * relation `ε² = 0` lifts exactly to `[[0, 1], [0, 0]]² = [[0, 0], [0, 0]]`.
 * Preserves +, ·, 0, and 1; witnessed below.
 *
 * Forward-mode automatic differentiation (already reified on `Dual<F>`) is
 * structurally the action of this embedding: applying a polynomial to `Dual`
 * equals applying the polynomial to its matrix image.
 *
 * @tparam T A carrier with ring-like arithmetic compatible with `Dual<T>`.
 */
export template <typename T>
constexpr Matrix2x2V<T> as_matrix2x2(const Dual<T>& d) {
  return {d.value(), d.derivative(), T{0}, d.value()};
}

/**
 * @brief Left inverse of `as_matrix2x2` on matrices of dual form.
 * @details Recovers `(a, b)` from `[[a, b], [0, a]]`. Precondition: M is in
 *          the image of 𝔻 ↪ M₂(T) — i.e. `m11 == m22` and `m21 == 0`. Not
 *          enforced; see the `complex_from_matrix2x2` note.
 */
export template <typename T>
constexpr Dual<T> dual_from_matrix2x2(const Matrix2x2V<T>& M) {
  return Dual<T>{M.m11, M.m12};
}

/** @section Ring_Homomorphism_Witnesses_over_ℚ
 *
 *  The paper-facing existential proofs that both embeddings are ring
 *  homomorphisms — exact at compile time over `Rational<long>`.
 */
namespace detail {

using Rat = dedekind::numbers::Rational<long>;

// Two concrete ℚ-valued complex numbers with non-trivial real and imaginary
// parts, chosen so that z*w has distinct real and imaginary components and
// the homomorphism property is not satisfied by a symmetry coincidence.
inline constexpr Complex<Rat> z_probe{Rat{1L}, Rat{2L}};
inline constexpr Complex<Rat> w_probe{Rat{3L}, Rat{-5L}};

static_assert(as_matrix2x2(Complex<Rat>{Rat{0L}, Rat{0L}}) ==
                  zero_matrix2x2_v<Rat>,
              "ℂ ↪ M₂(ℚ): the complex zero maps to the matrix zero.");
static_assert(as_matrix2x2(Complex<Rat>{Rat{1L}, Rat{0L}}) ==
                  identity_matrix2x2_v<Rat>,
              "ℂ ↪ M₂(ℚ): the complex unit maps to the matrix identity.");
static_assert(as_matrix2x2(z_probe + w_probe) ==
                  as_matrix2x2(z_probe) + as_matrix2x2(w_probe),
              "ℂ ↪ M₂(ℚ) preserves addition.");
static_assert(as_matrix2x2(z_probe* w_probe) ==
                  as_matrix2x2(z_probe) * as_matrix2x2(w_probe),
              "ℂ ↪ M₂(ℚ) preserves multiplication.");
static_assert(complex_from_matrix2x2(as_matrix2x2(z_probe)) == z_probe,
              "ℂ ↪ M₂(ℚ) admits a left inverse on its image.");

// Two concrete ℚ-valued dual numbers. `d_probe` has a non-zero derivative so
// the matrix is strictly upper-triangular (not diagonal), and the product
// mixes the nilpotent part non-trivially.
inline constexpr Dual<Rat> d_probe{Rat{2L}, Rat{3L}};
inline constexpr Dual<Rat> e_probe{Rat{-1L}, Rat{5L}};

static_assert(as_matrix2x2(Dual<Rat>{Rat{0L}, Rat{0L}}) ==
                  zero_matrix2x2_v<Rat>,
              "𝔻 ↪ M₂(ℚ): the dual zero maps to the matrix zero.");
static_assert(as_matrix2x2(Dual<Rat>{Rat{1L}, Rat{0L}}) ==
                  identity_matrix2x2_v<Rat>,
              "𝔻 ↪ M₂(ℚ): the dual unit maps to the matrix identity.");
static_assert(as_matrix2x2(d_probe + e_probe) ==
                  as_matrix2x2(d_probe) + as_matrix2x2(e_probe),
              "𝔻 ↪ M₂(ℚ) preserves addition.");
static_assert(as_matrix2x2(d_probe* e_probe) ==
                  as_matrix2x2(d_probe) * as_matrix2x2(e_probe),
              "𝔻 ↪ M₂(ℚ) preserves multiplication.");
static_assert(dual_from_matrix2x2(as_matrix2x2(d_probe)) == d_probe,
              "𝔻 ↪ M₂(ℚ) admits a left inverse on its image.");

// Nilpotency of ε lifts exactly to the matrix world.
inline constexpr Dual<Rat> eps_q{Rat{0L}, Rat{1L}};
static_assert(as_matrix2x2(eps_q* eps_q) == zero_matrix2x2_v<Rat>,
              "ε² = 0 in 𝔻 lifts to the zero matrix under the embedding.");

/** @section Matrix_Structural_Concept_Witnesses (Matrix2x2V<ℚ>)
 *
 *  The nine-claim slogan-pack from `:contracts`, witnessed by the concrete
 *  ℚ-valued Matrix2x2V / Vec2V / Covec2V triple. Each `static_assert` pins
 *  one claim; if any concept gets tightened in a way Matrix2x2V fails to
 *  satisfy, the build breaks here rather than silently at a downstream
 *  call site.
 */

// (1) A matrix column is a vector — `column_type` is an IsColumnVector.
static_assert(IsColumnVector<Vec2V<Rat>>,
              "Matrix2x2V's column_type must be a bona fide column vector.");

// (2) A matrix row is a row vector — `row_type` is an IsCovector.
static_assert(IsCovector<Covec2V<Rat>>,
              "Matrix2x2V's row_type must be a bona fide row vector.");

// (3) Vectors and covectors carry a dimension (cardinality-flavoured).
static_assert(HasDimensionCount<Vec2V<Rat>>,
              "Vec2V exposes a compile-time dimension count.");
static_assert(HasDimensionCount<Covec2V<Rat>>,
              "Covec2V exposes a compile-time dimension count.");

// (4) A matrix is both a horizontal concat of columns and a vertical concat
//     of rows — both decompositions are exposed.
static_assert(HasColumnDecomposition<Matrix2x2V<Rat>>,
              "Matrix2x2V admits a horizontal (column) decomposition.");
static_assert(HasRowDecomposition<Matrix2x2V<Rat>>,
              "Matrix2x2V admits a vertical (row) decomposition.");

// (5) A matrix has two dimensionalities: row count and column count.
static_assert(HasMatrixShape<Matrix2x2V<Rat>>,
              "Matrix2x2V exposes row_count and column_count.");

// (6) Matrix over a ring: at least a submodule-like carrier.
static_assert(IsMatrixSubmoduleLike<Matrix2x2V<Rat>, Rat>,
              "Matrix2x2V over ℚ is at least a submodule-like carrier.");

// (7) Matrix over a field: at least a ring (ring ops + transpose).
static_assert(IsMatrixOverFieldRingLike<Matrix2x2V<Rat>, Rat>,
              "Matrix2x2V over ℚ carries the ring-plus-transpose surface.");

// (8) Matrix multiplication is closed (non-commutativity is a property of
//     the operation, not a witness requirement — see the concept docstring).
static_assert(HasMatrixMultiplication<Matrix2x2V<Rat>>,
              "Matrix2x2V is closed under matrix multiplication.");

// (9) All ring ops plus transpose — the umbrella concept for
//     square matrices over a field.
static_assert(IsMatrixAlgebra<Matrix2x2V<Rat>, Rat>,
              "Matrix2x2V<ℚ> is a full matrix algebra in the slogan sense.");

// Decomposition witness: the first column of M = [[1,2],[3,4]] is (1,3)^T,
// and the second row is (3,4). Exercises the runtime accessors.
inline constexpr Matrix2x2V<Rat> decomp_probe{Rat{1L}, Rat{2L}, Rat{3L},
                                              Rat{4L}};
static_assert(decomp_probe.column(0) == Vec2V<Rat>{Rat{1L}, Rat{3L}},
              "column(0) recovers the first column vector.");
static_assert(decomp_probe.column(1) == Vec2V<Rat>{Rat{2L}, Rat{4L}},
              "column(1) recovers the second column vector.");
static_assert(decomp_probe.row(1) == Covec2V<Rat>{Rat{3L}, Rat{4L}},
              "row(1) recovers the second row vector.");

// Transpose witness: (Aᵀ)ᵀ = A, and the entries swap across the diagonal.
static_assert(decomp_probe.transpose().transpose() == decomp_probe,
              "Transpose is an involution.");
static_assert(decomp_probe.transpose() ==
                  Matrix2x2V<Rat>{Rat{1L}, Rat{3L}, Rat{2L}, Rat{4L}},
              "Transpose swaps off-diagonal entries.");

// Non-commutativity of matrix multiplication: an explicit pair A, B over ℚ
// with A*B ≠ B*A. This is the *property* the concept documentation claims;
// witnessing it keeps the documentation honest.
inline constexpr Matrix2x2V<Rat> nc_a{Rat{0L}, Rat{1L}, Rat{0L}, Rat{0L}};
inline constexpr Matrix2x2V<Rat> nc_b{Rat{0L}, Rat{0L}, Rat{1L}, Rat{0L}};
static_assert(nc_a * nc_b != nc_b * nc_a,
              "Matrix multiplication is non-commutative in general "
              "(witness over ℚ: AB ≠ BA for nilpotent A, B).");

}  // namespace detail

}  // namespace dedekind::linear_algebra
