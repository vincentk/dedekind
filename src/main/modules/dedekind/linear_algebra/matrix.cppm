/**
 * @file dedekind/linear_algebra/matrix.cppm
 * @partition :matrix
 * @brief Level 12.5a: Matrices as rectangular linear maps between tuple
 *        spaces. Hosts both the NTTP (type-level) and value-level matrix
 *        families, plus the concept witnesses that tie them to `:contracts`.
 *
 * @details
 * Following Stammbach's "Lineare Algebra", matrices are interpreted as
 * linear maps on the tuple carriers from `:tuple`. This partition carries
 * two parallel families:
 *
 *   - NTTP family: `Invertible2x2`, `Matrix2x2`, `Identity2x2`, `Zero2x2`,
 *     `DirectSum`, `BlockUpperTriangular`. Entries live in the type, so
 *     algebraic identities like `M * M.inverse() == Identity2x2<T>{}`
 *     reduce to type equalities and are decidable as `static_assert`s.
 *   - Value family: `Matrix2x2V<T>` with identity / zero constants plus
 *     shape-conforming linear actions `Matrix2x2V × Vec2V → Vec2V` and
 *     `Covec2V × Matrix2x2V → Covec2V`. This is the carrier the ℂ/𝔻 →
 *     M₂ regular representations in `:embeddings` target.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Tier0_Existential_Proof
 * Tier 0 of issue #366 — the block-Schur invertibility recursion program.
 * This partition establishes the base case that higher tiers compose:
 * `Invertible2x2<T, a, b, c, d>` is an NTTP-parameterised 2×2 matrix with the
 * full-rank witness `a*d - b*c ≠ 0` enforced by `static_assert` at
 * instantiation, and with `inverse_type` given by the closed-form Cramer
 * expression `(1/det) · [[d, -b], [-c, a]]`.
 *
 * The carrier `T` is expected to be a structural (C++20 NTTP-compatible) type
 * with ring-like arithmetic; the paper-facing existential proof uses
 * `Rational<long>` (ℚ as a proxy for ℝ per the #364 / #366 narrowing), since
 * exact rational arithmetic sidesteps the rounding / structurality questions
 * that `Real<double>` would raise today.
 *
 * @section Module_Action_Alignment
 * `Vec2<T, x, y>` is a structural 2-vector carrying its coordinates as NTTPs.
 * `Invertible2x2` carries a left action on `Vec2` via `operator*(Vec2)`, which
 * is the compile-time analogue of the linear action `GL_2(T) × T² → T²` that
 * underlies `dedekind.algebra:modules` / `:action` conceptually. Because
 * entries live in the type, each matrix-vector product is a type-level
 * rewrite, observable via `static_assert`.
 *
 * @section DSL_Surface (paper-facing)
 *
 *     using Q = dedekind::numbers::Rational<long>;
 *     // M = [[1, 2], [3, 4]],  det = -2
 *     constexpr Invertible2x2<Q, Q{1L}, Q{2L}, Q{3L}, Q{4L}> M{};
 *     static_assert(M * M.inverse() == Identity2x2<Q>{});
 *
 * Wikipedia: Invertible matrix, Cramer's rule, General linear group,
 * Group action
 *
 * @note "Die Erfindung einer Methode ist ein wesentlicher Teil der Mathematik."
 *       — Emmy Noether, attributed to Göttingen lecture remarks on ideal
 *         theory (reported by Alexandrov, 1935).
 *       [Trans: "The invention of a method is an essential part of
 *        mathematics."]
 */
module;

#include <concepts>
#include <cstddef>
#include <type_traits>

export module dedekind.linear_algebra:matrix;

import dedekind.numbers; // Rational<Z> for the ℚ carrier
import :contracts;       // matrix / vector / orientation concepts
import :tuple;           // Vec2 (NTTP), Vec2V / Covec2V (value-level)

namespace dedekind::linear_algebra {

/** @section NTTP_Matrix_Family — compile-time 2×2 matrices used for the
 *  block-Schur invertibility program (#366). Entries live in the type.
 */

/**
 * @brief Full-rank 2×2 matrix with entries as non-type template parameters.
 *
 * Entry layout:
 *
 *     M = [[ a, b ],
 *          [ c, d ]]
 *     det(M) = a·d − b·c        (non-zero; enforced at instantiation)
 *
 * The type carries the entries structurally, so `M * M.inverse()` is a
 * compile-time expression whose result type IS the identity matrix when the
 * inverse is exact over `T` (always true for `T = Rational<Z>`).
 *
 * @tparam T Structural (C++20 NTTP-compatible) carrier type with ring-like
 *           arithmetic. `Rational<Z>` over any `IsInteger Z` is the
 *           canonical choice for the paper-facing ℚ proof.
 * @tparam a,b,c,d Entries as NTTPs of type `T`.
 */
export template <typename T, T a, T b, T c, T d>
struct Invertible2x2 {
  using Domain = T;

  static constexpr T m11 = a;
  static constexpr T m12 = b;
  static constexpr T m21 = c;
  static constexpr T m22 = d;

  static constexpr T det = a * d - b * c;
  static_assert(!(det == T{0}),
                "Invertible2x2 requires full rank: a*d - b*c != 0");

  /**
   * @brief Matrix-matrix composition at the type level: `(A · B)`.
   *
   * The result is `Invertible2x2<T, …>` whose entries are the NTTP-evaluated
   * products-and-sums of the operands'. Invertibility is preserved: the
   * product of two invertible matrices is invertible, and the result type's
   * own `static_assert` re-checks this as a belt-and-braces guard against
   * carriers where a determinant might underflow.
   */
  template <T a2, T b2, T c2, T d2>
  constexpr auto operator*(Invertible2x2<T, a2, b2, c2, d2>) const {
    return Invertible2x2<T, a * a2 + b * c2, a * b2 + b * d2, c * a2 + d * c2,
                         c * b2 + d * d2>{};
  }

  /**
   * @brief Left action on `Vec2`: the compile-time analogue of
   *        `GL_2(T) × T² → T²`.
   *
   * The matrix-vector product is a type-level rewrite — each `Vec2<T, x, y>`
   * operand is mapped to `Vec2<T, a·x + b·y, c·x + d·y>` as a distinct type.
   * Observable via `static_assert(M * v == Vec2<T, …>{})`.
   */
  template <T x, T y>
  constexpr auto operator*(Vec2<T, x, y>) const {
    return Vec2<T, a * x + b * y, c * x + d * y>{};
  }

  /**
   * @brief Closed-form inverse via Cramer's rule.
   *
   *   M^{-1} = (1 / det(M)) · [[ d, -b ],
   *                            [-c,  a ]]
   *
   * Exact over `T = Rational<Z>` (any integer `Z`) and over integer matrices
   * whose determinant is `±1`. For carriers where the division is not exact
   * (e.g. `int` with `|det| > 1`) this expression loses information
   * silently; use `Rational<Z>` for the paper-facing existential proof.
   */
  using inverse_type = Invertible2x2<T, d / det, -b / det, -c / det, a / det>;

  constexpr auto inverse() const { return inverse_type{}; }

  /**
   * @brief Componentwise equality.
   *
   * Because entries are NTTPs, this is a strictly type-level comparison —
   * distinct matrices have distinct types, and the equality is decided at
   * instantiation.
   */
  template <T a2, T b2, T c2, T d2>
  constexpr bool operator==(Invertible2x2<T, a2, b2, c2, d2>) const {
    return a == a2 && b == b2 && c == c2 && d == d2;
  }
};

/**
 * @brief The 2×2 identity matrix over `T`: `[[1, 0], [0, 1]]`.
 *
 * Parameter-free reveal tag; mirrors the `Ø{}` / `Singleton<V>{}` convention
 * from `dedekind.order:halfspace`. Callers write
 * `static_assert(M * M.inverse() == Identity2x2<T>{})` without having to
 * spell the entries. `T` defaults to `int` for simple integer-matrix tests;
 * paper-facing ℚ proofs pass `T = Rational<Z>` explicitly.
 */
export template <typename T = int>
using Identity2x2 = Invertible2x2<T, T{1}, T{0}, T{0}, T{1}>;

/**
 * @brief General (possibly-singular) 2×2 matrix with NTTP entries.
 *
 * Companion to `Invertible2x2` without the full-rank `static_assert`.
 * Used for off-diagonal blocks in `BlockUpperTriangular` where the block
 * matrix's invertibility does not require the off-diagonal blocks to be
 * invertible themselves.
 *
 * Supports the arithmetic needed for Tier 2 of #366: composition, unary
 * negation, and mixed composition with `Invertible2x2`. Does not advertise
 * an inverse — singular `Matrix2x2`s are valid inhabitants of the type.
 */
export template <typename T, T a, T b, T c, T d>
struct Matrix2x2 {
  using Domain = T;

  static constexpr T m11 = a;
  static constexpr T m12 = b;
  static constexpr T m21 = c;
  static constexpr T m22 = d;

  template <T a2, T b2, T c2, T d2>
  constexpr auto operator*(Matrix2x2<T, a2, b2, c2, d2>) const {
    return Matrix2x2<T, a * a2 + b * c2, a * b2 + b * d2, c * a2 + d * c2,
                     c * b2 + d * d2>{};
  }

  template <T a2, T b2, T c2, T d2>
  constexpr auto operator*(Invertible2x2<T, a2, b2, c2, d2>) const {
    return Matrix2x2<T, a * a2 + b * c2, a * b2 + b * d2, c * a2 + d * c2,
                     c * b2 + d * d2>{};
  }

  constexpr auto operator-() const { return Matrix2x2<T, -a, -b, -c, -d>{}; }

  template <T a2, T b2, T c2, T d2>
  constexpr auto operator+(Matrix2x2<T, a2, b2, c2, d2>) const {
    return Matrix2x2<T, a + a2, b + b2, c + c2, d + d2>{};
  }

  template <T a2, T b2, T c2, T d2>
  constexpr bool operator==(Matrix2x2<T, a2, b2, c2, d2>) const {
    return a == a2 && b == b2 && c == c2 && d == d2;
  }
};

/**
 * @brief Mixed composition: `Invertible2x2 · Matrix2x2 → Matrix2x2`.
 *
 * Product of an invertible matrix with a possibly-singular one is in general
 * singular, so the result is typed as `Matrix2x2`. Defined as a free
 * function since it can't live as an `Invertible2x2` member — that member
 * already specialises to `Invertible2x2` return type.
 */
export template <typename T, T a, T b, T c, T d, T a2, T b2, T c2, T d2>
constexpr auto operator*(Invertible2x2<T, a, b, c, d>,
                         Matrix2x2<T, a2, b2, c2, d2>) {
  return Matrix2x2<T, a * a2 + b * c2, a * b2 + b * d2, c * a2 + d * c2,
                   c * b2 + d * d2>{};
}

/** @brief The 2×2 zero matrix — the additive identity in the ring of matrices.
 */
export template <typename T = int>
using Zero2x2 = Matrix2x2<T, T{0}, T{0}, T{0}, T{0}>;

/**
 * @brief Direct sum of two invertibles — the trivial compositional preserver.
 *
 *   (A ⊕ B) is the block-diagonal operator
 *
 *       [[ A,  0 ],
 *        [ 0,  B ]]
 *
 *   with `(A ⊕ B)^{-1} = A^{-1} ⊕ B^{-1}` and
 *        `(A ⊕ B) · (A' ⊕ B') = (A·A') ⊕ (B·B')`.
 *
 * This is Tier 1 of #366: invertibility of a block-diagonal sum is a
 * consequence of invertibility of the blocks, with no Schur-complement
 * recurrence needed. Cheapest move on the block-composition ladder, and
 * the one that builds arbitrarily-large well-behaved matrices from 2×2
 * bases "for free" by iteration.
 *
 * The factors `A` and `B` are expected to be types that participate in
 * composition (`operator*` returning a value of the same structural shape)
 * and expose `inverse()` returning an `inverse_type`; `Invertible2x2<T, …>`
 * is the canonical instance today.
 */
export template <typename A, typename B>
struct DirectSum {
  using first_factor = A;
  using second_factor = B;

  /**
   * @brief Inverse: block-diagonal of the factor inverses.
   *
   * The closed form `(A ⊕ B)^{-1} = A^{-1} ⊕ B^{-1}` is exact whenever
   * each factor's inverse is exact — here, inherited from `Invertible2x2`
   * over `Rational<Z>`.
   */
  using inverse_type =
      DirectSum<typename A::inverse_type, typename B::inverse_type>;

  constexpr auto inverse() const { return inverse_type{}; }

  /**
   * @brief Composition: `(A ⊕ B) · (A' ⊕ B') = (A·A') ⊕ (B·B')`.
   *
   * The two blocks compose independently; the output is again a DirectSum.
   */
  template <typename A2, typename B2>
  constexpr auto operator*(DirectSum<A2, B2>) const {
    using ProdA = decltype(A{} * A2{});
    using ProdB = decltype(B{} * B2{});
    return DirectSum<ProdA, ProdB>{};
  }

  template <typename A2, typename B2>
  constexpr bool operator==(DirectSum<A2, B2>) const {
    return std::same_as<A, A2> && std::same_as<B, B2>;
  }
};

/**
 * @brief Block-upper-triangular matrix — Tier 2 of #366, the degenerate-Schur
 *        case where the lower-left block is zero.
 *
 *   M = [[ A, B ],
 *        [ 0, D ]]
 *
 *   with `A`, `D` invertible; `B` arbitrary (possibly singular); lower-left
 *   block the zero matrix.
 *
 *   Inverse (closed form, no Schur complement needed since `C = 0` makes the
 *   Schur complement `S = D - C·A^{-1}·B = D`):
 *
 *     M^{-1} = [[ A^{-1},  -A^{-1}·B·D^{-1} ],
 *               [     0,         D^{-1}     ]]
 *
 * This is the slice of the block-Schur recurrence that does not require
 * matrix subtraction — a cheap-but-non-trivial advance past `DirectSum`,
 * already demonstrating the recurrence: invertibility of an assembled block
 * matrix emerges from invertibility of its diagonal blocks.
 */
export template <typename A, typename B, typename D>
struct BlockUpperTriangular {
  using top_left = A;
  using top_right = B;
  using bottom_right = D;

  /**
   * @brief Inverse via the degenerate-Schur closed form.
   *
   * The top-right entry is `-A^{-1}·B·D^{-1}`, computed at the type level by
   * chained `operator*` on matrix types.
   */
  using inverse_type =
      BlockUpperTriangular<typename A::inverse_type,
                           decltype(-(typename A::inverse_type{} * B{} *
                                      typename D::inverse_type{})),
                           typename D::inverse_type>;

  constexpr auto inverse() const { return inverse_type{}; }

  /**
   * @brief Block-matrix composition: preserves the upper-triangular shape.
   *
   *     [[A1, B1], [0, D1]] · [[A2, B2], [0, D2]]
   *     = [[ A1·A2,  A1·B2 + B1·D2 ],
   *        [    0,        D1·D2    ]]
   *
   * All three resulting blocks are derived at the type level; the top-right
   * block uses the `Matrix2x2`'s `operator+` for the cross-term sum.
   */
  template <typename A2, typename B2, typename D2>
  constexpr auto operator*(BlockUpperTriangular<A2, B2, D2>) const {
    using NewA = decltype(A{} * A2{});
    using NewB = decltype(A{} * B2{} + B{} * D2{});
    using NewD = decltype(D{} * D2{});
    return BlockUpperTriangular<NewA, NewB, NewD>{};
  }

  template <typename A2, typename B2, typename D2>
  constexpr bool operator==(BlockUpperTriangular<A2, B2, D2>) const {
    return std::same_as<A, A2> && std::same_as<B, B2> && std::same_as<D, D2>;
  }
};

/** @section Value_Level_Matrix_Family — the runtime companion to the NTTP
 *  carriers above. Entries are ordinary constexpr fields; equality is
 *  value equality. `Matrix2x2V` is the paper-facing target of the ℂ/𝔻
 *  regular representations in `:embeddings`.
 */

/**
 * @brief Value-level 2×2 matrix — the runtime companion to NTTP `Matrix2x2`.
 *
 * Stores entries as ordinary constexpr-friendly fields. Row-major layout:
 *
 *     [[ m11, m12 ],
 *      [ m21, m22 ]]
 *
 * Satisfies `IsMatrix<M>` and `IsMatrixOverFieldRingLike<M, T>` from
 * `:contracts`: exposes `row_count`/`column_count`, `column_type`/`row_type`,
 * both decompositions (via `column(i)` / `row(i)`), and the full ring-plus-
 * transpose surface. Columns are `Vec2V<T>` (a 2×1 matrix from `:tuple`);
 * rows are `Covec2V<T>` (a 1×2 matrix).
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

/** @section Shape_Conforming_Linear_Actions
 *
 *  `Matrix2x2V × Vec2V → Vec2V` (left action on column vectors, 2×2 · 2×1)
 *  and `Covec2V × Matrix2x2V → Covec2V` (right action on row vectors,
 *  1×2 · 2×2). The inner dimension `2 = column_count(M) = row_count(v)`
 *  lines up; the outer shape of the result is determined by the outer
 *  dimensions. Mismatched shapes would be structurally ill-typed — no
 *  overload exists for `Matrix2x2V × Covec2V` or `Vec2V × Matrix2x2V`.
 */

/** @brief Matrix-column-vector product: `(2×2) · (2×1) → (2×1)`. */
export template <typename T>
constexpr Vec2V<T> operator*(const Matrix2x2V<T>& A, const Vec2V<T>& v) {
  return {A.m11 * v.x + A.m12 * v.y, A.m21 * v.x + A.m22 * v.y};
}

/** @brief Row-vector-matrix product: `(1×2) · (2×2) → (1×2)`. */
export template <typename T>
constexpr Covec2V<T> operator*(const Covec2V<T>& v, const Matrix2x2V<T>& A) {
  return {v.x * A.m11 + v.y * A.m21, v.x * A.m12 + v.y * A.m22};
}

/** @section Concatenation_Builders — tuples into matrices.
 *
 *  Concatenation is the inverse of decomposition: the column view of a
 *  matrix `M.column(0)` and `M.column(1)` reassembles to `M` under `|`;
 *  similarly the row view reassembles under `/`. This makes the slogan
 *  "a matrix is both a horizontal concatenation of its columns and a
 *  vertical concatenation of its rows" operationally meaningful — the
 *  decomposition and the concatenation are mutual inverses at the value
 *  level, witnessed by `static_assert` below.
 *
 *  @b Operator_choice
 *   - `|` echoes the classical augmented-matrix notation `[A | B]` and is
 *     read as horizontal concatenation (append columns).
 *   - `/` mirrors the LaTeX vertical-split convention `[A / B]`; reads as
 *     vertical concatenation (stack rows).
 *   - `>>` was deliberately avoided: it is already wired throughout
 *     `dedekind.category` as categorical composition / Kleisli bind, and
 *     reusing it for the coproduct-side concatenation operation would
 *     overload the symbol across two distinct categorical ideas.
 *
 *  @b Scalar_→_tuple concatenation is already handled by aggregate
 *  initialisation: `Vec2V<T>{a, b}` and `Covec2V<T>{a, b}` literally
 *  concatenate two scalars into the respective 2×1 / 1×2 tuple.
 */

/**
 * @brief Horizontal concatenation of two column vectors into a 2×2 matrix.
 *
 *     (a.x, a.y)ᵀ | (b.x, b.y)ᵀ = [[a.x, b.x],
 *                                  [a.y, b.y]]
 *
 * Inverse of `Matrix2x2V::column`: `(M.column(0) | M.column(1)) == M`.
 */
export template <typename T>
constexpr Matrix2x2V<T> operator|(const Vec2V<T>& a, const Vec2V<T>& b) {
  return {a.x, b.x, a.y, b.y};
}

/**
 * @brief Vertical concatenation of two row vectors into a 2×2 matrix.
 *
 *     (a.x, a.y) / (b.x, b.y) = [[a.x, a.y],
 *                                [b.x, b.y]]
 *
 * Inverse of `Matrix2x2V::row`: `(M.row(0) / M.row(1)) == M`.
 */
export template <typename T>
constexpr Matrix2x2V<T> operator/(const Covec2V<T>& a, const Covec2V<T>& b) {
  return {a.x, a.y, b.x, b.y};
}

/** @section Concept_Witnesses_over_ℚ — the `:contracts` slogan-pack witnessed
 *  on the concrete Matrix2x2V / Vec2V / Covec2V triple.
 *
 *  The nine structural claims, shape-conformance, transpose involution /
 *  duality, and the orthogonal group O(2, ℚ) — each pinned by
 *  `static_assert`. If a `:contracts` concept is tightened in a way the
 *  concrete carriers fail, the build breaks here.
 */
namespace detail {

using Rat = dedekind::numbers::Rational<long>;

/** @subsection The_Nine_Matrix_Slogans */

// (1) A matrix column is a vector.
static_assert(IsColumnVector<Vec2V<Rat>>,
              "Matrix2x2V's column_type must be a bona fide column vector.");

// (2) A matrix row is a row vector.
static_assert(IsCovector<Covec2V<Rat>>,
              "Matrix2x2V's row_type must be a bona fide row vector.");

// (3) Vectors and covectors carry a dimension (cardinality-flavoured).
static_assert(HasDimensionCount<Vec2V<Rat>>);
static_assert(HasDimensionCount<Covec2V<Rat>>);

// (4) Both horizontal (column) and vertical (row) decompositions.
static_assert(HasColumnDecomposition<Matrix2x2V<Rat>>);
static_assert(HasRowDecomposition<Matrix2x2V<Rat>>);

// (5) Two dimensionalities: row count and column count.
static_assert(HasMatrixShape<Matrix2x2V<Rat>>);

// (6) Matrix over a ring: at least a submodule-like carrier.
static_assert(IsMatrixSubmoduleLike<Matrix2x2V<Rat>, Rat>);

// (7) Matrix over a field: at least a ring (ring ops + transpose).
static_assert(IsMatrixOverFieldRingLike<Matrix2x2V<Rat>, Rat>);

// (8) Matrix multiplication is closed (non-commutative — see (*) below).
static_assert(HasMatrixMultiplication<Matrix2x2V<Rat>>);

// (9) Umbrella: ring ops + transpose, bundled.
static_assert(IsMatrixAlgebra<Matrix2x2V<Rat>, Rat>);

// (*) Non-commutativity witnessed explicitly over ℚ.
inline constexpr Matrix2x2V<Rat> nc_a{Rat{0L}, Rat{1L}, Rat{0L}, Rat{0L}};
inline constexpr Matrix2x2V<Rat> nc_b{Rat{0L}, Rat{0L}, Rat{1L}, Rat{0L}};
static_assert(nc_a * nc_b != nc_b * nc_a,
              "Matrix multiplication is non-commutative: AB ≠ BA for "
              "nilpotent A, B over ℚ.");

// Decomposition witness: the first column of M = [[1,2],[3,4]] is (1,3)^T,
// and the second row is (3,4). Exercises the runtime accessors.
inline constexpr Matrix2x2V<Rat> decomp_probe{Rat{1L}, Rat{2L}, Rat{3L},
                                              Rat{4L}};
static_assert(decomp_probe.column(0) == Vec2V<Rat>{Rat{1L}, Rat{3L}});
static_assert(decomp_probe.column(1) == Vec2V<Rat>{Rat{2L}, Rat{4L}});
static_assert(decomp_probe.row(1) == Covec2V<Rat>{Rat{3L}, Rat{4L}});

// Transpose on matrices: entries swap across the diagonal, involution law.
static_assert(decomp_probe.transpose() ==
              Matrix2x2V<Rat>{Rat{1L}, Rat{3L}, Rat{2L}, Rat{4L}});
static_assert(decomp_probe.transpose().transpose() == decomp_probe);

// Concatenation is the inverse of decomposition — the slogan
// "matrix = horizontal concat of columns = vertical concat of rows" made
// operationally precise. Witnesses the round-trip on a concrete probe.
static_assert(
    (decomp_probe.column(0) | decomp_probe.column(1)) == decomp_probe,
    "Horizontal concat of columns reassembles the matrix.");
static_assert((decomp_probe.row(0) / decomp_probe.row(1)) == decomp_probe,
              "Vertical concat of rows reassembles the matrix.");

/** @subsection Shape_Conformance — type-level ill-typing of mismatched shapes.
 */

static_assert(MatchesAdditiveShape<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);
static_assert(!MatchesAdditiveShape<Matrix2x2V<Rat>, Vec2V<Rat>>,
              "Addition of a 2×2 and a 2×1 must be structurally ill-typed.");
static_assert(!MatchesAdditiveShape<Vec2V<Rat>, Covec2V<Rat>>,
              "Addition of a 2×1 and a 1×2 must be structurally ill-typed.");
static_assert(HasConformingMatrixAddition<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);

static_assert(MatchesMultiplicativeShape<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);
static_assert(MatchesMultiplicativeShape<Matrix2x2V<Rat>, Vec2V<Rat>>);
static_assert(MatchesMultiplicativeShape<Covec2V<Rat>, Matrix2x2V<Rat>>);
static_assert(!MatchesMultiplicativeShape<Vec2V<Rat>, Matrix2x2V<Rat>>,
              "Vec2V × Matrix2x2V has inner 1 ≠ 2 — must be ill-typed.");
static_assert(!MatchesMultiplicativeShape<Matrix2x2V<Rat>, Covec2V<Rat>>,
              "Matrix2x2V × Covec2V has inner 2 ≠ 1 — must be ill-typed.");
static_assert(
    HasConformingMatrixMultiplication<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);
static_assert(HasConformingMatrixMultiplication<Matrix2x2V<Rat>, Vec2V<Rat>>);
static_assert(HasConformingMatrixMultiplication<Covec2V<Rat>, Matrix2x2V<Rat>>);

/** @subsection Transpose_Duality_and_Involution */

// Vec2V / Covec2V carry the 2×1 and 1×2 shapes respectively.
static_assert(Vec2V<Rat>::row_count == 2u && Vec2V<Rat>::column_count == 1u);
static_assert(Covec2V<Rat>::row_count == 1u &&
              Covec2V<Rat>::column_count == 2u);

// Vec2V ↔ Covec2V is a transpose dual pair.
static_assert(IsTransposeDualPair<Vec2V<Rat>, Covec2V<Rat>>);

// Double-transpose is identity at the type level on all three carriers.
static_assert(HasInvolutiveTranspose<Matrix2x2V<Rat>>);
static_assert(HasInvolutiveTranspose<Vec2V<Rat>>);
static_assert(HasInvolutiveTranspose<Covec2V<Rat>>);

// Value-level: (aᵀ)ᵀ = a on column and row vectors.
inline constexpr Vec2V<Rat> tr_vec{Rat{7L}, Rat{-3L}};
inline constexpr Covec2V<Rat> tr_covec{Rat{2L}, Rat{5L}};
static_assert(tr_vec.transpose().transpose() == tr_vec);
static_assert(tr_covec.transpose().transpose() == tr_covec);
static_assert(tr_vec.transpose() == Covec2V<Rat>{Rat{7L}, Rat{-3L}});

/** @subsection O(2,_ℚ) — the multiplicative group of orthogonal matrices.
 *
 *  Concrete rational orthogonal matrices. For each one we check
 *  `MᵀM = M·Mᵀ = I`. Composition of orthogonals is orthogonal (group
 *  closure); transpose is the inverse (group inverse law).
 */

static_assert(IsOrthogonalMatrixCarrier<Matrix2x2V<Rat>>);

// Identity, 90° rotation, reflection across the x-axis.
inline constexpr Matrix2x2V<Rat> orth_I = identity_matrix2x2_v<Rat>;
inline constexpr Matrix2x2V<Rat> orth_R90{Rat{0L}, Rat{-1L}, Rat{1L}, Rat{0L}};
inline constexpr Matrix2x2V<Rat> orth_Rx{Rat{1L}, Rat{0L}, Rat{0L}, Rat{-1L}};

// Orthogonality law Mᵀ·M = M·Mᵀ = I, per concrete matrix.
static_assert(orth_I.transpose() * orth_I == identity_matrix2x2_v<Rat>);
static_assert(orth_I * orth_I.transpose() == identity_matrix2x2_v<Rat>);
static_assert(orth_R90.transpose() * orth_R90 == identity_matrix2x2_v<Rat>);
static_assert(orth_R90 * orth_R90.transpose() == identity_matrix2x2_v<Rat>);
static_assert(orth_Rx.transpose() * orth_Rx == identity_matrix2x2_v<Rat>);
static_assert(orth_Rx * orth_Rx.transpose() == identity_matrix2x2_v<Rat>);

// Group closure: composition of two orthogonals is orthogonal.
inline constexpr Matrix2x2V<Rat> orth_R180 = orth_R90 * orth_R90;
static_assert(orth_R180.transpose() * orth_R180 == identity_matrix2x2_v<Rat>,
              "Group closure in O(2, ℚ).");
static_assert(orth_R180 ==
                  Matrix2x2V<Rat>{Rat{-1L}, Rat{0L}, Rat{0L}, Rat{-1L}},
              "R180 = -I on ℚ².");

// Group inverse law: for orthogonal M, M⁻¹ = Mᵀ.
static_assert(orth_R90 * orth_R90.transpose() == orth_I);

// Matrix–vector linear action on ℚ² via the shape-conforming product.
inline constexpr Vec2V<Rat> col_e1{Rat{1L}, Rat{0L}};
inline constexpr Vec2V<Rat> col_e2{Rat{0L}, Rat{1L}};
static_assert(orth_R90 * col_e1 == col_e2);
static_assert(orth_R90 * col_e2 == -col_e1);

}  // namespace detail

}  // namespace dedekind::linear_algebra
