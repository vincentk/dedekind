/**
 * @file dedekind/linear_algebra/mat2x2.cppm
 * @partition :mat2x2
 * @brief 2×2 matrices — as linear maps between 2-tuples (the worked
 *        first-class instance; n>2 lives behind the
 *        structure-preserving combinators of #368).
 *
 * @details
 * Following Stammbach's "Lineare Algebra", matrices are interpreted as
 * linear maps on the 2-vector carriers from `:vec2`. This partition carries
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
 * @section matrix__Tier0_Existential_Proof
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
 * @section matrix__Module_Action_Alignment
 * `Vec2<T, x, y>` is a structural 2-vector carrying its coordinates as NTTPs.
 * `Invertible2x2` carries a left action on `Vec2` via `operator*(Vec2)`, which
 * is the compile-time analogue of the linear action `GL_2(T) × T² → T²` that
 * underlies `dedekind.algebra:modules` / `:action` conceptually. Because
 * entries live in the type, each matrix-vector product is a type-level
 * rewrite, observable via `static_assert`.
 *
 * @section matrix__DSL_Surface
 *
 *     using Q = dedekind::numbers::Rational<long>;
 *     // M = [[1, 2], [3, 4]],  det = -2
 *     constexpr Invertible2x2<Q, Q{1L}, Q{2L}, Q{3L}, Q{4L}> M{};
 *     static_assert(M * M.inverse() == Identity2x2<Q>{});
 *
 * Wikipedia: Invertible matrix, Cramer's rule, General linear group,
 * Group action
 *
 * @note "The notion of a matrix ... appears to me to be one of
 *  considerable importance: it includes that of an ordinary algebraic
 *  quantity, and also that of a quaternion."
 *       — Arthur Cayley, *A Memoir on the Theory of Matrices*,
 *         Philosophical Transactions of the Royal Society of London
 *         148 (1858), Article 14.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>  // std::invoke for matrix2x2_functor::φ
#include <type_traits>
#include <utility>  // std::declval for non-default-constructible type composition

export module dedekind.linear_algebra:mat2x2;

import dedekind.algebra; // HasRingOperators, HasFieldOperators, HasVectorSpaceOperators
import dedekind.category; // IsFunctor / Set / arrow (for matrix2x2_functor witness)
import dedekind.numbers; // Rational<Z> for the ℚ carrier
import dedekind.sets;    // Finite cardinality tag (for dimension_type)
import :basis;           // is_endomorphism_ring_v trait declaration
import :contracts;       // matrix / vector / orientation concepts
import :vec2;            // Vec2 (NTTP), Vec2V / Covec2V (value-level)

namespace dedekind::linear_algebra {

/** @section matrix__NTTP_Matrix_Family — compile-time 2×2 matrices used for the
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
// Design choice (#393): `HasFieldOperators<T>` is the right
// constraint here, NOT a stopgap.  `inverse_type` divides by `det`,
// and rings in general do not admit division, so the closed-form
// Cramer inverse genuinely needs the field-like scalar surface.
// Over `T = Rational<Z>` with integer `Z` the constraint is tight;
// over IEEE-edge carriers (`IEEE<double>`) the operational variant
// is exactly the right fit, since strict `IsField` would fail under
// rounding-non-associativity.  The Has*Operators shape concepts
// (in :ring / :order:lattice / :category:logic) are for callsites
// that need the operator surface to compile but make no algebraic
// claim --- not the case here.
export template <typename T, T a, T b, T c, T d>
  requires dedekind::algebra::HasFieldOperators<T>
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
// `HasRingOperators<T>` is sufficient: `Matrix2x2` uses +, -, unary -, *, but
// never divides. Tighter than `HasFieldOperators<T>`.
export template <typename T, T a, T b, T c, T d>
  requires dedekind::algebra::HasRingOperators<T>
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
    // `std::declval` lets us compose over factor types that are not default-
    // constructible; prior `A{} * A2{}` silently required default construction.
    using ProdA =
        decltype(std::declval<const A&>() * std::declval<const A2&>());
    using ProdB =
        decltype(std::declval<const B&>() * std::declval<const B2&>());
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
   * chained `operator*` on matrix types. Uses `std::declval` so that the
   * type composition does not silently demand that `A::inverse_type`, `B`,
   * and `D::inverse_type` be default-constructible.
   */
  using inverse_type =
      BlockUpperTriangular<typename A::inverse_type,
                           decltype(-(
                               std::declval<typename A::inverse_type>() *
                               std::declval<const B&>() *
                               std::declval<typename D::inverse_type>())),
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
   * `std::declval` avoids imposing a default-construction requirement on
   * the factor types.
   */
  template <typename A2, typename B2, typename D2>
  constexpr auto operator*(BlockUpperTriangular<A2, B2, D2>) const {
    using NewA = decltype(std::declval<const A&>() * std::declval<const A2&>());
    using NewB = decltype(std::declval<const A&>() * std::declval<const B2&>() +
                          std::declval<const B&>() * std::declval<const D2&>());
    using NewD = decltype(std::declval<const D&>() * std::declval<const D2&>());
    return BlockUpperTriangular<NewA, NewB, NewD>{};
  }

  template <typename A2, typename B2, typename D2>
  constexpr bool operator==(BlockUpperTriangular<A2, B2, D2>) const {
    return std::same_as<A, A2> && std::same_as<B, B2> && std::same_as<D, D2>;
  }
};

/** @section matrix__Value_Level_Matrix_Family — the runtime companion to the
 * NTTP carriers above. Entries are ordinary constexpr fields; equality is value
 * equality. `Matrix2x2V` is the paper-facing target of the ℂ/𝔻 regular
 * representations in `:embeddings`.
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
 * transpose surface. Columns are `Vec2V<T>` (a 2×1 matrix from `:vec2`);
 * rows are `Covec2V<T>` (a 1×2 matrix).
 */
// Bound: `HasRingOperators<T>` for the arithmetic surface (+, -, unary -,
// *; division is not used here — `Invertible2x2`'s Cramer inverse stays
// at `HasFieldOperators<T>`).  Pure syntactic shape, not an algebraic
// claim — see #393.  `std::regular<T>` for the value-semantics surface:
// members use `T x{}` default-init, `column(i)` / `row(i)` return `{}`
// out-of-range, `operator==` is defaulted (needs equality-comparable T),
// and struct copies require copyable T. The two constraints are
// orthogonal and both are load-bearing.
export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct Matrix2x2V {
  using scalar_type = T;
  using column_type = Vec2V<T>;
  using row_type = Covec2V<T>;
  // Both row and column dimensions are finite; the cardinality tag unifies
  // the shape-dimension story with `dedekind.sets:cardinality`.
  using dimension_type = dedekind::sets::Finite;
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

/** @section matrix__Functorial_Hub
 *
 *  `Matrix2x2V<·>` carries a 2×2 structural shape that is functorial in
 *  the element type @c T: an arrow @c f: T→T lifts elementwise to an
 *  arrow @c Matrix2x2V<T>→Matrix2x2V<T>.  The hub type below owns
 *  that lift and witnesses @c dedekind::category::IsFunctor; it
 *  complements @c vec2_functor / @c covec2_functor in @c :vec2,
 *  closing the (1×1, 2×1, 1×2, 2×2) shape family below.
 */
export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct matrix2x2_functor {
  using ArrowKind = dedekind::category::hub_arrow_tag;
  using Σ_cat = dedekind::category::CanonicalSetCCC<T>;
  using Τ_cat = dedekind::category::CanonicalSetCCC<Matrix2x2V<T>>;

  using Domain = Σ_cat;
  using Codomain = Τ_cat;

  template <typename U>
  using Shape = Matrix2x2V<U>;

  template <typename 𝗳>
    requires dedekind::category::IsArrow<std::remove_cvref_t<𝗳>>
  constexpr auto φ(𝗳&& f) const {
    return dedekind::category::arrow(
        [f = std::forward<𝗳>(f)](Matrix2x2V<T> const& m) -> Matrix2x2V<T> {
          return {std::invoke(f, m.m11), std::invoke(f, m.m12),
                  std::invoke(f, m.m21), std::invoke(f, m.m22)};
        });
  }

  constexpr Τ_cat operator()(const Σ_cat&) const noexcept { return {}; }
};

static_assert(dedekind::category::IsFunctor<matrix2x2_functor<int>>,
              "Matrix2x2V<·> is a functor Set<T> → Set<Matrix2x2V<T>>: "
              "lifts a T-arrow to the elementwise Matrix2x2V<T>-arrow.");

/** @section matrix__Scalar_Shape_As_Identity_Functor
 *
 *  A scalar T is the 1×1 corner of the shape family (1×1, 2×1, 1×2, 2×2):
 *  it carries no extra structural shape over T itself.  The identity
 *  functor on @c Set<T> witnesses this — its @c Shape<U> = U, so it sits
 *  at the apex of the functorial hierarchy where the inner and outer
 *  shapes coincide.  Pinned here next to the higher-rank siblings so the
 *  family is visible in one place.
 */
static_assert(
    dedekind::category::IsEndofunctor<dedekind::category::identity_functor<
        dedekind::category::CanonicalSetCCC<int>>>,
    "Scalar shape (1×1): identity_functor<Set<T>> is the trivial "
    "endofunctor on Set<T>; sits at the apex of the matrix-shape "
    "family alongside vec2_functor (2×1), covec2_functor (1×2), "
    "and matrix2x2_functor (2×2).");

/** @section matrix__Monadic_Unit_For_Matrix2x2V
 *
 *  The "scalar → linear map" lift the user expects to see at the
 *  linear-algebra layer is the unit @c η : T → Matrix2x2V<T> sending
 *  @c s ↦ s · I (a scalar matrix; equivalently, the centre of the
 *  matrix algebra).  This is also the canonical ring homomorphism
 *  @c T ↪ Matrix2x2V<T> witnessing the field/ring as a sub-ring of
 *  its own matrix algebra.
 */
}  // namespace dedekind::linear_algebra

namespace dedekind::category {

// PR #508: unit_witness / counit_witness now take a Hub regular type
// rather than a template-template parameter.  The existing
// matrix2x2_functor<T> Hub above carries the @c Shape<U> alias the new
// witness primary expects; reuse it directly.

export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct unit_witness<dedekind::linear_algebra::matrix2x2_functor<T>, T> final {
  constexpr dedekind::linear_algebra::Matrix2x2V<T> operator()(T s) const {
    return {s, T{0}, T{0}, s};
  }
};

/** @brief Counit / extract for @c Matrix2x2V<T>: top-left corner.  Matches
 *  the @c m11 = (1, 1) entry — the canonical projection from the matrix
 *  algebra to the underlying scalar ring.
 */
export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct counit_witness<dedekind::linear_algebra::matrix2x2_functor<T>, T> final {
  constexpr T operator()(
      const dedekind::linear_algebra::Matrix2x2V<T>& m) const {
    return m.m11;
  }
};

}  // namespace dedekind::category

namespace dedekind::linear_algebra {

static_assert(dedekind::category::unit_witness<matrix2x2_functor<int>, int>{}(
                  7) == Matrix2x2V<int>{7, 0, 0, 7},
              "Matrix2x2V η: scalar s ↦ s·I (scalar matrix in the centre of "
              "the matrix algebra; canonical ring embedding T ↪ M_2(T)).");
static_assert(dedekind::category::counit_witness<matrix2x2_functor<int>, int>{}(
                  Matrix2x2V<int>{3, 5, 7, 9}) == 3,
              "Matrix2x2V ε: extract top-left corner (canonical projection "
              "M_2(T) → T).");

// NEW-A trait registry (#498/#499): @c Matrix2x2V<T> is the
// endomorphism ring of @c Vec2V<T>, i.e.\ @c End(Vec2V<T>) @c =
// @c M_2(T).  Textbook reading: matrix multiplication is composition
// of linear endomorphisms; the @c (Hom(V, V), @c ∘) ring is the
// @c End ring of the module @c V.  Gated on @c IsModule<Vec2V<T>, T>
// so the module structure is in place before the End-ring claim is
// recorded.
template <typename T>
  requires dedekind::algebra::IsModule<Vec2V<T>, T>
inline constexpr bool is_endomorphism_ring_v<Matrix2x2V<T>, Vec2V<T>> = true;

// Witness uses @c unsigned @c int — the canonical primitive carrier
// that satisfies strict @c algebra::IsRing under modular arithmetic.
static_assert(
    is_endomorphism_ring_v<Matrix2x2V<unsigned int>, Vec2V<unsigned int>>,
    "Matrix2x2V<T> is End(Vec2V<T>) = M_2(T) (matrix multiplication "
    "= composition of linear endomorphisms of the rank-2 free module).");

// NEW-B compositional witness (#498/#500): the rank-axis lift
// (Vec_2 → M_2) composes cleanly with the carrier-strength choice
// (ℤ → ℚ).  Pinning M_2(ℚ) = End_ℚ(V_2(ℚ)) over the exact-rationals
// carrier @c Rational<default_integer> proves Figure 1's vertical
// (P) and horizontal (Frac, an H step) axes commute at the worked
// instance.  Builds on the post-#527 HSP architecture: the
// quotient-algebra propagation lifts species traits from
// @c default_integer to @c Rational<default_integer> (H-step) and
// componentwise from @c Rational<default_integer> to
// @c Vec2V<Rational<default_integer>> (P-step), so all witnesses
// below fire mechanically without per-carrier opt-in.
namespace _newb_compositional_pin {
using Q = dedekind::numbers::Rational<dedekind::numbers::default_integer>;
using Vec2_Q = dedekind::linear_algebra::Vec2V<Q>;
using Mat2_Q = dedekind::linear_algebra::Matrix2x2V<Q>;
static_assert(dedekind::algebra::is_module_v<Vec2_Q, Q>,
              "Vec2V<ℚ> is a ℚ-module — P step composes with the "
              "ℤ → ℚ Frac (H) step.");
static_assert(is_free_module_v<Vec2_Q, Q, 2>,
              "Vec2V<ℚ> is a free ℚ-module of rank 2.");
static_assert(
    is_endomorphism_ring_v<Mat2_Q, Vec2_Q>,
    "M_2(ℚ) = End_ℚ(V_2(ℚ)) — the rank-axis lift Vec_2 → M_2 commutes "
    "with the carrier-strength step ℤ → ℚ at the worked instance "
    "(Figure 1, two-axis lattice; #498).");
}  // namespace _newb_compositional_pin

/** @section matrix__Bifunctorial_And_Concept_Witnessed_Shapes
 *
 *  Higher matrix shapes do not fit the unary @c IsFunctor mould as
 *  cleanly as the (1×1, 2×1, 1×2, 2×2) family above:
 *
 *  - @c DirectSum<A, B> and @c BlockUpperTriangular<A, B, D> are
 *    naturally bifunctors / trifunctors @c Mat × Mat → Mat:
 *    block-diagonal / block-UT composition is functorial in each
 *    argument separately but not unary.  Witnessing them via the
 *    unary @c IsFunctor concept would require either a partial
 *    application trick (functor for fixed B / fixed D) or an
 *    @c IsBifunctor extension to the category partition;
 *  - Orthogonality, rotation, diagonality etc. live as concept-level
 *    invariants on @c Matrix2x2V<·> (e.g.\ @c IsOrthogonalMatrixCarrier
 *    in @c :contracts), not as separate carrier types.  Until #368
 *    introduces dedicated carriers @c Orthogonal2x2<T>,
 *    @c Rotation2x2<T>, etc., there is no type for a functor hub to
 *    map into;
 *
 *  Both directions are real upgrades but require either bifunctor
 *  concept work or new concrete carrier types from #368.  Tracked
 *  separately rather than folded in here.
 */

/** @section matrix__Shape_Conforming_Linear_Actions
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

/** @section matrix__Concatenation_Builders — tuples into matrices.
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

/** @section matrix__Concept_Witnesses_over_ℚ — the `:contracts` slogan-pack
 * witnessed on the concrete Matrix2x2V / Vec2V / Covec2V triple.
 *
 *  The nine structural claims, shape-conformance, transpose involution /
 *  duality, and the orthogonal group O(2, ℚ) — each pinned by
 *  `static_assert`. If a `:contracts` concept is tightened in a way the
 *  concrete carriers fail, the build breaks here.
 */
namespace detail {

using Rat = dedekind::numbers::Rational<long>;

/** @subsection Upstream_Concept_Witnesses — algebra concepts on ℚ carriers.
 *
 *  Rather than redefine the vector-space / module content, witness that the
 *  value-level tuple and matrix carriers satisfy the canonical operator-shape
 *  predicates living one level up: `HasFieldOperators` (in
 *  `dedekind.algebra:field`), `HasRingOperators` (in `dedekind.algebra:ring`),
 *  and `HasVectorSpaceOperators` (in `dedekind.algebra:vectorspace`).  This
 *  anchors the new tuple / matrix types to the upstream algebraic hierarchy
 *  without redefining its concept surface here.
 */

static_assert(dedekind::algebra::HasFieldOperators<Rat>,
              "Rational<long> is the operational field-like scalar under "
              "the active numeric policy.");
static_assert(dedekind::algebra::HasRingOperators<Rat>,
              "Rational<long> is also operationally ring-like (the subset "
              "of field-like that ignores the / requirement).");

static_assert(dedekind::algebra::HasVectorSpaceOperators<Vec2V<Rat>, Rat>,
              "Vec2V<ℚ> is an operational vector-space-like over ℚ.");
static_assert(dedekind::algebra::HasVectorSpaceOperators<Covec2V<Rat>, Rat>,
              "Covec2V<ℚ> is an operational vector-space-like over ℚ.");
static_assert(dedekind::algebra::HasVectorSpaceOperators<Matrix2x2V<Rat>, Rat>,
              "Matrix2x2V<ℚ> is an operational vector-space-like over ℚ "
              "(matrices over a field are themselves a vector space under "
              "entry-wise + and scalar *).");

// `Matrix2x2V<ℚ>` under its own +, unary -, * is operationally a ring —
// the load-bearing reuse of the new `HasRingOperators` concept. This is the
// precise form of the slogan "matrix over a field is at least a ring".
static_assert(dedekind::algebra::HasRingOperators<Matrix2x2V<Rat>>,
              "Matrix2x2V<ℚ> is operationally a ring (non-commutative in "
              "general — see the AB ≠ BA witness below).");

// Strict literal-operator surface (#393): Matrix2x2V's friend operators
// for +, binary -, unary -, * all return `Matrix2x2V<T>` exactly, so
// `HasRingOperators<Matrix2x2V<T>>` fires whenever T satisfies it.
// This lifts the math-textbook slogan "matrices over a ring are a
// (non-commutative) ring" up to the strict-closure shape concept.
static_assert(dedekind::algebra::HasRingOperators<Matrix2x2V<Rat>>,
              "Matrix2x2V<ℚ> closes strictly under the literal "
              "+,-,unary -,* surface — same_as<Matrix2x2V<ℚ>> for each.");
static_assert(dedekind::algebra::HasRingOperators<Matrix2x2V<unsigned int>>,
              "Matrix2x2V<unsigned int>: the literal ring-operator surface "
              "lifts through to the matrix carrier (entries' wrap closure "
              "lifts elementwise).");

// Dimension-as-cardinality: the tuple / matrix carriers expose a
// `dimension_type` tag drawn from `dedekind.sets:cardinality`. This wires
// the documented slogan "dimension resembles cardinality" into the type
// system.
static_assert(std::same_as<Vec2V<Rat>::dimension_type, dedekind::sets::Finite>,
              "Vec2V's dimension lives in the `Finite` cardinality.");
static_assert(
    std::same_as<Covec2V<Rat>::dimension_type, dedekind::sets::Finite>,
    "Covec2V's dimension lives in the `Finite` cardinality.");
static_assert(
    std::same_as<Matrix2x2V<Rat>::dimension_type, dedekind::sets::Finite>,
    "Matrix2x2V's row/column dimensions live in the `Finite` cardinality.");

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
static_assert((decomp_probe.column(0) | decomp_probe.column(1)) == decomp_probe,
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
