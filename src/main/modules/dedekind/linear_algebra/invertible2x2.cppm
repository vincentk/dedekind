/**
 * @file dedekind/linear_algebra/invertible2x2.cppm
 * @partition :invertible2x2
 * @brief Level 12.5a: Compile-time full-rank 2×2 matrices over a structural
 *        carrier, with a closed-form Cramer's-rule inverse at the type level
 *        and a left action on an NTTP-carried 2-vector.
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
#include <type_traits>

export module dedekind.linear_algebra:invertible2x2;

import dedekind.numbers; // Rational<Z> for the ℚ carrier

namespace dedekind::linear_algebra {

/**
 * @brief Structural 2-vector with coordinates at the type level.
 *
 * `Vec2<T, x, y>` carries the module element `(x, y) ∈ T²` as NTTPs. The
 * type is the element; `Vec2<T, 1, 2>` and `Vec2<T, 3, 4>` are distinct
 * types. Used here as the target of `Invertible2x2`'s left action.
 */
export template <typename T, T x, T y>
struct Vec2 {
  using Domain = T;

  static constexpr T first = x;
  static constexpr T second = y;

  template <T x2, T y2>
  constexpr bool operator==(Vec2<T, x2, y2>) const {
    return x == x2 && y == y2;
  }
};

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

  constexpr auto operator-() const {
    return Matrix2x2<T, -a, -b, -c, -d>{};
  }

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

/** @brief The 2×2 zero matrix — the additive identity in the ring of matrices. */
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
  using inverse_type = BlockUpperTriangular<
      typename A::inverse_type,
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

}  // namespace dedekind::linear_algebra
