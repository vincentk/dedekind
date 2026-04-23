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

import dedekind.numbers;  // Rational<Z> for the ℚ carrier

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

}  // namespace dedekind::linear_algebra
