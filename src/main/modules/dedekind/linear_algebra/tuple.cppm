/**
 * @file dedekind/linear_algebra/tuple.cppm
 * @partition :tuple
 * @brief Level 12.5a₀: Finite tuples — vectors and covectors as indexed
 *        sequences, oriented as columns or rows.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Motivation
 * Following Stammbach's "Lineare Algebra", a tuple is a finite, ordered
 * collection of elements in a common carrier: `(x₁, …, xₙ) ∈ Tⁿ`. Tuples
 * come before matrices in the pedagogical order — matrices will then be
 * interpreted as linear maps between tuple spaces. This partition hosts the
 * tuple-level types:
 *
 *   - `Vec2<T, x, y>`  — NTTP 2-tuple (entries at the type level).
 *   - `Vec2V<T>`       — value-level column 2-tuple (a 2×1 matrix).
 *   - `Covec2V<T>`     — value-level row 2-tuple (a 1×2 matrix).
 *
 * The column/row orientation tags come from `:contracts`; transpose
 * exchanges `Vec2V ↔ Covec2V`, establishing them as a dual pair.
 *
 * @note "Ordnung ist das halbe Leben."
 *       (Order is half of life.)
 *       -- German proverb; in tuple form, order IS the life.
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.linear_algebra:tuple;

import dedekind.algebra; // IsRingLike, IsVectorSpaceLike (upstream)
import dedekind.sets; // Finite tag — the cardinal the tuple dimension lives in
import :contracts;    // ColumnOrientation, RowOrientation tags

namespace dedekind::linear_algebra {

/** @section NTTP_Tuples — entries pinned at the type level. */

/**
 * @brief Structural 2-tuple with coordinates at the type level.
 *
 * `Vec2<T, x, y>` carries the module element `(x, y) ∈ T²` as NTTPs. The
 * type is the element; `Vec2<T, 1, 2>` and `Vec2<T, 3, 4>` are distinct
 * types. Used as the target of the NTTP `Invertible2x2`'s left action in
 * `:matrix`.
 */
export template <typename T, T x, T y>
  requires std::equality_comparable<T>
struct Vec2 {
  using Domain = T;

  static constexpr T first = x;
  static constexpr T second = y;

  template <T x2, T y2>
  constexpr bool operator==(Vec2<T, x2, y2>) const {
    return x == x2 && y == y2;
  }
};

/** @section Value_Level_Tuples — column / row vectors as 2×1 / 1×2 matrices. */

// Forward declaration so `Vec2V::transpose()` can name `Covec2V` at the
// point of definition. The bound matches the full declaration below.
export template <typename T>
  requires dedekind::algebra::IsRingLike<T>
struct Covec2V;

/**
 * @brief Value-level column vector of length 2, viewed as a 2×1 matrix.
 *
 * Carries:
 *   - `scalar_type = T`,
 *   - `orientation = ColumnOrientation`,
 *   - `dimension = 2` (number of coordinate axes),
 *   - `row_count = 2` / `column_count = 1` (matrix shape — a column vector
 *     IS a 2×1 matrix).
 *
 * Arithmetic is component-wise with a left and a right scalar action.
 * `transpose()` yields the dual `Covec2V<T>` (a 1×2 matrix), establishing
 * the `IsTransposeDualPair<Vec2V<T>, Covec2V<T>>` relationship from
 * `:contracts`.
 */
export template <typename T>
  requires dedekind::algebra::IsRingLike<T>
struct Vec2V {
  using scalar_type = T;
  using orientation = ColumnOrientation;
  // Dimension carried both as an integer (convenient for size arithmetic)
  // and as a `Finite` cardinality tag — the linear-algebraic reading of
  // the `dedekind.sets:cardinality` ladder: a tuple's index set is always
  // extensionally finite.
  // FIXME: promote `dimension_type` to a full `Cardinality` value if / when
  // infinite-dimensional tuple carriers land — for now `Finite` is a tag.
  using dimension_type = dedekind::sets::Finite;
  static constexpr std::size_t dimension = 2;
  static constexpr std::size_t row_count = 2;
  static constexpr std::size_t column_count = 1;

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

  /** @brief Transpose to the dual covector: column → row. */
  constexpr Covec2V<T> transpose() const;
};

/**
 * @brief Value-level row vector (covector) of length 2, viewed as a 1×2
 *        matrix.
 *
 * Distinguished from `Vec2V` by the `RowOrientation` tag and matrix shape
 * (1×2 vs 2×1). The distinction lets the type system keep the horizontal
 * and vertical concatenation views of a matrix separate, even though both
 * views share the same underlying scalar layout.
 */
export template <typename T>
  requires dedekind::algebra::IsRingLike<T>
struct Covec2V {
  using scalar_type = T;
  using orientation = RowOrientation;
  using dimension_type = dedekind::sets::Finite;  // cardinality tag
  static constexpr std::size_t dimension = 2;
  static constexpr std::size_t row_count = 1;
  static constexpr std::size_t column_count = 2;

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

  /** @brief Transpose to the dual vector: row → column. */
  constexpr Vec2V<T> transpose() const { return {x, y}; }
};

template <typename T>
  requires dedekind::algebra::IsRingLike<T>
constexpr Covec2V<T> Vec2V<T>::transpose() const {
  return {x, y};
}

}  // namespace dedekind::linear_algebra
