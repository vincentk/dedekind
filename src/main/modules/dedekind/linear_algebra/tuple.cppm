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
 * @note "I have in previous papers defined a 'Matrix' as a rectangular
 *  array of terms, out of which different systems of determinants may
 *  be engendered, as from the womb of a common parent."
 *       — James Joseph Sylvester, *Additions to the Articles 'On a
 *         New Class of Theorems' and 'On Pascal's Theorem'*,
 *         Philosophical Magazine 4th series, vol. 1 (1851), p. 295.
 *
 *       The Sylvester etymology --- @c matrix as @c mater, "womb" ---
 *       lands well at the tuple layer: the rank-1 carriers
 *       @c Vec2V<T> (a column) and @c Covec2V<T> (a row) are the
 *       parents from which @c Matrix2x2V<T> is engendered by
 *       horizontal / vertical concatenation.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.linear_algebra:tuple;

import dedekind.algebra;  // HasRingOperators, IsVectorSpaceLike (upstream)
import dedekind.category; // IsFunctor / Set / arrow (for vec2_functor witnesses)
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
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
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
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
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
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
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
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
constexpr Covec2V<T> Vec2V<T>::transpose() const {
  return {x, y};
}

/** @section Functorial_Hubs
 *
 *  `Vec2V<·>` and `Covec2V<·>` carry a structural shape (2×1 / 1×2 with
 *  element type @c T) that is functorial in @c T: an arrow @c f: T→T
 *  lifts elementwise to an arrow @c Vec2V<T>→Vec2V<T> (resp.\ Covec).
 *  The hub types below own that lift and witness
 *  @c dedekind::category::IsFunctor for each shape.  They mirror
 *  @c dedekind::category::box_functor / @c maybe_functor in pattern.
 */
export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct vec2_functor {
  using ArrowKind = dedekind::category::hub_arrow_tag;
  using Σ_cat = dedekind::category::CanonicalSetCCC<T>;
  using Τ_cat = dedekind::category::CanonicalSetCCC<Vec2V<T>>;

  using Domain = Σ_cat;
  using Codomain = Τ_cat;

  template <typename U>
  using Shape = Vec2V<U>;

  template <typename 𝗳>
    requires dedekind::category::IsArrow<std::remove_cvref_t<𝗳>>
  constexpr auto φ(𝗳&& f) const {
    return dedekind::category::arrow(
        [f = std::forward<𝗳>(f)](Vec2V<T> const& v) -> Vec2V<T> {
          return {std::invoke(f, v.x), std::invoke(f, v.y)};
        });
  }

  constexpr Τ_cat operator()(const Σ_cat&) const noexcept { return {}; }
};

export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct covec2_functor {
  using ArrowKind = dedekind::category::hub_arrow_tag;
  using Σ_cat = dedekind::category::CanonicalSetCCC<T>;
  using Τ_cat = dedekind::category::CanonicalSetCCC<Covec2V<T>>;

  using Domain = Σ_cat;
  using Codomain = Τ_cat;

  template <typename U>
  using Shape = Covec2V<U>;

  template <typename 𝗳>
    requires dedekind::category::IsArrow<std::remove_cvref_t<𝗳>>
  constexpr auto φ(𝗳&& f) const {
    return dedekind::category::arrow(
        [f = std::forward<𝗳>(f)](Covec2V<T> const& v) -> Covec2V<T> {
          return {std::invoke(f, v.x), std::invoke(f, v.y)};
        });
  }

  constexpr Τ_cat operator()(const Σ_cat&) const noexcept { return {}; }
};

static_assert(dedekind::category::IsFunctor<vec2_functor<int>>,
              "Vec2V<·> is a functor Set<T> → Set<Vec2V<T>>: lifts a T-arrow "
              "to the elementwise Vec2V<T>-arrow.");
static_assert(dedekind::category::IsFunctor<covec2_functor<int>>,
              "Covec2V<·> is a functor Set<T> → Set<Covec2V<T>>: lifts a "
              "T-arrow to the elementwise Covec2V<T>-arrow.");

// Vectors do not form a ring: there is no internal product
// `Vec2V × Vec2V → Vec2V`.  Vec2V exposes scalar multiplication
// `T × Vec2V → Vec2V` and `Vec2V × T → Vec2V`, but `a * b` for
// `a, b: Vec2V` is intentionally ill-formed.  The
// `HasRingOperators<Vec2V<unsigned int>>` shape concept correctly
// refuses; the textbook structure carried by Vec2V is an additive
// abelian group + a module / vector space (see
// `algebra::IsVectorSpaceLike`), not a ring.
static_assert(!dedekind::algebra::HasRingOperators<Vec2V<unsigned int>>,
              "Vec2V is not a ring: vectors have no internal product.");
static_assert(!dedekind::algebra::HasRingOperators<Covec2V<unsigned int>>,
              "Covec2V is not a ring: covectors have no internal product.");

}  // namespace dedekind::linear_algebra

/** @section Monadic_Unit_Witnesses
 *
 *  The functor hubs above are the morphism-lifting half of a Kleisli
 *  triple; this section pins the @c η / unit half — the scalar → linear-map
 *  lift that the user expects to see at the linear-algebra layer.  For
 *  @c Vec2V<T> and @c Covec2V<T> the canonical unit is the diagonal
 *  broadcast @c s ↦ {s, s}, which is the element of the kernel of the
 *  difference map @c {x, y} ↦ x − y.  Together with the elementwise
 *  functorial lift in @c vec2_functor / @c covec2_functor, this gives a
 *  Reader-flavoured Kleisli triple ([2] → T  with diagonal bind);
 *  the corresponding @c operator>>= is intentionally deferred to a
 *  follow-up since the project's Reader monad is not yet generalised
 *  beyond @c Path<·>.
 */
namespace dedekind::category {

export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct unit_witness<dedekind::linear_algebra::Vec2V, T> final {
  constexpr dedekind::linear_algebra::Vec2V<T> operator()(T s) const {
    return {s, s};
  }
};

export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct unit_witness<dedekind::linear_algebra::Covec2V, T> final {
  constexpr dedekind::linear_algebra::Covec2V<T> operator()(T s) const {
    return {s, s};
  }
};

/** @brief Counit / extract witness for @c Vec2V<T>.  Canonical projection
 *  to the first coordinate; mirrors the Reader-monad @c π_0 counit.
 */
export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct counit_witness<dedekind::linear_algebra::Vec2V, T> final {
  constexpr T operator()(const dedekind::linear_algebra::Vec2V<T>& v) const {
    return v.x;
  }
};

}  // namespace dedekind::category

namespace dedekind::linear_algebra {

static_assert(dedekind::category::unit_witness<Vec2V, int>{}(7) ==
                  Vec2V<int>{7, 7},
              "Vec2V η: scalar → diagonal broadcast.");
static_assert(dedekind::category::counit_witness<Vec2V, int>{}(Vec2V<int>{
                  3, 5}) == 3,
              "Vec2V ε: extract canonical first coordinate.");
static_assert(dedekind::category::unit_witness<Covec2V, int>{}(2) ==
                  Covec2V<int>{2, 2},
              "Covec2V η: scalar → diagonal broadcast.");

}  // namespace dedekind::linear_algebra
