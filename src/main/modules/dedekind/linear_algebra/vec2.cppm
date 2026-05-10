/**
 * @file dedekind/linear_algebra/vec2.cppm
 * @partition :vec2
 * @brief Level 12.5a₀: 2-tuples — Vec2 / Vec2V (column) and Covec2V (row).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section tuple__Motivation
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
 * @section vec2__CT_Framing
 * In CT lingo: @c Vec2V<T> @c = @c T² is a @b homogeneous @b pair —
 * the diagonal image of the binary Cartesian product
 * (@c Δ: @c T @c → @c T @c × @c T from
 * @c dedekind.category:cartesian) where both factors are the same
 * carrier.  This is the textbook @b P (direct product) of HSP
 * (Burris--Sankappanavar §II.10) at @c n = @c 2; the structural-
 * trait propagation in @c dedekind.algebra:quotient lifts the
 * species traits componentwise from @c T to @c Vec2V<T>.  The
 * @b heterogeneous @b tuple — @c (A, @c B) where @c A and @c B
 * are distinct carriers — lives in @c dedekind.category:cartesian
 * (Pierce-land); homogeneous @c n=2 is what this partition reifies.
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

export module dedekind.linear_algebra:vec2;

import dedekind.algebra; // HasRingOperators, HasVectorSpaceOperators (upstream)
import dedekind.category; // IsFunctor / Set / arrow (for vec2_functor witnesses)
import dedekind.order; // IsDirectedSet — algebraic gate on the operator[] index domain (any net domain works)
import dedekind.sets;  // Finite tag — the cardinal the tuple dimension lives in
import :basis;         // is_free_module_v trait declaration
import :contracts;     // ColumnOrientation, RowOrientation tags

namespace dedekind::linear_algebra {

/** @section tuple__NTTP_Tuples — entries pinned at the type level. */

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

/** @section tuple__Value_Level_Tuples Column / row vectors as 2x1 / 1x2
 * matrices. */

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

  /** @brief Indexed projection. Semantics match @c row()/@c column() in
   *         the matrix family: @c i=0 returns @c x, @c i=1 returns @c y,
   *         out-of-range returns @c T{} (the zero element).  Returned by
   *         value to avoid the @c m[i][j]-style dangling-reference trap
   *         when binding to a reference.
   *
   *  Categorical reading (Mac Lane CWM §IV.6): the subscript @c v[i] is
   *  the CCC eval counit @c ε: @c (T^2)^{2} @c × @c 2 @c → @c T applied
   *  to @c (v, @c i).  Equivalently the curried form @c v: @c 2 @c → @c T
   *  in @b Set, with @c v[i] @c = @c eval(v, @c i).  The @c is_eval_arrow_v
   *  trait below pins the categorical reading at the carrier site.
   *
   *  @c Idx is gated by @c dedekind::order::IsDirectedSet — the
   *  algebraic predicate that picks out @b net @b domains (compare
   *  @c sequences:net's @c IsNet, which requires
   *  @c IsDirectedSet on @c N::Domain).  Conversion to
   *  @c std::size_t names the index numerically and admits @c bool,
   *  @c int, @c unsigned, @c std::size_t, @c ℕ, and any future
   *  intensional cardinal that converts.
   */
  template <typename Idx>
    requires dedekind::order::IsDirectedSet<Idx> &&
             std::convertible_to<Idx, std::size_t>
  constexpr T operator[](Idx const& i) const {
    const std::size_t s = static_cast<std::size_t>(i);
    if (s == 0) return x;
    if (s == 1) return y;
    return T{};
  }

  /** @brief Halfspace-gated static-index overload (#372 slice b).
   *
   *  When the index is encoded at the type level via
   *  @c {std::integral_constant<U,I>} for any @c {std::integral U},
   *  the dimension halfspace @c {[0, dimension)} is decided at
   *  compile time: out-of-range indices fail to instantiate (the
   *  @c requires-clause refuses them).
   *
   *  Subsumes both presentations:
   *  @li @c {std::integral_constant<std::size_t,I>} — the size_t face;
   *      @c {v[std::integral_constant<size_t,2>{}]} is a type-check
   *      failure rather than a runtime fallback.
   *  @li @c {std::bool_constant<B>} (= @c {std::integral_constant<bool,B>})
   *      — the 𝔹-indexed face.  @c {Vec2V<T> ≅ 𝔹 → T} as a function
   *      space: @c {v[false_type] = x}, @c {v[true_type] = y}.  Drives
   *      home the slogan "the dimension halfspace at @c n=2 @b is 𝔹".
   */
  template <std::integral U, U I>
    requires(static_cast<std::size_t>(I) < dimension)
  constexpr T operator[](std::integral_constant<U, I>) const {
    if constexpr (I == U{0})
      return x;
    else
      return y;
  }

  /** @brief Named-pair view via @c operator->.
   *
   *  Returns a proxy whose members @c _1 / @c fst alias @c x and @c _2 /
   *  @c snd alias @c y.  Mirrors the textbook left/right projection of
   *  the homogeneous pair @c T² and the Pierce-style first/second tuple
   *  destructors.  Categorical reading: @c v->_1 reads as the comonadic
   *  extract on the canonical first projection (Wadler 1992).
   */
  struct ArrowView {
    T const& _1;
    T const& _2;
    T const& fst;
    T const& snd;
    constexpr ArrowView const* operator->() const { return this; }
  };
  constexpr ArrowView operator->() const { return ArrowView{x, y, x, y}; }

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

  /** @brief Indexed projection.  Same row-style semantics as
   *         @c Vec2V::operator[]: @c i=0 returns @c x, @c i=1 returns
   *         @c y, out-of-range returns @c T{}.  Returned by value.
   */
  template <typename Idx>
    requires dedekind::order::IsDirectedSet<Idx> &&
             std::convertible_to<Idx, std::size_t>
  constexpr T operator[](Idx const& i) const {
    const std::size_t s = static_cast<std::size_t>(i);
    if (s == 0) return x;
    if (s == 1) return y;
    return T{};
  }

  /** @brief Halfspace-gated static-index overload (#372 slice b).
   *         Same integral-parametric contract as
   *         @c Vec2V::operator[](integral_constant): admits any
   *         @c {std::integral_constant<U,I>} with non-negative
   *         @c {I < dimension}.  Subsumes the 𝔹-indexed face
   *         (@c bool_constant<B> = @c {integral_constant<bool,B>}):
   *         @c Covec2V<T> @c ≅ 𝔹 @c → @c T.
   */
  template <std::integral U, U I>
    requires(static_cast<std::size_t>(I) < dimension)
  constexpr T operator[](std::integral_constant<U, I>) const {
    if constexpr (I == U{0})
      return x;
    else
      return y;
  }

  /** @brief Transpose to the dual vector: row → column. */
  constexpr Vec2V<T> transpose() const { return {x, y}; }
};

template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
constexpr Covec2V<T> Vec2V<T>::transpose() const {
  return {x, y};
}

/** @section tuple__Functorial_Hubs
 *
 *  `Vec2V<·>` and `Covec2V<·>` carry a structural shape (2×1 / 1×2 with
 *  element type @c T) that is functorial in @c T: an arrow @c f: T→T
 *  lifts elementwise to an arrow @c Vec2V<T>→Vec2V<T> (resp.\ Covec).
 *  The hub types below own that lift and witness
 *  @c dedekind::category::IsFunctor for each shape.  They mirror
 *  @c dedekind::category::maybe_functor in pattern.
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
// `algebra::HasVectorSpaceOperators`), not a ring.
static_assert(!dedekind::algebra::HasRingOperators<Vec2V<unsigned int>>,
              "Vec2V is not a ring: vectors have no internal product.");
static_assert(!dedekind::algebra::HasRingOperators<Covec2V<unsigned int>>,
              "Covec2V is not a ring: covectors have no internal product.");

}  // namespace dedekind::linear_algebra

/** @section tuple__Monadic_Unit_Witnesses
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

// PR #508: unit_witness / counit_witness now take a Hub regular type
// rather than a template-template parameter.  The existing
// vec2_functor<T> / covec2_functor<T> Hubs (above in this partition)
// already carry the @c Shape<U> alias the new witness primary expects;
// reuse them directly.

export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct unit_witness<dedekind::linear_algebra::vec2_functor<T>, T> final {
  constexpr dedekind::linear_algebra::Vec2V<T> operator()(T s) const {
    return {s, s};
  }
};

export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct unit_witness<dedekind::linear_algebra::covec2_functor<T>, T> final {
  constexpr dedekind::linear_algebra::Covec2V<T> operator()(T s) const {
    return {s, s};
  }
};

/** @brief Counit / extract witness for @c Vec2V<T>.  Canonical projection
 *  to the first coordinate; mirrors the Reader-monad @c π_0 counit.
 */
export template <typename T>
  requires std::regular<T> && dedekind::algebra::HasRingOperators<T>
struct counit_witness<dedekind::linear_algebra::vec2_functor<T>, T> final {
  constexpr T operator()(const dedekind::linear_algebra::Vec2V<T>& v) const {
    return v.x;
  }
};

// Carrier-side construction shape: @c Vec2V<T> = @c T × @c T is a
// @b direct @b product (the @c P operation in Birkhoff's HSP;
// Burris-Sankappanavar §II.10).  Registering
// @c product_algebra_base<Vec2V<T>>::type @c = @c T fires the
// structural-trait propagation in @c algebra:quotient: associativity,
// commutativity, distributivity, and the @c IsTotal certificate
// (periodic / idempotent / saturating) all lift componentwise from
// @c T to @c Vec2V<T>.
template <typename T>
struct product_algebra_base<dedekind::linear_algebra::Vec2V<T>> {
  using type = T;
};

template <typename T>
struct product_algebra_base<dedekind::linear_algebra::Covec2V<T>> {
  using type = T;
};

// Carrier-specific additive identity + inverse on Vec2V / Covec2V.
// These don't propagate trivially because identity_trait::value is a
// constructed value of the carrier, requiring the carrier's ctor.
template <typename T>
struct identity_trait<dedekind::linear_algebra::Vec2V<T>,
                      std::plus<dedekind::linear_algebra::Vec2V<T>>> {
  using value_type = dedekind::linear_algebra::Vec2V<T>;
  static constexpr value_type value = value_type{T{}, T{}};
};

template <typename T>
struct identity_trait<dedekind::linear_algebra::Covec2V<T>,
                      std::plus<dedekind::linear_algebra::Covec2V<T>>> {
  using value_type = dedekind::linear_algebra::Covec2V<T>;
  static constexpr value_type value = value_type{T{}, T{}};
};

template <typename T>
inline constexpr bool
    is_invertible_v<dedekind::linear_algebra::Vec2V<T>,
                    std::plus<dedekind::linear_algebra::Vec2V<T>>> = true;

template <typename T>
inline constexpr bool
    is_invertible_v<dedekind::linear_algebra::Covec2V<T>,
                    std::plus<dedekind::linear_algebra::Covec2V<T>>> = true;

template <typename T>
struct inverse_trait<dedekind::linear_algebra::Vec2V<T>,
                     std::plus<dedekind::linear_algebra::Vec2V<T>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::linear_algebra::Vec2V<T>;
  static constexpr value_type compute(
      const dedekind::linear_algebra::Vec2V<T>& v) noexcept {
    return -v;
  }
};

template <typename T>
struct inverse_trait<dedekind::linear_algebra::Covec2V<T>,
                     std::plus<dedekind::linear_algebra::Covec2V<T>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::linear_algebra::Covec2V<T>;
  static constexpr value_type compute(
      const dedekind::linear_algebra::Covec2V<T>& v) noexcept {
    return -v;
  }
};

}  // namespace dedekind::category

namespace dedekind::linear_algebra {

static_assert(dedekind::category::unit_witness<vec2_functor<int>, int>{}(7) ==
                  Vec2V<int>{7, 7},
              "Vec2V η: scalar → diagonal broadcast.");
static_assert(dedekind::category::counit_witness<vec2_functor<int>, int>{}(
                  Vec2V<int>{3, 5}) == 3,
              "Vec2V ε: extract canonical first coordinate.");
static_assert(dedekind::category::unit_witness<covec2_functor<int>, int>{}(2) ==
                  Covec2V<int>{2, 2},
              "Covec2V η: scalar → diagonal broadcast.");

// NEW-A trait registry (#498/#499): @c Vec2V<T> is a free @c T-module
// of rank 2 (canonically isomorphic to @c T^2 via the (x, y)
// component projection).  Sibling @c Covec2V<T> is the dual rank-2
// module (row vector); both pin against the same trait at rank 2.
//
// The @c is_module_v witness fires automatically via the concept-based
// default in @c dedekind::algebra:modules (composing the
// @c product_algebra_base propagation declared next to the type
// itself); we only opt-in to the rank-bearing @c is_free_module_v
// here (free-module ⟹ module is the algebraic implication, but the
// rank @c N is structural metadata that no concept derives from the
// operator surface).
template <typename T>
  requires dedekind::algebra::IsModule<Vec2V<T>, T>
inline constexpr bool is_free_module_v<Vec2V<T>, T, 2> = true;

template <typename T>
  requires dedekind::algebra::IsModule<Covec2V<T>, T>
inline constexpr bool is_free_module_v<Covec2V<T>, T, 2> = true;

// Witnesses use @c unsigned @c int — the canonical primitive carrier
// that satisfies strict @c algebra::IsRing under modular arithmetic
// (signed @c int fails @c IsRing because of signed-overflow UB).

static_assert(dedekind::algebra::is_module_v<Vec2V<unsigned int>, unsigned int>,
              "Vec2V<T> is a T-module (free-module ⟹ module).");
static_assert(
    dedekind::algebra::is_module_v<Covec2V<unsigned int>, unsigned int>,
    "Covec2V<T> is a T-module (free-module ⟹ module).");
static_assert(is_free_module_v<Vec2V<unsigned int>, unsigned int, 2>,
              "Vec2V<T> is a free T-module of rank 2 (M ≅ T^2 via "
              "componentwise projection).");
static_assert(is_free_module_v<Covec2V<unsigned int>, unsigned int, 2>,
              "Covec2V<T> is a free T-module of rank 2 (the row-vector "
              "dual to Vec2V<T>).");

}  // namespace dedekind::linear_algebra

/** @section vec2__Categorical_Anchors_For_Operators
 *
 *  Pin the @c category:cartesian / @c category:kleisli opt-in markers
 *  (#531) on the homogeneous-pair carriers: @c Vec2V<T> and @c Covec2V<T>
 *  realise the CCC eval counit through @c operator[]; @c Vec2V<T> also
 *  realises comonadic extract @c ε through @c operator-> via the
 *  @c ArrowView proxy (first / second projection of the pair).
 */
namespace dedekind::category {

// Parametric in @c Idx, but constrained to match the carrier's
// @c operator[] requires-clause exactly: any net domain that the
// carrier actually accepts fires the CCC-counit reading.  Constraining
// the partial spec (rather than letting the @c HasSubscriptOperator
// concept gate alone) keeps the raw trait honest — it does not say
// @c true for @c Idx the carrier would refuse to instantiate.
template <typename T, typename Idx>
  requires dedekind::order::IsDirectedSet<Idx> &&
               std::convertible_to<Idx, std::size_t>
inline constexpr bool is_eval_arrow_v<dedekind::linear_algebra::Vec2V<T>, Idx> =
    true;

template <typename T, typename Idx>
  requires dedekind::order::IsDirectedSet<Idx> &&
               std::convertible_to<Idx, std::size_t>
inline constexpr bool
    is_eval_arrow_v<dedekind::linear_algebra::Covec2V<T>, Idx> = true;

// Static-index overloads from #372 slice b accept any
// integral_constant<U, I> with std::integral U — including bool
// (bool_constant<B> ≡ integral_constant<bool, B>). Pin
// is_eval_arrow_v on the integral-parametric form so IsEvalArrow
// fires uniformly across runtime and static-index surfaces.
template <typename T, std::integral U, U I>
inline constexpr bool is_eval_arrow_v<dedekind::linear_algebra::Vec2V<T>,
                                      std::integral_constant<U, I>> = true;
template <typename T, std::integral U, U I>
inline constexpr bool is_eval_arrow_v<dedekind::linear_algebra::Covec2V<T>,
                                      std::integral_constant<U, I>> = true;

template <typename T>
inline constexpr bool is_kleisli_deref_v<dedekind::linear_algebra::Vec2V<T>> =
    true;

}  // namespace dedekind::category

namespace dedekind::linear_algebra {

// Witness the CCC eval-counit reading across multiple algebraic net
// domains: @c bool (the binary directed set), @c unsigned @c int (the
// canonical primitive carrier under modular @c IsRing), and @c int.
// Future intensional @c ℕ slots in here without touching the carrier.
static_assert(dedekind::category::IsEvalArrow<Vec2V<unsigned int>, bool>,
              "Vec2V<T>::operator[] accepts bool as a net-domain index.");
static_assert(
    dedekind::category::IsEvalArrow<Vec2V<unsigned int>, unsigned int>,
    "Vec2V<T>::operator[] is the CCC eval counit (Mac Lane CWM §IV.6); "
    "Idx is gated algebraically by IsDirectedSet.");
static_assert(dedekind::category::IsEvalArrow<Vec2V<unsigned int>, int>,
              "Vec2V<T>::operator[] accepts int as a net-domain index.");
static_assert(
    dedekind::category::IsEvalArrow<Covec2V<unsigned int>, unsigned int>,
    "Covec2V<T>::operator[] is the CCC eval counit (row-vector dual).");
static_assert(
    dedekind::category::IsKleisliDeref<Vec2V<unsigned int>>,
    "Vec2V<T>::operator-> realises comonadic extract on the homogeneous "
    "pair (first / second projection via the ArrowView proxy; "
    "Wadler 1992).");

// Value-level witnesses for the operator surface.
namespace detail_op {
inline constexpr Vec2V<unsigned int> opv{3u, 7u};
static_assert(opv[0u] == 3u, "Vec2V::operator[](0u) returns x.");
static_assert(opv[1u] == 7u, "Vec2V::operator[](1u) returns y.");
static_assert(opv[false] == 3u, "Vec2V::operator[](false) returns x.");
static_assert(opv[true] == 7u, "Vec2V::operator[](true) returns y.");

// Halfspace-gated static-index overload (#372 slice b): valid indices
// pass; out-of-range indices fail the requires-clause.  Index type is
// @c std::integral_constant<size_t, I>; at the call site, the literal
// @c std::integral_constant<size_t, 0>{} encodes I=0 at the type level.
static_assert(opv[std::integral_constant<std::size_t, 0>{}] == 3u,
              "Vec2V::operator[]<0> returns x — static-index overload.");
static_assert(opv[std::integral_constant<std::size_t, 1>{}] == 7u,
              "Vec2V::operator[]<1> returns y — static-index overload.");
// Out-of-range static indices fail to instantiate via the overload's
// `requires (I < dimension)` clause; demonstrating this with a
// `!requires { v[ic<2>{}] }` static_assert hits a clang diagnostic
// quirk (the inner expression's diagnostic escapes the requires
// SFINAE).  The compile-time rejection is a property of the overload
// itself; verifying it negatively at the assert level is left to a
// follow-up that uses a SFINAE-friendly variable-template wrapper.

// 𝔹-indexed static overload: the type-theoretic reading of dim=2.
// Vec2V<T> ≅ 𝔹 → T, with v[false_type] = x and v[true_type] = y.
// No out-of-range case — bool exhausts its inhabitants — so no negative
// requires-witness is needed.
static_assert(opv[std::false_type{}] == 3u,
              "Vec2V[false_type] = x — Vec2V<T> ≅ 𝔹 → T, false ↦ first.");
static_assert(opv[std::true_type{}] == 7u,
              "Vec2V[true_type] = y — Vec2V<T> ≅ 𝔹 → T, true ↦ second.");
static_assert(opv->_1 == 3u, "Vec2V::operator->::_1 is the first projection.");
static_assert(opv->_2 == 7u, "Vec2V::operator->::_2 is the second projection.");
static_assert(opv->fst == 3u, "Vec2V::operator->::fst aliases _1.");
static_assert(opv->snd == 7u, "Vec2V::operator->::snd aliases _2.");

inline constexpr Covec2V<unsigned int> opcv{4u, 9u};
static_assert(opcv[0u] == 4u, "Covec2V::operator[](0u) returns x.");
static_assert(opcv[1u] == 9u, "Covec2V::operator[](1u) returns y.");

static_assert(opcv[std::integral_constant<std::size_t, 0>{}] == 4u,
              "Covec2V::operator[]<0> returns x — static-index overload.");
static_assert(opcv[std::integral_constant<std::size_t, 1>{}] == 9u,
              "Covec2V::operator[]<1> returns y — static-index overload.");
static_assert(opcv[std::false_type{}] == 4u,
              "Covec2V[false_type] = x — Covec2V<T> ≅ 𝔹 → T.");
static_assert(opcv[std::true_type{}] == 9u,
              "Covec2V[true_type] = y — Covec2V<T> ≅ 𝔹 → T.");
}  // namespace detail_op

}  // namespace dedekind::linear_algebra
