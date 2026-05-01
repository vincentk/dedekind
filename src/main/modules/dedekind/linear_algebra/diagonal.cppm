/**
 * @file dedekind/linear_algebra/diagonal.cppm
 * @partition :diagonal
 * @brief Intensional diagonal and rank-1 outer-product matrix carriers
 *        (#372 slice c + e) — work at any dimension, including @c ℵ_0.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section diagonal__Motivation
 *
 * Two intensional matrix carriers that bypass dense storage and therefore
 * extend uniformly to infinite dimension @c ℵ_0:
 *
 *  - @c Diagonal<D, @c F> — a diagonal matrix @c M: @c D@c ×@c D @c →
 *    @c T storing the rule @c F: @c ℕ @c → @c T with @c M[i][j] @c =
 *    @c δ_ij @c · @c F(i).  The canonical instance @c Identity<D, @c T>
 *    fixes @c F to the constant @c i @c ↦ @c T{1}.
 *
 *  - @c OuterProduct<U, @c V> — the rank-1 matrix @c M @c = @c u@c ⊗@c v
 *    with @c M[i][j] @c = @c u(i)@c ·@c v(j), where @c u and @c v are
 *    @c IsArrow factors with @c size_t domain and a common scalar
 *    codomain.  Either factor may be infinite-dimensional (e.g.
 *    @c sequences::Path<T> over @c ℵ_0); the outer product never
 *    materialises a dense @c m@c × @c n storage.
 *
 * Together they discharge the named trigger of #372 ("first
 * infinite-dim tuple/matrix carrier lands") via two complementary
 * intensional shapes: the diagonal subalgebra of @c End(ℓ²), and the
 * rank-1 image of @c ℓ² @c ⊗ @c ℓ² in the dense matrix algebra.
 *
 * @section diagonal__Type_Level_Dimension_Witnesses
 *
 *  - @c dim_finite<N> (defined here) — finite dimension @c N as an NTTP,
 *    @c is_finite @c = @c true.  Mirrors @c std::integral_constant but
 *    with the @c is_finite / @c is_countable discriminators the
 *    cardinality ladder uses.
 *  - @c dedekind::sets::ℵ<N> (existing) — @c is_finite @c = @c false,
 *    @c is_countable @c = @c (N==0).  Use @c ℵ_0 (= @c ℵ<0>) for
 *    countable infinity.
 *
 * The @c IsDimension concept selects either via the
 * @c is_finite / @c is_countable surface, without committing to a
 * @c std::variant<> NTTP (which the @c numbers::natural precedent
 * (#402) navigated for runtime values, but type-template parameters
 * keep the carrier-side dispatch simpler at the type level).
 *
 * @note "Sumus quod sumus."  ("We are what we are.")
 *       — anonymous Latin proverb, applied here to the rule-based
 *       carriers: a diagonal matrix @b is its diagonal rule, and a
 *       rank-1 matrix @b is the pair of its factor vectors — no
 *       further dense representation is required.
 */
module;

#include <concepts>
#include <cstddef>
#include <type_traits>

export module dedekind.linear_algebra:diagonal;

import dedekind.algebra;    // (multiplication on T for the product rule)
import dedekind.category;   // IsArrow
import dedekind.order;      // IsDirectedSet — algebraic gate on the index
import dedekind.sets;       // ℵ<N>, ℵ_0 — type-level infinite cardinals

namespace dedekind::linear_algebra {

/** @section diagonal__Dimension_Witnesses */

/**
 * @brief Type-level witness for a finite dimension @c N.  Sibling of
 *        @c dedekind::sets::ℵ<N> on the infinite side; together they
 *        cover the dimension axis used by @c Diagonal / @c OuterProduct
 *        without forcing the @c std::variant<> @c Cardinality value to
 *        appear as an NTTP.
 */
export template <std::size_t N>
struct dim_finite {
  static constexpr std::size_t value = N;
  static constexpr bool is_finite = true;
  static constexpr bool is_countable = true;
  constexpr friend bool operator==(const dim_finite&,
                                   const dim_finite&) = default;
};

/** @concept IsDimension
 *  @brief A type-template witness for a dimension cardinality.  Today's
 *         inhabitants: @c dim_finite<N> and @c dedekind::sets::ℵ<N>.
 */
export template <typename D>
concept IsDimension = requires {
  { D::is_finite } -> std::convertible_to<bool>;
  { D::is_countable } -> std::convertible_to<bool>;
};

static_assert(IsDimension<dim_finite<2>>,
              "dim_finite<N> is a bona fide dimension witness.");
static_assert(IsDimension<dedekind::sets::ℵ_0>,
              "ℵ_0 is the canonical infinite dimension witness.");

/** @section diagonal__Diagonal_Carrier */

/**
 * @brief Diagonal matrix carrier @c M: @c D@c ×@c D @c → @c T parametric
 *        in dimension @c D and diagonal rule @c F.
 *
 * @tparam D  Dimension witness (@c dim_finite<N> or @c ℵ<N>).
 * @tparam F  Diagonal rule, an @c IsArrow with @c Domain @c = @c size_t
 *            and scalar @c Codomain.
 *
 * @details Stored intensionally — the rule @c F is the carrier; the
 *          dense @c D@c ×@c D entry table is never materialised.  Works
 *          uniformly at any @c D, including @c ℵ_0.  Off-diagonal entries
 *          are @c T{} (the additive zero), per the @c δ_ij convention.
 */
export template <typename D, typename F>
  requires IsDimension<D> && dedekind::category::IsArrow<F>
struct Diagonal {
  using scalar_type = typename std::remove_cvref_t<F>::Codomain;
  using dimension_type = D;
  static constexpr bool is_finite = D::is_finite;

  F rule{};

  /** @brief Diagonal entry at @c row @c = @c col @c = @c i. */
  template <typename Idx>
    requires dedekind::order::IsDirectedSet<Idx> &&
             std::convertible_to<Idx, std::size_t>
  constexpr scalar_type at(Idx const& i) const {
    return rule(static_cast<std::size_t>(i));
  }

  /** @brief Matrix entry @c (i,j) @c = @c δ_ij @c · @c F(i).  Returns
   *         @c T{} off-diagonal. */
  template <typename I, typename J>
    requires dedekind::order::IsDirectedSet<I> &&
             dedekind::order::IsDirectedSet<J> &&
             std::convertible_to<I, std::size_t> &&
             std::convertible_to<J, std::size_t>
  constexpr scalar_type operator()(I const& i, J const& j) const {
    const std::size_t si = static_cast<std::size_t>(i);
    const std::size_t sj = static_cast<std::size_t>(j);
    return si == sj ? rule(si) : scalar_type{};
  }
};

/** @brief Identity rule: @c i @c ↦ @c T{1}.  IsArrow witness for the
 *         constant unit. */
export template <typename T>
struct identity_rule {
  using Domain = std::size_t;
  using Codomain = T;
  constexpr T operator()(std::size_t) const { return T{1}; }
};

/** @brief Zero rule: @c i @c ↦ @c T{0}. */
export template <typename T>
struct zero_rule {
  using Domain = std::size_t;
  using Codomain = T;
  constexpr T operator()(std::size_t) const { return T{0}; }
};

static_assert(dedekind::category::IsArrow<identity_rule<int>>,
              "identity_rule<T> is an IsArrow witness.");
static_assert(dedekind::category::IsArrow<zero_rule<int>>,
              "zero_rule<T> is an IsArrow witness.");

/** @brief Identity matrix as @c Diagonal<D, @c identity_rule<T>>. */
export template <typename D, typename T>
using Identity = Diagonal<D, identity_rule<T>>;

/** @brief Zero diagonal as @c Diagonal<D, @c zero_rule<T>>. */
export template <typename D, typename T>
using DiagonalZero = Diagonal<D, zero_rule<T>>;

/** @section diagonal__Componentwise_Product
 *
 *  @c Diagonal<D, @c F> @c · @c Diagonal<D, @c G> @c = @c Diagonal<D,
 *  @c F@c ·@c G> componentwise.  The diagonal subalgebra is closed
 *  under multiplication (and commutative when @c T is).
 */

/** @brief The componentwise rule @c (F@c ·@c G)(i) @c = @c F(i) @c ·
 *         @c G(i). */
export template <typename F, typename G>
  requires dedekind::category::IsArrow<F> && dedekind::category::IsArrow<G>
struct diagonal_product_rule {
  using Codomain = typename std::remove_cvref_t<F>::Codomain;
  using Domain = std::size_t;
  static_assert(std::same_as<Codomain, typename std::remove_cvref_t<G>::Codomain>,
                "diagonal_product_rule: F and G must share the scalar codomain.");

  F f{};
  G g{};

  constexpr Codomain operator()(std::size_t i) const { return f(i) * g(i); }
};

/** @brief Diagonal-times-diagonal: returns the componentwise-product
 *         diagonal at the same dimension @c D. */
export template <typename D, typename F, typename G>
  requires IsDimension<D> && dedekind::category::IsArrow<F> &&
           dedekind::category::IsArrow<G>
constexpr auto operator*(Diagonal<D, F> const& a, Diagonal<D, G> const& b) {
  return Diagonal<D, diagonal_product_rule<F, G>>{
      diagonal_product_rule<F, G>{a.rule, b.rule}};
}

/** @section diagonal__OuterProduct_Carrier */

/**
 * @brief Rank-1 matrix @c M @c = @c u@c ⊗@c v with @c M[i][j] @c =
 *        @c u(i)@c ·@c v(j).
 *
 * @tparam U  Left factor (@c IsArrow with @c size_t domain).
 * @tparam V  Right factor (@c IsArrow with @c size_t domain, same
 *            scalar codomain as @c U).
 *
 * @details Stored intensionally — neither factor needs to be
 *          dense-materialised.  Either or both factors may be
 *          infinite-dimensional intensional sequences (e.g.
 *          @c dedekind::sequences::Path<T>); the outer product itself
 *          carries no dense @c m@c ×@c n storage.
 */
export template <typename U, typename V>
  requires dedekind::category::IsArrow<U> && dedekind::category::IsArrow<V>
struct OuterProduct {
  using scalar_type = typename std::remove_cvref_t<U>::Codomain;
  static_assert(
      std::same_as<scalar_type, typename std::remove_cvref_t<V>::Codomain>,
      "OuterProduct: U and V must share the scalar codomain.");

  U u{};
  V v{};

  /** @brief Entry @c (i, @c j) @c = @c u(i) @c · @c v(j). */
  template <typename I, typename J>
    requires dedekind::order::IsDirectedSet<I> &&
             dedekind::order::IsDirectedSet<J> &&
             std::convertible_to<I, std::size_t> &&
             std::convertible_to<J, std::size_t>
  constexpr scalar_type operator()(I const& i, J const& j) const {
    return u(static_cast<std::size_t>(i)) * v(static_cast<std::size_t>(j));
  }
};

/** @section diagonal__Static_Asserts — value-level proofs at finite and
 *  @c ℵ_0 dimensions.
 */
namespace detail_diag {

// (1) Identity * Identity = Identity, witnessed pointwise at concrete
//     indices.  Holds at any dimension D since the rule i ↦ 1 has
//     1·1 = 1 componentwise.

inline constexpr Identity<dim_finite<2>, int> id2{};
static_assert((id2 * id2).at(0) == 1,
              "(Identity<dim_finite<2>, int>{} * Identity{}).at(0) = 1·1 = 1.");
static_assert((id2 * id2).at(1) == 1,
              "(Identity<dim_finite<2>, int>{} * Identity{}).at(1) = 1·1 = 1.");

inline constexpr Identity<dedekind::sets::ℵ_0, int> id_inf{};
static_assert(
    (id_inf * id_inf).at(0) == 1,
    "(Identity<ℵ_0, int>{} * Identity{}).at(0) = 1·1 = 1 — at infinite dim.");
static_assert(
    (id_inf * id_inf).at(42) == 1,
    "(Identity<ℵ_0, int>{} * Identity{}).at(42) = 1·1 = 1 — at infinite dim, "
    "deep into the diagonal.");
static_assert(
    (id_inf * id_inf).at(static_cast<std::size_t>(1) << 20) == 1,
    "(Identity<ℵ_0, int>{} * Identity{}).at(2^20) = 1·1 = 1 — the carrier "
    "is intensional, no dense storage materialised.");

// (2) Off-diagonal entries are zero.
static_assert(id2(0, 1) == 0, "Identity off-diagonal: I(0, 1) = 0.");
static_assert(id_inf(7, 9) == 0,
              "Identity off-diagonal at infinite dim: I_∞(7, 9) = 0.");

// (3) Componentwise diagonal product, with a non-trivial rule on the
//     left and identity on the right: D(f) * I = D(f).
struct linear_rule_int {
  using Domain = std::size_t;
  using Codomain = int;
  constexpr int operator()(std::size_t i) const { return static_cast<int>(i); }
};
static_assert(dedekind::category::IsArrow<linear_rule_int>);

inline constexpr Diagonal<dedekind::sets::ℵ_0, linear_rule_int> diag_lin{};
static_assert((diag_lin * id_inf).at(0) == 0,
              "Diagonal(i ↦ i) at i=0 multiplied by Identity = 0·1 = 0.");
static_assert((diag_lin * id_inf).at(7) == 7,
              "Diagonal(i ↦ i) at i=7 multiplied by Identity = 7·1 = 7.");
static_assert((diag_lin * diag_lin).at(5) == 25,
              "(Diagonal(i ↦ i) * Diagonal(i ↦ i))(5) = 5·5 = 25 — "
              "componentwise diagonal product at concrete index, "
              "infinite-dim carrier.");

// (4) OuterProduct: rank-1 entry law u ⊗ v at finite and infinite axes.
struct const_rule_three {
  using Domain = std::size_t;
  using Codomain = int;
  constexpr int operator()(std::size_t) const { return 3; }
};
static_assert(dedekind::category::IsArrow<const_rule_three>);

inline constexpr OuterProduct<linear_rule_int, const_rule_three> rk1{};
static_assert(rk1(0, 0) == 0,
              "(λi.i ⊗ λj.3)(0, 0) = 0·3 = 0.");
static_assert(rk1(2, 5) == 6,
              "(λi.i ⊗ λj.3)(2, 5) = 2·3 = 6.");
static_assert(rk1(100, 100) == 300,
              "(λi.i ⊗ λj.3)(100, 100) = 100·3 = 300 — the OuterProduct "
              "carrier evaluates intensionally at any (i, j).");
// (5) Bilinearity in the second factor: u ⊗ (s · v) at the entry level.
//     Implemented here by composing with a literal multiplier rule.
struct const_rule_six {  // = 2 · const_rule_three
  using Domain = std::size_t;
  using Codomain = int;
  constexpr int operator()(std::size_t) const { return 6; }
};
inline constexpr OuterProduct<linear_rule_int, const_rule_six> rk1_scaled{};
static_assert(rk1_scaled(2, 5) == 12,
              "(λi.i ⊗ λj.6)(2, 5) = 2·6 = 12 — bilinearity witness: "
              "scaling the right factor by 2 doubles the entry.");

}  // namespace detail_diag

}  // namespace dedekind::linear_algebra
