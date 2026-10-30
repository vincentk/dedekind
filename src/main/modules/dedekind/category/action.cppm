/**
 * @file dedekind/category/action.cppm
 * @partition :action
 * @brief The Morphisms of Influence (Actions and Representations).

 * @section action__Structural_Influence
 * While Level 0 (:morphism) defines mapping between species,
 * Level 1 defines the "Action" of a Species S upon a Species M (S ⟳ M).
 * In the Dedekind structuralist view, an Action is the reification of
 * Scalar multiplication, leading naturally to the concepts of Modules,
 * Vector Spaces, and eventually, the Linear Algebra of the Continuum.
 *
 * @section action__Declared_Laws
 * A signature is no law: the semimodule laws of an action are a declared
 * certificate, @c is_linear_action, false where none is declared (#1099).
 *
 * @copyright 2026 The Dedekind Authors
 *
 * @note « Качественные методы позволяют исследовать структуру пространства
 *   через инвариантные множества и их преобразования. »
 *  (Qualitative methods allow the study of the structure of space
 *   through invariant sets and their transformations.)
 *  — V. V. Stepanov (В. В. Степанов), 'Курс дифференциальных уравнений'
 *
 */
module;

#include <algorithm>  // std::ranges::fold_left
#include <concepts>
#include <cstddef>  // std::size_t: the counts that act
#include <functional>
#include <ranges>
#include <type_traits>
#include <utility>

export module dedekind.category:action;

import :total;
import :discrete;  // constant: the witness's copies
import :morphism;  // IsArrow, IsConstantArrow, Cod
import :chain;     // the counting chain: fold on n copies of one value
import :species;   // IsIdempotent: what the semimodule rung leaves alone

namespace dedekind::category {

/**
 * @concept IsAction
 * @brief An Action (S ⟳ M) where S acts on M via its mapping property.
 * @tparam S The scalar/influencer species.
 * @tparam M The acted-on species.
 * @tparam Act External action witness (defaults to `std::multiplies<>`).
 *
 * Supports both textbook forms:
 * 1. callable action object `s(m)`
 * 2. binary action witness `Act{}(s, m)`
 */
export template <typename S, typename M, typename Act = std::multiplies<>>
concept IsAction = requires(S s, M m) {
  requires(
      requires {
        // Categorical action as an arrow object.
        { s(m) } -> std::same_as<M>;
      } ||
      requires {
        // Categorical action via external binary witness.
        { Act{}(s, m) } -> std::same_as<M>;
      });
};

/**
 * @brief The semimodule laws of an action @c Act : S x M -> M, declared:
 *        1 m = m, 0 m = 0, (s + t) m = s m + t m, (s t) m = s (t m) and
 *        s (m + n) = s m + s n.  A certificate, false where none is
 *        declared: a carrier with the right signatures is not thereby a
 *        semimodule.  For scalars of type @c size_t, the integers modulo
 *        2^64, the laws make n m the n-fold sum of m for every n.
 *  @tparam M the module.  @tparam S the scalars.  @tparam AddM @tparam AddS
 *  @tparam MultS the operations.  @tparam Act the action.
 */
export template <typename M, typename S, typename AddM, typename AddS,
                 typename MultS, typename Act>
struct is_linear_action : std::false_type {};
/** @brief Value form of @c is_linear_action. */
export template <typename M, typename S, typename AddM, typename AddS,
                 typename MultS, typename Act>
inline constexpr bool is_linear_action_v =
    is_linear_action<M, S, AddM, AddS, MultS, Act>::value;
namespace detail_action {
/** @brief A carrier whose product stays in it: no integer narrower than
 *  @c int, whose product promotes to a signed @c int that may overflow. */
template <typename S>
concept MultipliesInPlace =
    !std::integral<S> ||
    std::same_as<decltype(std::declval<S>() * std::declval<S>()), S>;
}  // namespace detail_action
/** @brief A commutative semiring over itself, by its own multiplication: the
 *  semimodule laws are the semiring's (Golan, @e Semirings and their
 *  Applications, 1999, ch. 14), among them the unsigned integers, the ring of
 *  integers modulo a power of two.  @tparam S the semiring.  @tparam Add
 *  @tparam Mult its operations. */
template <typename S, typename Add, typename Mult>
  requires IsSemiring<S, Add, Mult> && IsCommutative<S, Mult> &&
           detail_action::MultipliesInPlace<S>
struct is_linear_action<S, S, Add, Add, Mult, Mult> : std::true_type {};
/** @brief The same, acted on by the transparent @c std::multiplies<>.
 *  @tparam S the semiring.  @tparam Add its addition. */
template <typename S, typename Add>
  requires IsSemiring<S, Add, std::multiplies<S>> &&
           IsCommutative<S, std::multiplies<S>> &&
           detail_action::MultipliesInPlace<S>
struct is_linear_action<S, S, Add, Add, std::multiplies<S>, std::multiplies<>>
    : std::true_type {};

/**
 * @concept IsSemimodule
 * @brief A commutative monoid @c M on which the semiring @c S acts linearly.
 * @tparam M The module carrier.
 * @tparam S The scalar carrier.
 * @tparam AddM Additive law on M.
 * @tparam AddS Additive law on S.
 * @tparam MultS Multiplicative law on S.
 * @tparam Act The action S x M -> M.
 * @details The laws are the trait @c is_linear_action_v; @c dedekind.algebra
 *          builds its modules and vector spaces on this shape.
 */
export template <typename M, typename S, typename AddM = std::plus<M>,
                 typename AddS = std::plus<S>,
                 typename MultS = std::multiplies<S>,
                 typename Act = std::multiplies<>>
concept IsSemimodule =
    IsCommutativeMonoid<M, AddM> && IsSemiring<S, AddS, MultS> &&
    IsAction<S, M, Act> && is_linear_action_v<M, S, AddM, AddS, MultS, Act>;

/** @brief @c concat on @c n copies of one value where @c (T, Op) is a
 *  semimodule over the counts: one action, @f$n \cdot x@f$, by the
 *  semimodule laws, @f$1 \cdot x = x@f$ and @f$(s + t) \cdot x = s \cdot x + t
 *  \cdot x@f$, which make @f$n \cdot x@f$ the @c n-fold sum.  The same
 *  signature as the binary method of @c :total and a stronger constraint, so
 *  resolution prefers it; an idempotent operation stays with @c :total's
 *  idempotent rung.  @param r the copies.  @param op the operation. */
export template <std::ranges::input_range R,
                 typename T = std::ranges::range_value_t<R>,
                 typename Op = std::plus<T>>
  requires IsMonoid<T, Op> && IsCopies<R> && IsSemimodule<T, std::size_t, Op> &&
           (!IsIdempotent<T, Op>)
constexpr T concat(R r, Op op = {}) {
  const std::size_t n = copies_count(r);
  if (n == 0) return unit<T>(op);  // no copy to read
  return static_cast<T>(std::multiplies<>{}(n, copies_value(r)));
}
static_assert(
    concat(std::views::repeat(std::size_t{7}, std::size_t{100'000'000})) ==
            700'000'000 &&
        concat(std::views::repeat(std::size_t{7}, std::size_t{0})) == 0,
    "n copies under + over the counts are one multiplication");

/**
 * @struct ZeroAction
 * @brief The 'Absorption' logic.
 * @details Maps any input of species A to the neutral element of species B.
 */
export template <typename A, typename B, typename Op>
struct ZeroAction final {
  constexpr B operator()(const A&) const noexcept { return unit<B>(Op{}); }
};

/** @section action__Module_FreeModule_EndoRing_Trait_Registry
 *
 * The NEW-A trait registry (#498/#499) was previously declared here
 * as opt-in trait variables.  It now lives where the underlying
 * concepts live, in concept-based-default form so that every carrier
 * that structurally satisfies the algebra automatically registers
 * (no per-carrier opt-in required):
 *
 *   - @c is_module_v<M, R>       — @c dedekind.algebra:modules
 *                                  (concept-default = @c IsModule<M, R>).
 *   - @c is_vector_space_v<V, F> — @c dedekind.algebra:vectorspace
 *                                  (concept-default = @c IsVectorSpace<V, F>).
 *   - @c is_free_module_v<M, R, N>     — @c dedekind.linear_algebra:basis
 *     @c is_endomorphism_ring_v<A, V>    (opt-in trait variables;
 *                                        carry structural metadata
 *                                        @c N or the @c A=End(V) identity
 *                                        that no concept derives from
 *                                        the operator surface alone).
 */

/** @section action__Zero Morphism Verification */

/** @section action__Peak Symmetry: Zero vs. Groupoid */

// Proof: The result of zero() belongs to the Identity element.
static_assert(zero<int, int, std::plus<int>>()(99) == 0,
              "Absorption: Z -> Z via + must yield 0.");

// 1. Proof: Zero is an Arrow from int to int (under addition).
using ZeroZ = decltype(zero<int, int, std::plus<int>>());

// Proof: The Zero Morphism (int -> int) is an Arrow,
// even if it maps into an Abelian Groupoid.
static_assert(IsArrow<ZeroZ>, "Zero: A 'Black Hole' arrow must map Z to Z.");

static_assert(IsArrow<ZeroZ>, "Zero: Must be a valid morphism mapping Z to Z.");

// 2. Action Proof: Zero maps everything to 0.
static_assert(zero<int, int, std::plus<int>>()(42) == 0,
              "Absorption: Zero morphism must return the identity element.");

// 3. Action Proof: Zero maps everything to 'true' (under logic AND).
static_assert(zero<int, bool, std::logical_and<bool>>()(42) == true,
              "Absorption: Boolean AND zero must return 'true'.");

/** @section action__Fold
 *  @brief A fold along the walk, @c fold(r, v, h), and its specialisation by
 *  the action law (the Jlt paper's Listing 4).  @c acting_monoid<H> names the
 *  monoid an action @c h acts by on the right, a declared certificate of
 *  @f$h(h(v, a), b) = h(v, a \cdot b)@f$ and @f$h(v, 1) = v@f$, the product
 *  in the walk's order (a left action registers @c Opposite<Op>, the
 *  product of the opposite monoid, from @c :involution); keyed by the
 *  action's type without cv-qualification, as a fold deduces it.
 *  FIXME(#1104): @c IsAction above is
 *  scalar first, @f$S \times M \to M@f$, a left action, and
 *  @c IsMonoidAction a right one: two conventions to reconcile.  On @c n
 *  copies of one value the fold is then @c v acted on by their product, and
 *  @c concat (@c :total) computes the product by squaring: two declared laws,
 *  two overloads, composed by resolution. */
export template <typename H>
struct acting_monoid {};
/** @brief @c H is an action @f$M \times S \to M@f$, on the right, of the
 *  monoid @c (S, Op) that @c acting_monoid<H> names: @c M is a right
 *  @c S-set (Mac Lane and Moerdijk), an @c S-act (Kilp, Knauer and Mikhalev,
 *  Monoids, Acts and Categories).  @tparam H the action.
 *  @tparam M what it acts on.  @tparam S the monoid. */
export template <typename H, typename M, typename S>
concept IsMonoidAction =
    std::invocable<const H&, M, S> &&
    requires(const H& h, M v, S s) {  // called directly: a function object
      { h(std::move(v), std::move(s)) } -> std::convertible_to<M>;
    } && requires { typename acting_monoid<std::remove_cvref_t<H>>::type; } &&
    IsMonoid<S, typename acting_monoid<std::remove_cvref_t<H>>::type>;
/** @brief The fold along the walk: the standard's @c fold_left.
 *  @param r the range.  @param v the start.  @param h the action. */
export template <std::ranges::input_range R, std::semiregular M, typename H>
  requires std::invocable<const H&, M, std::ranges::range_value_t<R>>
constexpr M fold(R r, M v, H h) {
  return std::ranges::fold_left(std::move(r), std::move(v), h);
}
/** @brief The fold of @c n copies of one value under a monoid action:
 *  @c v acted on by their product.  The same signature as the plain fold and
 *  a stronger constraint, so resolution prefers it.  The law makes any fold
 *  @c v acted on by the product in the walk's order; on distinct values that
 *  product costs the same @c n steps, so only copies are specialised.
 *  @param r the copies.  @param v the start.  @param h the
 *  action. */
export template <std::ranges::input_range R, std::semiregular M, typename H>
  requires IsCopies<R> && IsMonoidAction<H, M, std::ranges::range_value_t<R>>
constexpr M fold(R r, M v, H h) {
  using Op = typename acting_monoid<std::remove_cvref_t<H>>::type;
  return h(std::move(v), concat(std::move(r), Op{}));
}
/** @brief A monoid acts on itself: @c (std::size_t, +) by @c +. */
template <>
struct acting_monoid<std::plus<std::size_t>> {
  using type = std::plus<std::size_t>;
};
namespace detail_fold_witness {
constexpr std::size_t ten = 10;
static_assert(fold(ten >> chain<std::size_t>(), std::size_t{0},
                   std::plus<std::size_t>{}) == 45 &&
                  fold(std::size_t{100'000'000} >> chain<std::size_t>() >>
                           constant<std::size_t>(std::size_t{7}),
                       std::size_t{1}, std::plus<std::size_t>{}) == 700'000'001,
              "fold is fold_left; on n copies under a monoid action it is the "
              "start acted on by their product, which concat squares");
}  // namespace detail_fold_witness

}  // namespace dedekind::category
