/**
 * @file dedekind/category/involution.cppm
 * @partition :involution
 * @brief Involution --- a direction-reversal that is its own inverse --- the
 *        atomic shape behind the dagger of a @f$\dagger@f$-category.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section involution__The_Notion
 * An @b involution is an endomap @f$f : T \to T@f$ that undoes itself:
 * @f$f \circ f = \mathrm{id}_T@f$.  It is the mildest form of "reversibility":
 * weaker than a group inverse (no second operand), and the shape shared by
 * complement (@c !!x = x), negation (@c --x = x), transpose
 * (@f$(A^{\top})^{\top} = A@f$), conjugation, and the @b converse of a relation
 * (@f$R^{\smile\smile} = R@f$).
 *
 * @section involution__What_Lives_Here
 * This partition grew from an atomic seed into the @f$\dagger@f$-category
 * surface.  It is the extraction target flagged by @c :lattice's
 * @c Involutive_Endofunctor (an involution at the @b functor level,
 * @f$F^2 \cong \mathrm{Id}@f$) and the home for a notion the library keeps
 * re-discovering under different names --- in each the dagger @f$\dagger@f$
 * coincides with the inverse exactly on the "good" arrows:
 *
 *  @li @b Rel: dagger = converse @f$R^{\smile}@f$; @f$R^{\smile}=R^{-1}@f$ iff
 *      @c R is a @b bijection.
 *  @li real inner-product spaces: dagger = transpose @f$Q^{\top}@f$;
 *      @f$Q^{\top}=Q^{-1}@f$ iff @c Q is @b orthogonal.
 *  @li complex Hilbert spaces: dagger = conjugate-transpose @f$Q^{*}@f$;
 *      @f$Q^{*}=Q^{-1}@f$ iff @c Q is @b unitary.
 *  @li a Boolean subobject lattice: dagger = complement @f$\neg@f$ (self-dual).
 *
 * Below the dagger, the involution on operations: @c Opposite<Op>, the
 * product of the opposite monoid, @f$a \cdot^{op} b = b \cdot a@f$.  A dagger
 * is an involutive map into it, @f$(f\,g)^\dagger = g^\dagger f^\dagger@f$.
 *
 * The unifying statement --- an arrow in a @f$\dagger@f$-category whose dagger
 * @b is its inverse (a @b unitary / dagger-iso) --- makes "the converse is the
 * inverse" (bijective relation) and "the transpose is the inverse" (orthogonal
 * matrix) @b one theorem.  That surface is now @b built here: @c IsInvolution
 * (the atom), @c IsDagger, the computed @c is_isometry / @c is_coisometry /
 * @c is_unitary predicates, and the @c IsIsometry / @c IsUnitary concepts ---
 * with the linear-algebra consumer (@c TransposeF over @c Mat(S)) as the first
 * witness.  Still a @b follow-up (#787): the @b default @c inverse from the
 * dagger (so an orthogonal matrix / bijective relation gets @c inverse for
 * free) and the migration of @c Involutive_Endofunctor, which need the
 * @c :morphism arrow surface.
 *
 * @build_order imports @c :species (@c unit_v); upstream of @c :lattice /
 *              @c :category.
 */
module;

#include <algorithm>   // std::ranges::reverse: the involution on strings
#include <compare>     // std::strong_ordering, which Reverse also reverses
#include <concepts>    // std::invocable, std::convertible_to
#include <cstddef>     // std::size_t, the carrier of the negate witness
#include <functional>  // std::logical_not / std::bit_not (canonical involutions)
#include <iterator>    // std::permutable: a word Reverse may reorder
#include <ranges>      // the words Reverse reverses
#include <string>      // the carrier of Reverse
#include <type_traits>
#include <utility>  // std::declval: the bare callable read as endo<T>(f)  // std::invoke_result_t, std::is_integral_v
#include <vector>   // a word of letters, the other carrier of Reverse

export module dedekind.category:involution;

import :morphism;  // IsArrow / Dom / Cod / Morphism: the arrow language;
                   // Identity
import :species;   // unit_v: the Op-identity the (co)isometry law is
                   // measured against

namespace dedekind::category {

/** @brief Trait: a callable @c F is involutive on @c T iff @c F(F(x)) = x for
 *         all @c x ∈ @c T.  Primary is @c std::false_type; opt-in via
 *         specialisation or member discovery (mirrors @c :species's
 *         @c is_reflexive / @c is_transitive export pattern).  It is the
 *         CERTIFICATE the shape cannot supply: @c F² = @c Id is a claim about
 *         @c F, not a syntactic property of its call. */
export template <typename F, typename T>
struct is_involutive : std::false_type {};

/** @brief Discovery: a type may opt in via a nested @c is_involutive_v template
 *         member. */
template <typename F, typename T>
  requires requires { F::template is_involutive_v<T>; }
struct is_involutive<F, T>
    : std::bool_constant<F::template is_involutive_v<T>> {};

export template <typename F, typename T>
inline constexpr bool is_involutive_v = is_involutive<F, T>::value;

/** @brief Canonical: @c std::logical_not<bool> is the involution on @c bool. */
template <>
struct is_involutive<std::logical_not<bool>, bool> : std::true_type {};

/** @brief Canonical: @c std::bit_not<T> is the involution on @b non-bool
 *  integral @c T (@c ~~x = x).  @c bool is EXCLUDED: @c ~x promotes to @c int
 *  (@c −1/−2) then converts back to @c true, so @c bit_not is not an involution
 *  on @c bool --- its involution is @c std::logical_not. */
template <typename T>
  requires std::is_integral_v<T> && (!std::is_same_v<T, bool>)
struct is_involutive<std::bit_not<T>, T> : std::true_type {};

/** @brief Canonical: unary minus is an involution on the @b unsigned integral
 *  carriers, where @c -x is @c 2^n - x and @c -(-x) = x for every @c x.  The
 *  signed carriers are EXCLUDED: @c -INT_MIN is undefined behaviour, so
 *  @c std::negate<int> is an involution on all values but one, and that one
 *  makes it not a function.  (The Jlt paper's Listing 1 states both.) */
template <std::unsigned_integral T>
struct is_involutive<std::negate<T>, T> : std::true_type {};

/** @brief An involution on a carrier that is not a number: reversing a word
 *  twice is the identity, on any word a value holds, a @c std::string or a
 *  @c std::vector of letters.  A pure function object, value in and value
 *  out, so that it is an arrow in the sense of the Jlt paper's Rule 1, and a
 *  family of them, one per word type, as @c std::logical_not is one per
 *  truth type; @c endo<W>(reverse) names the domain. */
export struct Reverse {
  template <std::ranges::bidirectional_range W>
    requires std::semiregular<W> && std::permutable<std::ranges::iterator_t<W>>
  constexpr W operator()(W s) const {
    std::ranges::reverse(s);
    return s;
  }
  /** @brief A three-way comparison reversed, @c less ↔ @c greater and
   *  @c equal fixed: the comparison in the opposite order, for carriers that
   *  compute an ordering one way round and flip it (the reals' cut and field
   *  comparisons), written once. */
  constexpr std::strong_ordering operator()(std::strong_ordering o) const {
    if (o == std::strong_ordering::less) return std::strong_ordering::greater;
    if (o == std::strong_ordering::greater) return std::strong_ordering::less;
    return std::strong_ordering::equal;
  }
  constexpr bool operator==(const Reverse&) const = default;
};
template <std::ranges::bidirectional_range W>
  requires std::semiregular<W> && std::permutable<std::ranges::iterator_t<W>>
struct is_involutive<Reverse, W> : std::true_type {};
template <>
struct is_involutive<Reverse, std::strong_ordering> : std::true_type {};
/** @brief The reversal: of a word's positions, @f$i \mapsto n - 1 - i@f$, the
 *  symmetry of a palindrome, and of a comparison. */
export inline constexpr Reverse reverse{};

/** @brief The identity arrow is (trivially) an involution: @c id(id(x)) = x,
 *  so @c id⁻¹ = id.  @c Identity is the degenerate case where @b idempotent
 *  (@c f²=f), @b involution (@c f²=id) and @b isomorphism coincide --- the
 *  unique arrow with @c f²=f=id.  Witnessed here so the compiler / @c cata can
 *  SEE it: the composition unit law (@c id∘f=f) subsumes it in practice, but a
 *  downstream inverse-cancellation rule (@c f∘f⁻¹→id, of which @c neg∘neg→id is
 *  the non-trivial case) needs @c id certified as the trivial member. */
template <typename T>
struct is_involutive<Identity<T>, T> : std::true_type {};

/**
 * @concept IsInvolution
 * @brief An endomap @f$f : T \to T@f$ that is CERTIFIED its own inverse:
 *        @f$f \circ f = \mathrm{id}_T@f$.
 *
 * @details The atomic seed of the @f$\dagger@f$ notion.  It is NOT shape-only:
 * beyond the endomap shape (@c f invocable @c T @c → @c T) it requires the
 * @c is_involutive_v certificate, so a merely-composable map like
 * @c [](int x){return x+1;} is @b rejected --- consumers can rely on
 * @f$f(f(x)) = x@f$.  @c :lattice's @c IsInvolutiveEndofunctor is this same
 * predicate under the lattice-complement reading.
 */
namespace detail_involution {
/** @brief @c F read as an arrow on the object @c T: itself when it is one; a
 *  bare callable (@c std::logical_not, a logic's @c RFL) as the endomorphism
 *  @c endo<T>(f) (@c :morphism), the value-type default where the object is
 *  one type and the callable's shape is its arrow. */
template <typename F, typename T>
using on_t =
    std::conditional_t<IsArrow<F>, F, decltype(endo<T>(std::declval<F>()))>;
}  // namespace detail_involution

/** @brief Stated in arrow language: @c F is an endomorphism on the object @c T
 *  (@c IsEndomorphism, domain and codomain @c T --- applying it twice returns
 *  to the domain) carrying the certificate @f$f \circ f = \mathrm{id}_T@f$.  @c
 * T defaults to the arrow's domain; a bare callable names it.  The object may
 * be presented by many types (@c morphism__Objects_And_Their_Presentations), so
 *  the complement on @f$\mathrm{Sub}(T)@f$ is an involution here, through
 *  this one concept. */
export template <typename F, typename T = Dom<F>>
concept IsInvolution = IsEndomorphism<detail_involution::on_t<F, T>> &&
                       std::same_as<Dom<detail_involution::on_t<F, T>>, T> &&
                       is_involutive_v<std::remove_cvref_t<F>, T>;

/** @brief Trait: the arrows @c F and @c G commute, @f$f \circ g = g \circ
 *  f@f$.  Primary @c std::false_type, a certificate as @c is_involutive is:
 *  that two maps commute is a claim about them, not a property of their
 *  calls.  Read symmetrically by @c commutes_v.  It is what makes a composite
 *  of two involutions one: @f$(g f)^2 = g f g f = g g f f = \mathrm{id}@f$
 *  where @f$f g = g f@f$, and no involution where they do not, as @c ~ and
 *  @c - on @c int below. */
export template <typename F, typename G>
struct commutes : std::false_type {};
export template <typename F, typename G>
inline constexpr bool commutes_v =
    commutes<std::remove_cvref_t<F>, std::remove_cvref_t<G>>::value ||
    commutes<std::remove_cvref_t<G>, std::remove_cvref_t<F>>::value;

/** @brief A composite of two commuting involutions is one, certified at the
 *  type where both legs are: @f$(g f)^2 = g g f f = \mathrm{id}@f$.  The same
 *  law @c is_involution reads on a value (@c :posetal), where a leg's law may
 *  be decided on its value instead. */
template <typename F, typename G, typename T>
  requires commutes_v<F, G>
struct is_involutive<Compose<F, G>, T>
    : std::bool_constant<is_involutive_v<F, T> && is_involutive_v<G, T>> {};

/** @section involution__Formal_Verification
 *  @c std::logical_not on @c bool is the canonical trivial involution
 *  (@c !!x = x): a certified witness that the seed rejects non-involutions. */
static_assert(IsInvolution<std::logical_not<bool>, bool>);
static_assert(IsInvolution<std::bit_not<int>, int>);
static_assert(IsInvolution<std::negate<std::size_t>, std::size_t>);
static_assert(!IsInvolution<std::negate<int>, int>,
              "unary minus on a signed carrier is not certified: -INT_MIN is "
              "undefined, so it is not a function on int.");
static_assert(IsInvolution<Reverse, std::string> &&
              IsInvolution<Reverse, std::vector<char>>);
static_assert(Reverse{}(Reverse{}(std::string{"posture"})) == "posture",
              "reversing twice is the identity, at compile time too.");
static_assert(!IsInvolution<decltype([](int x) { return x + 1; }), int>,
              "a merely-composable endomap is NOT an involution: the seed "
              "requires the is_involutive_v certificate, not just the shape.");
static_assert(std::logical_not<bool>{}(std::logical_not<bool>{}(true)) == true,
              "!!true = true: negation is an involution.");

/** @brief The arrow wrapping a callable has the law iff the callable does:
 *  @c endo<A>(f) is an involution when @c f is. */
template <typename A, typename Impl>
struct is_involutive<Morphism<A, A, Impl>, A>
    : std::bool_constant<is_involutive_v<Impl, A>> {};

/** @brief An involution is its own inverse: @f$f \circ f = \mathrm{id}@f$
 *  makes @c f an @c IsIsomorphism, through the @c :morphism hook (found by
 *  ordinary lookup, wherever @c f lives), with no @c inverse of its own.  The
 *  hooks for a specific arrow (@c Identity, the negate @c Morphism) are more
 *  specialised and keep precedence. */
template <typename F>
  requires(IsArrow<F> && IsInvolution<F>)
struct is_self_inverse<F> : std::true_type {};

/** @brief An involution is an endomorphism once read on its object: the
 *  implication holds of the lifted arrow, @c endo<T>(f), and not of a bare
 *  callable such as @c std::logical_not<bool>, which is a family of arrows,
 *  one per type, and no @c IsArrow until @c endo names its domain. */
static_assert(IsInvolution<std::logical_not<bool>, bool> &&
              !IsEndomorphism<std::logical_not<bool>> &&
              IsEndomorphism<decltype(endo<bool>(std::logical_not<bool>{}))> &&
              IsInvolution<decltype(endo<bool>(std::logical_not<bool>{}))>);

/** @brief The involutions the Jlt paper's Listing 1 defines from the
 *  certified callables: unary minus and the complement on @c size_t.
 *  Composed, −(~x) = x + 1 is an isomorphism, by (g∘f)⁻¹ = f⁻¹∘g⁻¹, and no
 *  involution: applied twice it is x + 2.  Not exported: a two-letter name
 *  at namespace scope shadows locals wherever the module is used. */
namespace detail_involution {
constexpr auto neg = endo<std::size_t>(std::negate<std::size_t>{});
constexpr auto flip = endo<std::size_t>(std::bit_not<std::size_t>{});
static_assert(IsInvolution<decltype(neg)> && IsInvolution<decltype(flip)> &&
              IsIsomorphism<decltype(flip >> neg)> &&
              !IsInvolution<decltype(flip >> neg)> && (flip >> neg)(3uz) == 4 &&
              (flip >> neg)((flip >> neg)(3uz)) == 5);
}  // namespace detail_involution

namespace detail_involution {
constexpr auto not_bool = endo<bool>(std::logical_not<bool>{});
constexpr auto not_not = not_bool >> not_bool;  // an involution twice
// two different involutions on int: ~ and − do not commute
constexpr auto flip_neg = endo<int>(std::bit_not<int>{}) >>
                          endo<int>(std::negate<int>{});
}  // namespace detail_involution
static_assert(
    IsInvolution<std::remove_cvref_t<decltype(detail_involution::not_bool)>> &&
        IsIsomorphism<
            std::remove_cvref_t<decltype(detail_involution::not_bool)>>,
    "endo<bool>(!) is an involution (the law forwarded from the "
    "callable), hence an isomorphism (its own inverse)");
static_assert(
    IsIsomorphism<std::remove_cvref_t<decltype(detail_involution::not_not)>> &&
        detail_involution::not_not(true) == true,
    "! ∘ ! is an isomorphism by (g∘f)⁻¹ = f⁻¹∘g⁻¹, and the identity "
    "at every point");
static_assert(
    IsIsomorphism<std::remove_cvref_t<decltype(detail_involution::flip_neg)>> &&
        !IsInvolution<
            std::remove_cvref_t<decltype(detail_involution::flip_neg)>, int> &&
        detail_involution::flip_neg(3) == 4,
    "− ∘ ~ is an isomorphism (two involutions compose to one) but no "
    "involution: −(~3) = 4, applied twice −(~4) = 5 ≠ 3; no rule "
    "certifies what is false");

/**
 * @section involution__Opposite
 * @brief The opposite operation, an involution on operations: exchanging the
 *        arguments twice is the operation itself.  The dagger below is an
 *        involutive anti-isomorphism, a homomorphism into the opposite.
 */
namespace detail_involution {
/** @brief @c Op with its arguments exchanged, @f$a \cdot^{op} b = b \cdot
 *  a@f$.  @tparam Op the operation. */
template <std::semiregular Op>
struct Reversed {
  /** @param a @param b the operands.  @return @c Op{}(b, a). */
  template <typename A, typename B>
    requires std::invocable<const Op&, const B&, const A&>
  constexpr auto operator()(const A& a, const B& b) const {
    return Op{}(b, a);
  }
  constexpr bool operator==(const Reversed&) const = default;
};
template <std::semiregular Op>
struct opposite {
  using type = Reversed<Op>;
};
template <std::semiregular Op>
struct opposite<Reversed<Op>> {
  using type = Op;
};
}  // namespace detail_involution
/** @brief The opposite operation, @f$a \cdot^{op} b = b \cdot a@f$, the
 *  product of the opposite monoid @f$M^{op}@f$ (Mac Lane, @e Categories for
 *  the Working Mathematician, II.2), an involution on operations:
 *  @c Opposite<Opposite<Op>> is @c Op, the same type.  A left action of @f$M@f$
 * is a right action of @f$M^{op}@f$, so a left action folded along a walk acts
 * by the product in @f$M^{op}@f$. The laws read backwards are the laws:
 * associativity, the unit, commutativity and idempotence carry over, declared
 * once here, and totality in @c :total.
 *  @tparam Op the operation. */
export template <std::semiregular Op>
using Opposite = typename detail_involution::opposite<Op>::type;
// Each law is read off the operation's value form, which sees registrations
// on the struct and on the value alike; the forwarding is a struct partial
// specialisation, which fires across module boundaries where a variable
// template's partial specialisation may not (@c algebra:registration).
template <typename T, std::semiregular Op>
struct is_associative<T, detail_involution::Reversed<Op>>
    : std::bool_constant<is_associative_v<T, Op>> {};
template <typename T, std::semiregular Op>
struct is_commutative<T, detail_involution::Reversed<Op>>
    : std::bool_constant<is_commutative_v<T, Op>> {};
template <typename T, std::semiregular Op>
struct is_idempotent<T, detail_involution::Reversed<Op>>
    : std::bool_constant<is_idempotent_v<T, Op>> {};
template <typename T, std::semiregular Op>
struct unit_trait<T, detail_involution::Reversed<Op>> : unit_trait<T, Op> {};

/** @brief Over a commutative operation on the codomain, a homomorphism into
 *  it is one into its opposite: @f$b \cdot a = a \cdot b@f$ there.
 *  @tparam F the map.  @tparam T its carrier.  @tparam Op @tparam Op2 the
 *  operations. */
template <typename F, typename T, typename Op, std::semiregular Op2>
  requires std::invocable<const F&, const T&> &&
           is_commutative_v<detail_morphism::image_t<F, T>, Op2> &&
           is_homomorphism_v<F, T, Op, Op2>
struct is_homomorphism<F, T, Op, detail_involution::Reversed<Op2>>
    : std::true_type {};
/** @brief The arrow wrapping a callable reverses an operation where the
 *  callable does, declared or derived: the meeting of the two rules above,
 *  more specialised than either, so that they never both apply.
 *  @tparam A @tparam B the arrow's ends.  @tparam Impl the callable.
 *  @tparam Op @tparam Op2 the operations. */
template <typename A, typename B, typename Impl, typename Op,
          std::semiregular Op2>
  requires std::same_as<detail_morphism::image_t<Impl, A>, B>
struct is_homomorphism<Morphism<A, B, Impl>, A, Op,
                       detail_involution::Reversed<Op2>>
    : std::bool_constant<
          is_homomorphism_v<Impl, A, Op, detail_involution::Reversed<Op2>>> {};

/** @concept IsAntiHomomorphism
 *  @brief A map on @c T that reverses @c Op, @f$f(a \cdot b) = f(b) \cdot
 *  f(a)@f$: a homomorphism into the opposite, @c Opposite<Op>, read off the
 *  homomorphism certificate rather than declared apart.  @tparam F the map.
 *  @tparam T its carrier.  @tparam Op the operation. */
export template <typename F, typename T, typename Op>
concept IsAntiHomomorphism =
    IsMagmaHomomorphism<F, T, Op, Opposite<std::remove_cvref_t<Op>>>;

/**
 * @section involution__Dagger_Category
 * @brief The @f$\dagger@f$-category surface built on @c IsInvolution:
 *        @c IsDagger, the named intermediate @c IsIsometry, and @c IsUnitary
 * --- where "the converse is the inverse" (bijective relation) and "the
 *        transpose is the inverse" (orthogonal matrix) are @b one theorem.
 *
 * @details A @f$\dagger@f$-category equips every arrow with a
 * direction-reversal
 * @f$f^\dagger@f$ (a contravariant, identity-on-objects, involutive functor).
 * An arrow is @b unitary (a @b dagger-iso) when its dagger @b is its inverse,
 * @f$f^\dagger = f^{-1}@f$.  That one law instantiates as:
 *   @li @b Rel: dagger = converse; unitary ⟺ the relation is a @b bijection.
 *   @li real inner-product spaces: dagger = transpose; unitary ⟺ @b orthogonal
 *       (@f$Q^{\top} = Q^{-1}@f$).
 *   @li complex Hilbert spaces: dagger = conjugate-transpose; unitary ⟺ the
 *       usual @b unitary matrix.
 * The two @b CRUCIAL findings the surface encodes --- unitarity is a @b value
 * property (not a type trait), and the dagger is the @b first bridge to the
 * mono / epi / iso factorisation surface --- are documented at the concepts
 * below.
 */
/**
 * @concept IsDagger
 * @brief A dagger on the arrows @c T of one object, composed by @c Op: an
 * involution, @f$f^{\dagger\dagger} = f@f$, that reverses composition,
 * @f$(f\,g)^\dagger = g^\dagger f^\dagger@f$.  A refinement of
 * @c IsInvolution, its certificate inherited, and of @c IsAntiHomomorphism, a
 * homomorphism into @c Opposite<Op>: the ring theorist's involution, an
 * involutive anti-automorphism.  The unit comes for free, since a bijective
 * anti-homomorphism carries the unit to the unit.  The one-object case: in
 * general a dagger's component is an arrow @f$\mathrm{Hom}(A, B) \to
 * \mathrm{Hom}(B, A)@f$, an isomorphism with the component at @f$(B, A)@f$ as
 * its inverse, and the involution is its diagonal (#787).
 * @note Discussed and postponed (2026-10-30): the multi-object dagger, a
 * refinement of @c IsIsomorphism with this involution as its diagonal.  A
 * matrix transpose is still a matrix, but off the diagonal not of the same
 * type: an @f$N \times M@f$ matrix transposes to an @f$M \times N@f$ one, and a
 * transposed tensor has its dimension order changed.  The tree's daggers are
 * all on square carriers, so the one-object case covers them for now.
 * @note Contravariance depends on the pair @c (T, @c Op): the transpose
 * reverses matrix multiplication only over a commutative scalar @c ⊗,
 * @f$(A B)^{\top}_{ij}=\sum_k A_{jk} B_{ki}@f$ while
 * @f$(B^{\top}A^{\top})_{ij}=\sum_k B_{ki} A_{jk}@f$; over a noncommutative
 * semiring it is still an involution but no dagger, which is the
 * conjugate-transpose against the scalar anti-involution.
 * @tparam Dagger the dagger.  @tparam T the arrows.  @tparam Op their
 * composition.
 */
export template <typename Dagger, typename T, typename Op>
concept IsDagger = IsInvolution<Dagger, T> && IsAntiHomomorphism<Dagger, T, Op>;

/**
 * @section involution__Unitary_Is_A_Value_Property
 * @brief @b CRUCIAL (load-bearing across the library): "the converse @b is the
 *        inverse" --- unitarity --- is a property of an @b arrow (a @b value),
 *        @b not of a type.  This is why the retired @c is_unitary certificate
 *        was wrong, and why the surface below is @b two coordinated forms of
 *        @b one computed law.
 *
 * @details A type-level @c is_involutive-style certificate would @b lie: a
 * general matrix / relation carrier holds @b both unitary and non-unitary
 * arrows, so no trait on the @b type can honestly claim "unitary".  The honest
 * thing is to @b compute @f$f^\dagger \circ f = \mathrm{id}@f$ on the @b arrow.
 *  @li @c is_isometry / @c is_coisometry / @c is_unitary are @b predicates on
 *      the arrow @b value --- always applicable (a matrix, a materialised
 *      relation);
 *  @li @c IsIsometry / @c IsUnitary are @b concepts that compute the same law
 *      on @c F{}, so they bite exactly on a @b singleton morphism-carrier (a
 *      type that @b is one arrow --- the @b Trsk relation carriers).
 * It is the same @b type-vs-value seam the library already lives on (@c IsSet
 * is structural, a @c Singleton is a @c Subobject): unitarity sits on the @b
 * value side, and a type-level trait pretending otherwise is precisely the bug.
 *
 * @section involution__Dagger_Fixes_The_Witness
 * @brief @b CRUCIAL: the dagger is the @b first bridge from this involution
 *        surface to the @c :morphism factorisation surface (mono / epi / iso).
 *        The @b arrow @b class fixes the inverse structure; the @b dagger fixes
 *        the @b witness (Heunen &amp; Vicary, @e Categories @e for @e Quantum
 *        @e Theory, OUP 2019; Selinger, ENTCS 170, 2007; nLab, @e dagger
 *        @e category).
 *
 * @details Layered on @c IsDagger, @b how @b far the dagger inverts an arrow
 * @b is its factorisation class --- and in each case the dagger @b hands over
 * the witness a bare arrow would have to @b search for:
 * @verbatim
 *   dagger law                arrow class      witness the dagger supplies
 *   ---------------------------------------------------------------------------
 *   f† ∘ f = id  (isometry)    split MONO       retraction  r = f†   (r∘f = id)
 *   f ∘ f† = id  (coisometry)  split EPI        section     s = f†   (f∘s = id)
 *   both         (unitary)     ISO              inverse     f⁻¹ = f†
 *   f f† f = f   (partial iso) general m∘e      generalized inv. f†
 * (Moore–Penrose)
 * @endverbatim
 * The fourth row completes the ladder: a general arrow @c f=m∘e (dagger-mono
 * @c m ∘ dagger-epi @c e) is a @b partial @b isometry, its dagger the
 * @b generalized (Moore–Penrose) inverse (@f$f f^\dagger f = f@f$, and
 * @f$f^\dagger f f^\dagger = f^\dagger@f$) --- a two-sided @b partial inverse,
 * neither @c f†f nor @c ff† being @c id but each a projection.  No concept yet
 * (see the follow-up); paper Table @c tab:inverse-image-laws carries all four.
 * @c IsIsometry is the @b named intermediate (Heunen &amp; Vicary): @b no
 * linearity --- a split mono in a @f$\dagger@f$-monoid (the @b one-object case:
 * @c f is an endo-arrow, @c Op its composition; a general @f$f:A\to B@f$ needs
 * the multi-object surface), so bijective relation, orthogonal matrix and
 * unitary matrix are @b one theorem.  The implication runs @b one way (a mono
 * need not be an isometry).  The @b real subsumption @c IsIsometry
 * @f$\Rightarrow@f$ split mono (and coisometry
 * @f$\Rightarrow@f$ split epi, unitary @f$\Rightarrow@f$ @c IsIsomorphism, by
 * registering @c inverse(f)@c =@c f†) needs the dagger carriers lifted into the
 * @c :morphism arrow surface --- a @b follow-up (tracking issue).  Here the
 * correspondence is @b stated and the law is @b witnessed; paper
 * Table @c tab:inverse-image-laws carries the full picture.
 */

/** @brief The isometry law, @b computed on an arrow @c f:
 *  @f$f^\dagger \circ f = \mathrm{id}@f$ --- the dagger is a @b left inverse.
 *  General over the dagger @c Dagger and composition @c Op (@c id is the
 *  @c Op-identity @c unit_v); @b no linearity.  @c f is an @b endo-arrow
 *  (@c Op a monoid on the single object @c T); a general @f$f:A\to B@f$
 * isometry is the multi-object case, left to the arrow surface. */
export template <typename Dagger, typename Op, typename T>
  requires std::equality_comparable<T> && requires(const T& g) {
    Op{}(Dagger{}(g), g);
    unit<T>(Op{});
  }
constexpr bool is_isometry(const T& f) {
  return Op{}(Dagger{}(f), f) == unit<T>(Op{});
}

/** @brief The coisometry law: @f$f \circ f^\dagger = \mathrm{id}@f$ --- the
 *  dagger is a @b right inverse. */
export template <typename Dagger, typename Op, typename T>
  requires std::equality_comparable<T> && requires(const T& g) {
    Op{}(g, Dagger{}(g));
    unit<T>(Op{});
  }
constexpr bool is_coisometry(const T& f) {
  return Op{}(f, Dagger{}(f)) == unit<T>(Op{});
}

/** @brief The unitary law: isometry @b and coisometry, hence
 *  @f$f^\dagger = f^{-1}@f$ (a dagger-iso).  Generalises the permutation's
 *  P° ; P = Δ check: the @b one predicate reads as "the converse is the
 * inverse" (bijection, @c Mat(𝔹)), "the transpose is the inverse" (orthogonal,
 *  @c Mat(ℝ)) and "the adjoint is the inverse" (unitary, @c Mat(ℂ)) --- the
 *  real / complex face of the same law. */
export template <typename Dagger, typename Op, typename T>
constexpr bool is_unitary(const T& f) {
  return is_isometry<Dagger, Op>(f) && is_coisometry<Dagger, Op>(f);
}

/** @concept IsIsometry
 *  @brief A @b singleton morphism-carrier @c F (its default value @b is the
 *  arrow) whose dagger is a left inverse: the @b named non-linear intermediate
 *  (Heunen &amp; Vicary).  For a value-level carrier, use the @c is_isometry
 *  predicate instead. */
export template <typename F, typename Dagger, typename Op>
concept IsIsometry = IsDagger<Dagger, F, Op> && std::default_initializable<F> &&
                     requires { requires is_isometry<Dagger, Op>(F{}); };

/** @concept IsUnitary
 *  @brief A singleton morphism-carrier whose dagger @b is its inverse (isometry
 *  @b and coisometry) --- a dagger-iso, @f$f^\dagger = f^{-1}@f$.  Hence an
 *  @c IsIsomorphism once lifted into the arrow surface (follow-up). */
export template <typename F, typename Dagger, typename Op>
concept IsUnitary = IsIsometry<F, Dagger, Op> &&
                    requires { requires is_coisometry<Dagger, Op>(F{}); };

/** @section involution__Concept_Verification
 *  @brief The concepts are @b instantiated here, so an ill-formed constraint is
 *  a @b compile error rather than latent.  @c std::bit_not is an
 *  @b involution on @c int but no dagger of @c +: @c ~(a+b) @c ≠ @c ~a+~b, so
 *  no homomorphism into the opposite is declared, and @c IsDagger, which asks
 *  for one, refuses it.  The (co)isometry law computes @b false as well:
 *  @c ~0 @c + @c 0 @c = @c -1 @c ≠ @c 0, the @c +-identity.  A @b positive
 *  concept witness needs a @b singleton morphism-carrier whose default value
 *  @b is a unitary arrow (follow-up #787); the value-level @c is_unitary
 *  predicate is witnessed over @c Mat(𝔹) (a permutation) in
 *  @c linear_algebra, and the real / complex (orthogonal / unitary) reading
 *  is the @b same predicate, left unexhibited for want of a @b constexpr
 *  real-field carrier (#790). */
static_assert(IsInvolution<std::bit_not<int>, int> &&
                  !IsDagger<std::bit_not<int>, int, std::plus<int>>,
              "bit_not is an involution on int, no dagger of +.");
static_assert(!IsIsometry<int, std::bit_not<int>, std::plus<int>>,
              "int is not an isometry under (bit_not, +).");
static_assert(!IsUnitary<int, std::bit_not<int>, std::plus<int>>,
              "int is not unitary under (bit_not, +): ~0 + 0 = -1 ≠ 0.");

}  // namespace dedekind::category
