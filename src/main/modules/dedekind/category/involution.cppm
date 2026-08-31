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
 * @section involution__Why_A_Seed
 * This partition is deliberately @b almost @b empty.  It is the extraction
 * target flagged by @c :lattice's @c Involutive_Endofunctor (an involution at
 * the @b functor level, @f$F^2 \cong \mathrm{Id}@f$) and the natural home for a
 * notion the library keeps re-discovering under different names --- in each the
 * dagger @f$\dagger@f$ coincides with the inverse exactly on the "good" arrows:
 *
 *  @li @b Rel: dagger = converse @f$R^{\smile}@f$; @f$R^{\smile}=R^{-1}@f$ iff
 *      @c R is a @b bijection.
 *  @li real inner-product spaces: dagger = transpose @f$Q^{\top}@f$;
 *      @f$Q^{\top}=Q^{-1}@f$ iff @c Q is @b orthogonal.
 *  @li complex Hilbert spaces: dagger = conjugate-transpose @f$Q^{*}@f$;
 *      @f$Q^{*}=Q^{-1}@f$ iff @c Q is @b unitary.
 *  @li a Boolean subobject lattice: dagger = complement @f$\neg@f$ (self-dual).
 *
 * The unifying statement --- an arrow in a @f$\dagger@f$-category whose dagger
 * @b is its inverse (a @b unitary / dagger-iso) --- is what makes "the converse
 * is the inverse" (bijective relation) and "the transpose is the inverse"
 * (orthogonal matrix) @b one theorem.  Building that @f$\dagger@f$/unitary
 * surface here (so an orthogonal/unitary matrix gets a @b default @c inverse
 * from its transpose/adjoint, and a bijective relation from its converse) is
 * the follow-up this seed exists to receive.  For now it carries only the
 * atomic
 * @c IsInvolution concept; the @f$\dagger@f$-category and @c IsUnitary layers,
 * and the migration of @c Involutive_Endofunctor, land when the second consumer
 * (linear algebra) arrives.
 *
 * @build_order atom (Level 0); no intra-category dependencies.
 */
module;

#include <concepts>    // std::invocable, std::convertible_to
#include <functional>  // std::logical_not / std::bit_not (canonical involutions)
#include <type_traits>  // std::invoke_result_t, std::is_integral_v

export module dedekind.category:involution;

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
export template <typename F, typename T>
concept IsInvolution =
    std::invocable<F, T> &&
    std::convertible_to<std::invoke_result_t<F, T>, T> && is_involutive_v<F, T>;

/** @section involution__Formal_Verification
 *  @c std::logical_not on @c bool is the canonical trivial involution
 *  (@c !!x = x): a certified witness that the seed rejects non-involutions. */
static_assert(IsInvolution<std::logical_not<bool>, bool>);
static_assert(IsInvolution<std::bit_not<int>, int>);
static_assert(!IsInvolution<decltype([](int x) { return x + 1; }), int>,
              "a merely-composable endomap is NOT an involution: the seed "
              "requires the is_involutive_v certificate, not just the shape.");
static_assert(std::logical_not<bool>{}(std::logical_not<bool>{}(true)) == true,
              "!!true = true: negation is an involution.");

/**
 * @section involution__Next_Dagger_Unitary
 * @brief SEED for the follow-up (PR TBD): the @f$\dagger@f$-category surface
 * and
 *        @c IsUnitary --- where "the converse is the inverse" (bijective
 *        relation) and "the transpose is the inverse" (orthogonal matrix)
 * become
 *        @b one theorem.
 *
 * @details A @f$\dagger@f$-category equips every arrow with a
 * direction-reversal
 * @f$f^\dagger@f$ (a contravariant, identity-on-objects, involutive functor).
 * An arrow is @b unitary (a @b dagger-iso) when its dagger IS its inverse,
 * @f$f^\dagger = f^{-1}@f$.  That single certificate instantiates as:
 *   @li @b Rel: dagger = converse; unitary ⟺ the relation is a @b bijection.
 *   @li real inner-product spaces: dagger = transpose; unitary ⟺ @b orthogonal
 *       (@f$Q^{\top} = Q^{-1}@f$).
 *   @li complex Hilbert spaces: dagger = conjugate-transpose; unitary ⟺ the
 *       usual @b unitary matrix.
 * With it, @c inverse gets a @b default from the dagger wherever the
 * converse/transpose already is one.
 *
 * The follow-up will design the categorical @c IsUnitary concept (it needs the
 * @f$\dagger@f$ operation + inverse + composition, which vary per category);
 * that design is intentionally left open.  This seed pins only the atomic
 * @b certificate slot below --- the opt-in trait that says "@c F's dagger is
 * its inverse on @c T", exactly parallel to @c is_involutive above.  It has no
 * consumers yet; the @f$\dagger@f$-category and the per-carrier witnesses
 * (bijective @c Rel, orthogonal matrix) arrive with the linear-algebra
 * consumer.
 */
export template <typename F, typename T>
struct is_unitary : std::false_type {};

export template <typename F, typename T>
inline constexpr bool is_unitary_v = is_unitary<F, T>::value;

// A unitary arrow is in particular an involution up to inverse; the general
// relationship (and the per-carrier opt-ins: bijection, orthogonal, unitary)
// is deferred to the follow-up's dagger-category surface.

}  // namespace dedekind::category
