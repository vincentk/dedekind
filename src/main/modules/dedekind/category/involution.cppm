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
 * @build_order imports @c :species (@c identity_v); upstream of @c :lattice /
 *              @c :category.
 */
module;

#include <concepts>    // std::invocable, std::convertible_to
#include <functional>  // std::logical_not / std::bit_not (canonical involutions)
#include <type_traits>  // std::invoke_result_t, std::is_integral_v

export module dedekind.category:involution;

import :species;  // identity_v: the Op-identity the (co)isometry law is
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
 * @brief A @b certified dagger on @c T: a direction-reversal
 * @f$f \mapsto f^\dagger@f$ that is an @b involution
 * (@f$f^{\dagger\dagger} = f@f$).  At the endomap level a dagger @b is exactly
 * this involutive core, so @c IsDagger is @b deliberately @c IsInvolution; the
 * contravariance @f$(f\,g)^\dagger = g^\dagger f^\dagger@f$ and
 * identity-on-objects structure are @b not re-checked here (they hold for the
 * daggers the library registers: converse in @c order::halfspace, transpose in
 * @c linear_algebra).
 */
export template <typename Dagger, typename T>
concept IsDagger = IsInvolution<Dagger, T>;

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
 *  @c Op-identity @c identity_v); @b no linearity.  @c f is an @b endo-arrow
 *  (@c Op a monoid on the single object @c T); a general @f$f:A\to B@f$
 * isometry is the multi-object case, left to the arrow surface. */
export template <typename Dagger, typename Op, typename T>
constexpr bool is_isometry(const T& f) {
  return Op{}(Dagger{}(f), f) == identity_v<T, Op>;
}

/** @brief The coisometry law: @f$f \circ f^\dagger = \mathrm{id}@f$ --- the
 *  dagger is a @b right inverse. */
export template <typename Dagger, typename Op, typename T>
constexpr bool is_coisometry(const T& f) {
  return Op{}(f, Dagger{}(f)) == identity_v<T, Op>;
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
concept IsIsometry = IsDagger<Dagger, F> && std::default_initializable<F> &&
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
 *  a @b compile error rather than latent: @c std::bit_not is a certified dagger
 *  (an involution) on @c int, but @c int is @b not unitary under @c + because
 *  @c ~0 @c + @c 0 @c = @c -1 @c ≠ @c 0 (the @c +-identity) --- the dagger is
 * not the inverse.  A @b positive concept witness needs a @b singleton
 *  morphism-carrier whose default value @b is a unitary arrow, which arrives
 *  with the arrow surface (follow-up); the value-level @c is_unitary predicate
 *  is witnessed over @c Mat(𝔹) (a permutation) in @c linear_algebra.  The real
 * / complex (orthogonal / unitary) reading is the @b same predicate, left
 *  unexhibited only for want of a @b constexpr real-field carrier. */
static_assert(IsDagger<std::bit_not<int>, int>,
              "bit_not is a certified dagger (an involution) on int.");
static_assert(!IsIsometry<int, std::bit_not<int>, std::plus<int>>,
              "int is not an isometry under (bit_not, +).");
static_assert(!IsUnitary<int, std::bit_not<int>, std::plus<int>>,
              "int is not unitary under (bit_not, +): ~0 + 0 = -1 ≠ 0.");

}  // namespace dedekind::category
