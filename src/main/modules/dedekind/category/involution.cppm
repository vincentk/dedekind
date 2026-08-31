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

#include <concepts>    // std::convertible_to
#include <functional>  // std::logical_not (the canonical trivial involution)

export module dedekind.category:involution;

namespace dedekind::category {

/**
 * @concept IsInvolution
 * @brief An endomap @f$f : T \to T@f$ that is its own inverse:
 *        @f$f \circ f = \mathrm{id}_T@f$.
 *
 * @details The atomic seed of the @f$\dagger@f$ notion.  This is the
 * @b shape-level check (@c f is a callable @c T @c → @c T composable with
 * itself); the semantic law @f$f(f(x)) = x@f$ is a @c static_assert obligation
 * at the point of certification, in the library's usual "concept carries the
 * shape, witness carries the law" style.
 */
export template <typename F, typename T>
concept IsInvolution = requires(const F& f, const T& x) {
  { f(x) } -> std::convertible_to<T>;
  { f(f(x)) } -> std::convertible_to<T>;
};

/** @section involution__Formal_Verification
 *  @c std::logical_not on @c bool is the canonical trivial involution
 *  (@c !!x = x): a shape witness that the seed compiles and a law witness that
 *  the double application is the identity. */
static_assert(IsInvolution<std::logical_not<bool>, bool>);
static_assert(std::logical_not<bool>{}(std::logical_not<bool>{}(true)) == true,
              "!!true = true: negation is an involution.");
static_assert(std::logical_not<bool>{}(std::logical_not<bool>{}(false)) ==
                  false,
              "!!false = false: negation is an involution.");

}  // namespace dedekind::category
