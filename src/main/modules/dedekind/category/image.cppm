/**
 * @file dedekind/category/image.cppm
 * @partition :image
 * @brief Image-as-coequalizer-of-kernel-pair: @c IsImageOf<S, F> on the
 *        mono side and @c IsCoequalizer<Q, F, G> on the quotient side.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section image__Motivation
 *
 * Hosts the canonical epi-mono factorisation chain set up by
 * @c :pullback (kernel pair, parallel pair) and @c :topoi (subobject,
 * quotient):
 *
 *   @c IsKernelPair @c → @c IsParallelPair @c → @c IsCoequalizer @c → @c
 * IsImageOf
 *
 * For an arrow @c F @c : @c A @c → @c B, the @b image @c Im(F) is the
 * coequalizer of the kernel pair of @c F:
 *
 *   @f$\mathrm{Im}(F) @f$ @f$=@f$ @f$\mathrm{coeq}(\pi_1, \pi_2 :
 *   \mathrm{KernelPair}(F) \rightrightarrows A)@f$.
 *
 * Equivalently (in a topos) the image is the smallest @b subobject of
 * @c B through which @c F factors, giving the canonical epi-mono
 * factorisation @c F @c = @c m @c ∘ @c e:
 *
 *   @c A @c → @c Im(F) @c → @c B
 *
 * with @c e regular-epi (the coequalizer projection) and @c m mono
 * (the inclusion of the image into the codomain).  Two structurally
 * named concepts pin the two sides:
 *
 *   - @b mono @b side: @c IsImageOf<S, F> --- @c S is a Subobject of
 *     @c Cod<F> realising the image.
 *   - @b quotient @b side: @c IsCoequalizer<Q, F, G> --- @c Q is a
 *     Quotient of @c Cod<F> equating the parallel pair @c F, @c G.
 *
 * Both are co-located here in @c :image since the image-as-coequalizer
 * reading is the structural payoff that ties them together.  The
 * universal-property obligations (smallest such subobject, canonical
 * factorisation, regular-epi universality of @c q) remain the
 * engineer's honesty obligation in the @c IsNNO style.
 *
 * @section image__Structural_vs_Universal
 *
 * What @c IsImageOf @b can pin (structural shape):
 *
 *   - @c F is an arrow ( @c IsArrow<F>),
 *   - @c S is a Subobject of @c Cod<F> ( @c IsSubobject<S, @c Cod<F>>).
 *
 * What @c IsImageOf @b cannot pin (engineer's honesty obligation, as
 * for @c IsNNO):
 *
 *   - That @c S is the @b smallest such subobject (universality of the
 *     image factorisation),
 *   - That @c F factors through @c S as @c m @c ∘ @c e with @c e
 *     regular-epi and @c m mono,
 *   - That the factorisation is unique up to isomorphism.
 *
 * C++ concepts cannot quantify over the universal property's $\forall$;
 * they can only pin operational signatures.  The naming here is the
 * structural anchor — a downstream carrier witness declares
 * @c IsImageOf<MyImageSet, @c MyArrow> to record the @b intent that
 * its subobject realises the image.  The audit (that this is in fact
 * the smallest containing subobject) lives in the witness's
 * documentation and review.
 *
 * @section image__Realisation_Regimes
 *
 * The §3 paper outline (`Functions on Sets: Filtering and
 * Transforming`) names three realisation regimes for the abstract
 * image, each pinned by @c IsImageOf with a different @c S:
 *
 *   - @b Extensional (finite carriers): @c S is a concrete
 *     std-container subobject realised by iteration in
 *     @c sets:extensional ( @c image(f, @c std::unordered_set<T>) etc.).
 *   - @b Intensional (NNO-domain transforms): @c S is the orbit closure
 *     of an iterated step, naturally pinnable on @c Path<T> in
 *     @c sequences:path (categorical-anchor breadcrumbs land in the
 *     follow-up slice).
 *   - @b Symbolic (Tier C, future): @c S is a deferred set expression
 *     under an inhabitancy DSL (#603), evaluated lazily.
 *
 * @section image__Layering
 *
 * Upstream-of-everything-numeric.  Lives in @c :category alongside
 * @c :pullback; carrier witnesses register downstream where the
 * carriers are available ( @c sets:extensional for std-container
 * carriers; @c sequences:path for the NNO-morphism reading).
 *
 * @note "There is geometry in the humming of the strings, there is
 *        music in the spacing of the spheres."
 *       — Pythagoras, fragment.
 */
module;

#include <concepts>

export module dedekind.category:image;

import :morphism;  // For IsArrow / Cod / Dom
import :pullback;  // For IsParallelPair
import :topoi;     // For IsSubobject / IsQuotient

namespace dedekind::category {

/**
 * @concept IsImageOf
 * @brief @c S realises the image of arrow @c F as a Subobject of @c Cod<F>.
 *
 * @details Structurally, @c IsImageOf<S, F> is exactly @c IsArrow<F>
 * @c && @c IsSubobject<S, @c Cod<F>>.  The added value over the bare
 * conjunction is the @b naming: a downstream witness declaring this
 * concept records the intent that its subobject @b is the image of
 * @c F (the smallest subobject of @c Cod<F> through which @c F
 * factors), not just any subobject of @c Cod<F>.
 *
 * The universality (smallest such subobject; canonical epi-mono
 * factorisation @c F @c = @c m @c ∘ @c e) is the engineer's honesty
 * obligation, as for @c IsNNO.  C++ concepts cannot quantify over
 * the universal property; they pin the operational signature only.
 *
 * @tparam S The candidate image species (a Subobject of @c Cod<F>).
 * @tparam F The arrow whose image @c S claims to realise.
 */
export template <typename S, typename F>
concept IsImageOf = IsArrow<F> && IsSubobject<S, Cod<F>>;

/**
 * @concept IsCoequalizer
 * @brief The categorical dual of @c IsEqualizer --- a Quotient of
 *        @c Cod<F> equating two parallel arrows @c F, @c G.
 *
 * @details For a parallel pair @c F, @c G @c : @c A @c → @c B, the
 * coequalizer is the smallest quotient @c Q of @c B together with a
 * regular epi @c q @c : @c B @c → @c Q satisfying the equalising
 * condition
 *
 *   @c q @c ∘ @c F @c = @c q @c ∘ @c G                 (equalising)
 *
 * universal among such: any other arrow @c B @c → @c X equating @c F
 * and @c G factors uniquely through @c q.  In a topos the coequalizer
 * is the regular-epi onto the quotient by the smallest equivalence
 * relation containing the image of the parallel pair.
 *
 * Where @c IsEqualizer pins the structural shape via
 * @c IsSubobject<E, @c Dom<F>>, the dual @c IsCoequalizer pins it via
 * @c IsQuotient<Q, @c Cod<F>> (in @c :topoi as the dual to
 * @c IsSubobject).  The universal property's $\forall$ ---
 * @em existence and @em uniqueness of the unique factoring map for any
 * arrow that equates the kernel relation, plus the equalising
 * condition @c q∘F @c = @c q∘G --- is the engineer's honesty
 * obligation, as for @c IsNNO.
 *
 * @section image__Image_as_coequalizer
 *
 * For a single arrow @c F @c : @c A @c → @c B with kernel pair
 * @c π_1, @c π_2 @c : @c P @c ⇒ @c A (@c P satisfying
 * @c IsKernelPair<P, @c F> in @c :pullback), the @em image of @c F is
 * the coequalizer of @c (π_1, @c π_2) --- the canonical epi-mono
 * factorisation
 *
 *   @c A @c → @c Im(F) @c → @c B
 *
 * read off as "collapse the equivalence relation 'same F-image'".
 * The mono side @c Im(F) @c ↪ @c B is what @c IsImageOf above pins;
 * the quotient side @c A @c ↠ @c A/~ is what @c IsCoequalizer here
 * pins (with @c F = @c π_1, @c G = @c π_2 over the kernel pair).
 *
 * @tparam Q The candidate coequalizer species (a Quotient of @c Cod<F>).
 * @tparam F The first parallel arrow @c F @c : @c A @c → @c B.
 * @tparam G The second parallel arrow @c G @c : @c A @c → @c B.
 */
export template <typename Q, typename F, typename G>
concept IsCoequalizer = IsParallelPair<F, G> && IsQuotient<Q, Cod<F>>;

}  // namespace dedekind::category
