/**
 * @file dedekind/category/image.cppm
 * @partition :image
 * @brief The image of an arrow as a structurally-named subobject of its
 *        codomain — `IsImageOf<S, F>`.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section image__Motivation
 *
 * Lifts the @em image role to first-class concept status, completing
 * the kernel-pair / coequalizer naming chain set up in @c :pullback:
 *
 *   @c IsKernelPair @c → @c IsCoequalizer @c → @c IsImageOf
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
 * (the inclusion of the image into the codomain).  The two readings —
 * @em quotient @em of @em domain (kernel-pair coequalizer) and
 * @em subobject @em of @em codomain (mono part of the factorisation)
 * — are isomorphic in a topos.  This partition pins the
 * @b mono-side reading as @c IsImageOf<S, F>: @c S is a Subobject of
 * @c Cod<F>.  The dual quotient-side reading is named in
 * @c :pullback as @c IsCoequalizer.
 *
 * @section image__Structural_vs_Universal
 *
 * What @c IsImageOf @b can pin (structural shape):
 *
 *   - @c F is an arrow ( @c IsArrow<F>),
 *   - @c S is a Subobject of @c Cod<F> ( @c IsSubobject<S, @c Cod<F>>).
 *
 * What @c IsImageOf @b cannot pin (engineer's honesty obligation, as
 * for @c IsNNO and @c IsCoequalizer):
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

import :morphism;  // For IsArrow / Cod
import :topoi;     // For IsSubobject

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
 * obligation, as for @c IsNNO and @c IsCoequalizer.  C++ concepts
 * cannot quantify over the universal property; they pin the
 * operational signature only.
 *
 * @tparam S The candidate image species (a Subobject of @c Cod<F>).
 * @tparam F The arrow whose image @c S claims to realise.
 */
export template <typename S, typename F>
concept IsImageOf = IsArrow<F> && IsSubobject<S, Cod<F>>;

}  // namespace dedekind::category
