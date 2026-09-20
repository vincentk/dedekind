/**
 * @file dedekind/category/iso.cppm
 * @partition :iso
 * @brief The retract / iso-enabling arrow surface: the constructions that turn
 *        a factorisation class into a downstream decidability class.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section iso__What_Lives_Here
 * The "advanced" arrow constructions, extracted from @c :morphism so the arrow
 * base keeps only the lean factorisation primitives (@c IsArrow /
 * @c IsMonicArrow / @c IsEpicArrow / @c IsIsomorphism stay there), mirroring
 * how the @c is_involutive machinery was extracted into @c :involution:
 *  @li @c IsRetractableArrow, a monic arrow carrying a partial inverse: the
 *      concept the @c :image decidability seam dispatches on;
 *  @li @c IsoRetract and the blanket @c retract, the wiring
 *      @c IsIsomorphism ⟹ @c IsRetractableArrow, so a sound iso is retractable
 *      @b by @b construction (its total @c inverse wrapped in always-Some),
 * with no manual, unaudited hook.
 *
 * @section iso__Retract_As_Kleisli
 * A retract is a Kleisli arrow into the maybe monad,
 * @c retract(f) @c : @c Cod<F> @c → @c Maybe<Dom<F>>, spelled honestly with
 * @c category::Maybe and the unit @c η (@c maybe_hub) from @c :functor /
 * @c :natural, not raw @c std::optional.  That is why this partition sits
 * downstream of @c :natural (@c :morphism → @c :natural → @c :iso): it removes
 * the layering inversion in which @c :morphism (importing only @c :species)
 * reached for a raw @c optional that is really @c Maybe.
 *
 * @section iso__Branch_On_Iso
 * Dispatch branches on iso: the @c :image @c ImageChi iso specialisation and
 * the DSL @c image(iso, Set) overload (#657) take precedence over the retract
 * path (guarded @c !IsIsomorphism), so an iso uses its own tighter path and
 * only monic-but-not-iso arrows take the retract route.  The blanket iso @c
 * retract additionally gates on the inverse arrow being @b const-invocable (the
 * #823 discipline), so a mutable-inverse-only iso is @b not advertised
 * retractable.
 *
 * Wikipedia: Section (category theory); Kleisli category; Equaliser
 * (mathematics).
 *
 * @note "Every standard construction is induced by a pair of adjoint
 *       functors."  Heinrich Kleisli, Proc. Amer. Math. Soc. 16(3) (1965),
 *       pp. 544-546: the title of the note showing every monad (a "standard
 *       construction") resolves through the Kleisli category.  Fitting, since a
 *       retract here is a Kleisli arrow, the structure handed back through the
 *       unit @c η.  (Kleisli wrote in English; no translation needed.)
 *
 * @build_order imports @c :morphism, @c :functor, @c :natural; upstream of
 *              @c :image (which dispatches on @c IsRetractableArrow).
 */
module;

#include <concepts>     // std::same_as
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::forward

export module dedekind.category:iso;

import :morphism;  // IsArrow, IsIsomorphism, IsMonicArrow, Cod, Dom, inverse
import :functor;   // Maybe<T> = std::optional<T> (the retract's monad)
import :natural;   // η (the maybe unit) + maybe_hub tag

namespace dedekind::category {

/**
 * @brief The retract of an @c IsIsomorphism: its total @c inverse arrow
 *        wrapped in an always-engaged @c std::optional.
 * @details Every iso @f$f:A\to B@f$ has a two-sided inverse
 *          @f$f^{-1}:B\to A@f$, so its retract is total: @c retract(f)(y)
 *          is always @c Some(@c f^{-1}(y)), built through the maybe unit
 *          @c η(maybe_hub, ·) rather than a raw @c std::optional constructor.
 *          A named functor (not a lambda) so the type is nameable and the
 *          closure is transparent.
 * @tparam F The iso arrow type.
 */
export template <typename F>
struct IsoRetract {
  F f;
  constexpr Maybe<Dom<F>> operator()(const Cod<F>& y) const {
    return η(maybe_hub, inverse(f)(y));
  }
};

/** @brief Blanket @c retract for @b any @c IsIsomorphism: wraps @c inverse(f)
 *  in always-Some (see @c IsoRetract).  This is what makes
 *  @c IsIsomorphism ⟹ @c IsRetractableArrow hold with no manual hook.
 *
 *  Declared @b before @c IsRetractableArrow so the concept's dependent
 *  @c retract(f) lookup finds it by @b ordinary (non-ADL) lookup: an iso
 *  defined in a client namespace (whose associated namespaces do not include
 *  @c dedekind::category) is then still retractable, as the blanket
 *  implication advertises.
 *
 *  Gated on the inverse arrow being @b const-invocable: @c IsoRetract holds
 *  @c f as a @c const member and applies @c inverse(f) in a @c const
 *  @c operator(), so a mutable-inverse-only iso (one that satisfies
 *  @c IsIsomorphism but whose inverse arrow has a mutable-only @c operator())
 *  must @b not be advertised as retractable: without this guard the concept's
 *  call-expression check passes (the body is not instantiated) yet the actual
 *  @c retract(f)(y) fails to compile.  This mirrors the #823 const-invocability
 *  discipline the @c ImageChi retract seam also enforces. */
export template <typename F>
  requires IsIsomorphism<std::remove_cvref_t<F>> &&
           requires(const std::remove_cvref_t<F>& cf, const Cod<F>& y) {
             inverse(cf)(y);
           }
constexpr auto retract(F&& f) {
  return IsoRetract<std::remove_cvref_t<F>>{std::forward<F>(f)};
}

/**
 * @concept IsRetractableArrow
 * @brief A monic arrow that ships with a structurally-known @em retract, a
 *        partial inverse @c retract(f) @c : @c Cod<F> @c → @c Maybe<Dom<F>>,
 *        discoverable via ADL on @p F.
 *
 * @details The retract is the operational gate for a decidability path
 * on @c image(f, S) that's strictly more general than @c IsIsomorphism
 * (which requires a @em total inverse): for a monic @c F that admits a
 * partial inverse, the image membership
 *
 *   @c y @c ∈ @c image(F, @c S)
 *
 * reduces to
 *
 *   @c let @c mx @c = @c retract(f)(y); @c mx.has_value() @c && @c
 * S(*mx)
 *
 * which is decidable whenever the retract itself is decidable.  The
 * canonical project use case is the @c embed_* family of carrier-
 * lattice embeddings, each of which is monic and admits a natural
 * partial inverse (e.g.\ @c embed_𝔹_ℕ has retract
 * @c Cardinality @c → @c std::optional<bool>; @c embed_uint_ℕ has
 * retract @c Cardinality @c → @c std::optional<unsigned> that fires on
 * the finite-representable range).
 *
 * @par Concept shape
 * The concept requires that @c retract(f) is invocable on
 * @c Cod<F> @c const& and returns a @c Maybe -shaped value
 * (@c Maybe<T> @c = @c std::optional<T>, supporting @c has_value() and
 * @c operator*).  Generalising to
 * the project's broader @c IsPotential surface (which also admits
 * @c Partial<T> / @c TernaryResult<T> as Maybe-likes) is a deliberate
 * follow-up; for retracts specifically, the binary has/has-not
 * distinction @c std::optional carries is what the image-overload
 * needs, and tighter shapes than that aren't load-bearing today.
 *
 * @par Relation to @c IsIsomorphism
 * Mathematically, an isomorphism has a total inverse, so a retract for
 * it is available by construction (wrap @c inverse(f) in always-Some).
 * We wire that up: @c IsMonicArrow accepts isos (in @c :morphism) and the
 * @c IsoRetract blanket @c retract above serves every @c IsIsomorphism,
 * so @c IsIsomorphism ⟹ @c IsRetractableArrow with @b no manual opt-in.
 * A sound iso yields a sound retract, so this trades the (never audited)
 * requirement to hand-register a retract for a derivation that cannot
 * drift from @c inverse.  Dispatch still branches on iso where a tighter
 * iso path exists: the DSL @c image(iso, Set) overload (#657, in
 * @c :sets:expressions) and the @c ImageChi iso specialisation both take
 * precedence over the retract path (guarded @c !IsIsomorphism), so an iso
 * uses its own path and only monic-but-not-iso arrows take the retract
 * route.  The point is to avoid divergent parallel paths, not to guard a
 * developer against a mathematically-sound derived retract.
 *
 * @par Opt-in semantics
 * A monic-but-not-iso retract is user-declared via the @c retract(f) ADL
 * hook; there the user owns the @b correctness obligation (the hook
 * genuinely partially-inverts F).  The iso case needs no such hook.
 */
export template <typename F>
concept IsRetractableArrow = IsMonicArrow<F> && requires(F f, const Cod<F>& y) {
  { retract(f)(y) } -> std::same_as<Maybe<Dom<F>>>;
};

}  // namespace dedekind::category
