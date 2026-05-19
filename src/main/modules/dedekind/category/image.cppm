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
 * @note "ἁρμονία δὲ πολυμιγέων ἕνωσις καὶ δίχα φρονεόντων
 *        συμφρόνησις."
 *       ("Harmony is the unification of things many-mixed, and the
 *        same-mindedness of things thinking-apart.")
 *       — Philolaus of Croton, fragment B10 DK,
 *         apud Stobaeus, @em Anthology I.21.7d.
 */
module;

#include <concepts>
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::forward

export module dedekind.category:image;

import :morphism;       // For IsArrow / Cod / Dom / Identity
import :pullback;       // For IsParallelPair
import :topoi;          // For IsSubobject / IsQuotient / Subobject
import :factorisation;  // For IsFactorisationSystem / IsRegularCategory /
                        // IsExactCategory — the categorical context for the
                        // (regular epi, mono) factorisation system this
                        // partition narrates (#718 Slice 2).
import :cartesian;  // For CanonicalSetCCC — Set witness site (#718 Slice 2).
import :logic;      // For Ternary — default codomain of image_of's
                    // predicate (Honest Rejection until concrete
                    // specialisation, see image_of below).

namespace dedekind::category {

/**
 * @concept IsImageOf
 * @brief @c S realises the image of arrow @c F as a Subobject of @c Cod<F>
 *        — the @b mono @b side of the (regular epi, mono) factorisation
 *        system pinned in @c :factorisation.
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
 * Categorical context (#718 Slice 2): @c IsImageOf is the @b mono @b
 * leg of the @c IsFactorisationSystem<C> pair in @c :factorisation.
 * In a regular category (@c IsRegularCategory<C>), every arrow's
 * image is a regular subobject and the factorisation is unique up to
 * iso; in @b Set and every topos this is the canonical (regular
 * epi, mono) factorisation.
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

/**
 * @section image__Factorisation_Cross_Reference
 *
 * @c IsImageOf (mono leg) + @c IsCoequalizer (epi leg) jointly realise
 * the (regular epi, mono) factorisation system named in
 * @c :factorisation as @c IsFactorisationSystem<C>.  The
 * decomposition F = m ∘ e, with e the regular epi onto Im(F) and m
 * the mono Im(F) ↪ Cod<F>, is what @c IsRegularCategory<C> witnesses
 * universally; in @b Set / @b CanonicalSetCCC and every topos this
 * factorisation is unique up to isomorphism.  The witnesses below pin
 * the canonical Set instances of the categorical chain.
 */

// ---------------------------------------------------------------------------
// Set / CanonicalSetCCC witness for the (regular epi, mono) factorisation
// system.  Set is the exemplar exact category (Borceux vol 2 §2; "in Set
// and more generally any topos, every epimorphism is the coequalizer of
// its kernel pair") — registering IsExactCategory auto-upgrades to
// IsRegularCategory and IsFactorisationSystem via the chain set up in
// :factorisation (Slice 1).
// ---------------------------------------------------------------------------

template <typename A>
inline constexpr bool is_exact_category_v<CanonicalSetCCC<A>> = true;

static_assert(IsExactCategory<CanonicalSetCCC<int>>,
              "CanonicalSetCCC<A> is exact: every equivalence relation "
              "on a Set object is the kernel pair of some arrow.");
static_assert(IsRegularCategory<CanonicalSetCCC<int>>,
              "CanonicalSetCCC<A> is regular by the IsExactCategory ⇒ "
              "IsRegularCategory upgrade.");
static_assert(IsFactorisationSystem<CanonicalSetCCC<int>>,
              "CanonicalSetCCC<A> admits the canonical (regular epi, mono) "
              "factorisation system by the IsRegularCategory ⇒ "
              "IsFactorisationSystem upgrade.");

// ---------------------------------------------------------------------------
// image_of(f) — factory producing the image of an arrow F as a Subobject
// of Cod<F>.  Sollbruchstelle for the categorical anchor; concrete
// realisations specialise the predicate for specific carrier patterns
// (e.g. Slice 4's First-Iso-Theorem needs the mod_n case to compute
// concretely on ℤ/nℤ).  Also feeds #602 (image-of-set-under-function)
// at the per-arrow-not-per-set level.
// ---------------------------------------------------------------------------

/**
 * @brief The characteristic predicate of @c image_of(F) — recognises @c y
 *        as "in the image of @c F".
 *
 * @details Default behaviour: returns @c Ternary::Unknown because the
 * existential @c ∃x ∈ Dom<F>. F(x) = y is undecidable on a general
 * @c IsArrow without further structure on @c Dom<F> (e.g.\ finite
 * enumerability, inversion, or a comprehension-DSL hook).  Concrete
 * specialisations override for specific @c F — Slice 4 lands the
 * @c mod_n case, where the image is the interval [0, n).
 *
 * @tparam F The arrow whose image this predicate classifies.
 */
export template <typename F>
  requires IsArrow<F>
struct ImageChi {
  F f;
  using Domain = Cod<F>;
  using Codomain = Ternary;

  /** @brief Default Honest-Rejection classifier: returns
   *  @c Ternary::Unknown.  Specialisations on specific @c F fire
   *  the @c True / @c False answer; until then, the image is
   *  honestly opaque. */
  constexpr Ternary operator()(const Cod<F>&) const noexcept {
    return Ternary::Unknown;
  }
};

/**
 * @brief Factory: @c image_of(f) constructs the image of @c f as a
 *        @c Subobject of @c Cod<F>.
 *
 * @details The returned @c Subobject's characteristic predicate
 * (@c ImageChi above) recognises @c y as "@c y is in the image of
 * @c f".  Pairs with @c IsImageOf at the type level: the result
 * satisfies @c IsImageOf<decltype(image_of(f)), F> for every
 * @c IsArrow @c F.  The Slice-2 realisation defers the existential
 * resolution to @c Ternary::Unknown (Honest Rejection); concrete
 * carrier-specific specialisations of @c ImageChi land in later
 * slices (Slice 4's First-Iso-Theorem exhibit).
 *
 * @tparam F The arrow whose image is being constructed.
 */
export template <typename F>
  requires IsArrow<F> && std::constructible_from<std::remove_cvref_t<F>, F&&>
constexpr auto image_of(F&& f) {
  using FT = std::remove_cvref_t<F>;
  return Subobject<Cod<FT>, ImageChi<FT>>{ImageChi<FT>{std::forward<F>(f)}};
}

// Witness: image_of(Identity<int>) is an IsSubobject of int — the trivial
// image is the whole carrier.  Pins the type signature at compile time.
static_assert(IsSubobject<decltype(image_of(Identity<int>{})), int>,
              "image_of(Identity<int>) realises a Subobject of int — the "
              "trivial image-of-identity exhibit.");

// ===========================================================================
// First Isomorphism Theorem — reusable typed surface (#718 Slice 4).
//
// Textbook statement (Burris-Sankappanavar §II.6; Birkhoff & Mac Lane
// "Algebra" §III): for every homomorphism @c f: @c A @c → @c B between
// algebras of the same signature,
//
//   @c A/ker(f)  ≅  @c im(f).
//
// We give the theorem a @b reusable @b typed surface in three pieces:
//
//   1. Two opt-in trait families that encode the textbook claim
//      "for this @c F, @c A/ker(F) IS @c X" and "for this @c F,
//      @c im(F) IS @c Y" mechanically (per-arrow @c ::Class /
//      @c ::type registrations).
//
//   2. The @c WitnessesFirstIso<F, Connector> concept, which gates
//      a user-supplied @c Connector iso against the trait registry —
//      the connector's @c Domain must match the registered quotient
//      and its @c Codomain must match the registered image.
//
//   3. Per-instance witnesses are then a short pattern: register
//      the two textbook facts, hand-roll a connector, and assert
//      @c WitnessesFirstIso<F, Connector>.  The concept does the
//      load-bearing work — a regression in @c F's quotient or image
//      that breaks the textbook claim surfaces immediately.
//
// Same shape works for @c mod_2, @c mod_3, parity-on-@c ℕ,
// projection-to-row-reduced-echelon, any concrete homomorphism a
// future slice instantiates.
// ===========================================================================

/**
 * @brief Opt-in trait: textbook claim "for this @c F, @c A/ker(F)
 *        is the carrier @c ::Class".
 *
 * @details Default is empty (no @c ::Class member); specialise on a
 *          per-@c F basis at the carrier-defining site to declare
 *          which carrier @c F's kernel-pair quotient inhabits.  The
 *          mechanical type-check happens in
 *          @c WitnessesFirstIso below, which uses the @c ::Class
 *          typedef to gate a candidate iso's @c Domain.
 *
 *          Universal-algebra reference: Burris-Sankappanavar §II.6
 *          (First Iso) / §II.5 (congruence / kernel as congruence).
 */
export template <typename F>
struct kernel_quotient {};

/**
 * @brief Opt-in trait: textbook claim "for this @c F, @c im(F)
 *        is the carrier @c ::type".
 *
 * @details Default is empty (no @c ::type member); specialise on a
 *          per-@c F basis to declare which carrier @c F's image
 *          inhabits.  Used by @c WitnessesFirstIso to gate a
 *          candidate iso's @c Codomain.  Sibling to the
 *          @c image_of(F) factory above (which produces a generic
 *          Subobject); @c image_carrier names the carrier-of-record
 *          for a specific @c F when the textbook fact is known
 *          (e.g.\ @c mod_n's image is the bounded interval
 *          @c [0, @c n)).
 */
export template <typename F>
struct image_carrier {};

/**
 * @concept WitnessesFirstIso
 * @brief @c Connector witnesses the First Isomorphism Theorem at
 *        homomorphism @c F.
 *
 * @details The typed shape of the theorem.  A user-supplied
 *          @c Connector is accepted as a First-Iso witness iff
 *
 *            (i)  it is an iso (@c IsIsomorphism<Connector>),
 *
 *            (ii) its @c Domain is exactly the registered
 *                 @c kernel_quotient<F>::Class (the textbook
 *                 quotient @c A/ker(F)),
 *
 *            (iii) its @c Codomain is exactly the registered
 *                  @c image_carrier<F>::type (the textbook
 *                  image @c im(F)).
 *
 *          A regression in any leg surfaces at compile time.  In
 *          particular, hand-rolling an iso between unrelated types
 *          and @b claiming it witnesses First-Iso is not enough —
 *          the registered quotient and image must match.
 *
 *          Form-chain row 7 (paper-§3 crown) of #718.
 *
 * @tparam F          The homomorphism @c F @c : @c A @c → @c B.
 * @tparam Connector  Candidate iso @c A/ker(F) @c → @c im(F).
 */
export template <typename F, typename Connector>
concept WitnessesFirstIso =
    IsArrow<F> && IsIsomorphism<Connector> &&
    requires {
      typename kernel_quotient<F>::Class;
      typename image_carrier<F>::type;
    } &&
    std::same_as<typename Connector::Domain,
                 typename kernel_quotient<F>::Class> &&
    std::same_as<typename Connector::Codomain, typename image_carrier<F>::type>;

}  // namespace dedekind::category
