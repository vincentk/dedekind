/**
 * @file dedekind/relational/graph.cppm
 * @partition :graph
 * @brief The graph of a function as a relation: the analytic arrow @c f and
 *        its set of pairs are one object.  A graph @b is a dyadic (binary)
 *        relation (functional @f$R\subseteq A\times B@f$ for @f$f:A\to B@f$;
 *        an endorelation only when @f$A=B@f$), so it lives with the relations
 *        (@c dedekind.relational), not in @c :sets.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section graph__The_Graph_of_a_Function
 * By the function-as-graph definition (Bourbaki; Lawvere and Rosebrugh,
 * @em Sets @em for @em Mathematics, §2), a function @f$f : A \to B@f$ @b is
 * its graph
 * @f[ \Gamma_f = \{\, (a,b) \in A\times B \mid b = f(a) \,\} \subseteq A\times
 * B, @f] a functional (total, single-valued) binary relation.  In a Cartesian
 * closed category with a subobject classifier --- which @c IsSet reifies ---
 * the analytic arrow and its graph are the same object (currying together
 * with the classifier).  @c graph(f) exhibits that identity at the type
 * level: it lifts any @c IsArrow into the @c Set<std::pair> relation form of
 * @c :expressions, so the arrow participates mechanically in the relational
 * algebra of @c :relational.
 *
 * @section graph__The_Graph_Is_An_Equalizer
 * Categorically @f$\Gamma_f@f$ is the @b equalizer subobject of @f$A\times B@f$
 * of the parallel pair @f$(f\circ\pi_A,\ \pi_B) : A\times B \to B@f$: exactly
 * the members where the two coincide, @f$f(a)=b@f$.  Hence a graph is a
 * @c category::IsEqualizer (an @c IsSubobject of @c pair<A,B>), @b not an
 * @c IsIsomorphism arrow (its @c Set-domain is the @b product @c pair<A,B>, not
 * @c A).  The function it carries is the @b triple @c IsFunction<Graph<F>, A,
 * B>
 * @c = @c (domain @c A, @c codomain @c B, @b predicate), and the predicate slot
 * (the equalizing @f$f(a)=b@f$) is the categorically-honest home of the
 * function-ness: derived properties such as retractability and decidable
 * @c image pin on @b that predicate, not on any @c pair<A,B> @c → @c Ω view of
 * the @c Set.  Pinned concretely for the affine translation graph by
 * @c static_assert(category::IsEqualizer<...>) in @c
 * algebra:halfspace_transport
 * (#876).
 *
 * @section graph__Single_Source_Of_Truth
 * The membership predicate delegates to @c category::arrow_as_relation<F>
 * (the two-argument indicator @f$(a,b) \mapsto f(a)=b@f$, which already
 * registers the left-total / right-unique traits).  The @c Set<pair> graph
 * and the binary-relation form therefore share @b one definition of
 * @f$f(a)=b@f$ and cannot drift: a future divergence between the two
 * encodings is a compile error, not a silent fork.  @c graph generalises
 * @c sequences::as_relation (the graph of @c path.at) from a @c Path to an
 * arbitrary arrow.
 *
 * @section graph__Decidability
 * Membership @f$\Gamma_f(a,b)@f$ is decidable exactly when @c B has decidable
 * equality (@c std::equality_comparable --- the @c std::regular codomain, the
 * same carrier discipline @c IsSet roots).  Enumerating the graph, or
 * deciding that a given relation @b is a graph, additionally needs a
 * finite / enumerable domain; past that boundary the honest answer is
 * @c Unknown (Rice's theorem).  That finite witness is @c is_graph_of below:
 * a value-level pointwise check over an enumerable domain, expressed with the
 * range-generic @c forall.  It is not a type-level concept precisely because
 * deciding "this relation is that function's graph" is undecidable in general
 * (Rice); the codebase keeps single-valuedness value-level
 * (@c is_single_valued_at) for the same reason.
 *
 * @build_order after dedekind.sets (:expressions, :quantifier)
 * @dependency :category, dedekind.sets
 *
 * @see dedekind.sequences:path (@c as_relation --- the @c Path special case)
 * @see dedekind.category:cartesian (@c arrow_as_relation --- the 2-arg form)
 */
module;

#include <algorithm>    // std::ranges::all_of
#include <concepts>     // std::equality_comparable, std::same_as
#include <cstddef>      // std::size_t: the slot of a projection
#include <functional>   // std::less_equal: the inclusion order
#include <ranges>       // std::ranges::input_range, range_value_t
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::pair

export module dedekind.relational:graph;

import dedekind.category; // IsArrow, Dom, Cod, arrow_as_relation, Boole
import dedekind.sets;     // Set, Relation (:expressions)
import :dyadic;           // the Tarski BASE: a graph Γ_f IS a dyadic relation

// Namespace is dedekind::relational — the module's own namespace, so the
// symbols match the module.  A graph belongs with the relations.  The carrier
// (Set<pair>) stays dedekind::sets::Set; a graph simply IS a Set of pairs.
// See :tables.

namespace dedekind::relational {
using dedekind::sets::Comprehension;  // the relation carrier: {p ∈ 𝔸<pair> | P}
using dedekind::sets::𝔸;

/**
 * @brief The membership predicate of a functional graph @f$\Gamma_f@f$:
 *        @f$(a,b)\mapsto b = f(a)@f$.
 *
 * @details @b Named (not a lambda) so it @b carries its arrow @c f.  That is
 * what lets two graphs compose by the @b relative product without enumerating
 * the shared intermediate (@c operator>> below): @c GraphPredicate recovers the
 * arrow, so @f$\Gamma_f;\Gamma_g = \Gamma_{f;g}@f$ is available on any carrier.
 * Membership delegates to @c arrow_as_relation (one source of truth for
 * @f$b=f(a)@f$).
 */
export template <typename F>
struct GraphPredicate {
  using ArrowType = std::remove_cvref_t<F>;
  ArrowType arrow;
  using Pair =
      std::pair<typename ArrowType::Domain, typename ArrowType::Codomain>;
  constexpr bool operator()(const Pair& p) const {
    return dedekind::category::arrow_as_relation<ArrowType>{arrow}(p.first,
                                                                   p.second);
  }
};

/**
 * @brief @c Graph<F> --- the type of @c graph(f): the @c Set<pair> relation
 *        @f$\Gamma_f=\{(a,b)\mid b=f(a)\}\subseteq A\times B@f$ of @c F : A→B.
 */
export template <typename F>
using Graph =
    Comprehension<𝔸<std::pair<typename F::Domain, typename F::Codomain>,
                    dedekind::category::Boole>,
                  GraphPredicate<F>>;

/**
 * @brief @c graph(f) --- the graph of a function @c f : A → B as the @c Set
 *        of pairs @f$\{\,(a,b) \mid b = f(a)\,\} \subseteq A\times B@f$.
 *
 * @tparam F An @c IsArrow whose @c Codomain has decidable equality.
 * @param  f The analytic arrow.
 * @return A @c Graph<F> (a @c :expressions Relation) whose @c GraphPredicate<F>
 *         membership carries @c f for composition.
 *
 * @note @b Not part of the @c Trsk grammar (Listing~6), on purpose: lifting an
 *       arrow to a relation forces @c image / @c preimage to be @e inferred
 *       from @c f rather than spelled point-free, which is the source of the
 *       lambda-opacity these reifications (@c GraphPredicate, @c
 * FibrePredicate,
 *       @c PreimagePredicate) exist to contain.  Prefer building relations
 *       point-free (the restricted product @c S*T@c |@c relpred and the Tarski
 *       operators); @c graph stays available for the arrow-composition path but
 *       is kept out of the grammar surface.  Sibling of the doc-deprecated
 *       @c element scout.
 *
 * @note @b Related surfaces (#824).  The point-free arrow lift now exists as
 *       @c ap(f, π_I) → @c ProjApplyEq (the @c 𝑦 @c == @c f(𝑥) relpred) in
 *       @c :order, and image-membership decidability is read off the arrow's
 *       certificate in @c :category:image (@c ImageChi specialised for
 *       @c IsRetractableArrow → @c Boole / decidable, else the default
 *       @c Ternary::Unknown).  Redefining @c graph @b itself as sugar over
 *       @c ap (@c graph(f) @c := @c 𝔸<X>*𝔸<Y> @c | @c (𝑦==ap(f,𝑥))) is
 *       @b parked: the lift plus the decidability seam capture the value
 *       without the DSL relocation.  The group-derived mileage --- @c IsGroup
 *       ⟹ translations retractable ⟹ free decidable image --- is #875.
 *       Corrections retained from the #870 CP review: forward membership
 *       @f$b=f(a)@f$ gates on @c equality_comparable (not monicity); the
 *       pre-image capability gates on @c IsRetractableArrow / iso (monicity
 *       gives a single-valued pre-image, the retract a @b computable one).
 */
export template <typename F>
  requires dedekind::category::IsArrow<F> &&
           std::equality_comparable<typename std::remove_cvref_t<F>::Codomain>
constexpr Graph<std::remove_cvref_t<F>> graph(F f) {
  // Encode-the-pullback: membership IS arrow_as_relation's indicator (carried
  // by the named GraphPredicate), so the Set<pair> form cannot diverge from the
  // 2-arg form AND the arrow stays recoverable for composition.
  return Graph<std::remove_cvref_t<F>>{
      GraphPredicate<std::remove_cvref_t<F>>{f}};
}

}  // namespace dedekind::relational

// A graph IS a functional (single-valued) AND entire (total) relation --- it is
// the graph of a total function.  Mark both faces on the @c Graph<F> type (the
// @c Set<pair> form), the same convention @c arrow_as_relation already carries,
// so @c IsFunctional / @c IsEntire hold on @c graph(f) itself, not only on the
// 2-argument indicator.  A set over a product answers @c r(a, @c b) as well as
// @c r({a, @c b}) (#1047), so @c IsFunction<R,A,B> holds on the graph too.
// Lets the relative product below gate on @c IsFunctional at this partition's
// own level.
//
// CAVEAT: @c is_left_total_v (entireness) below is registered unconditionally,
// which assumes @c F is a @b total arrow.  @c is_right_unique_v (single-valued)
// is always honest for a graph; entireness is not, if @c F is genuinely partial
// (e.g. an arrow that @c throw s / rejects part of its domain).  No current
// caller graphs a partial arrow, so this is latent; a partial-arrow graph would
// need @c is_left_total gated on the arrow's totality (there is no general
// "arrow is total" trait yet --- FIXME if one is needed).
namespace dedekind::category {
template <typename F>
inline constexpr bool is_left_total_v<dedekind::relational::Graph<F>> = true;
template <typename F>
inline constexpr bool is_right_unique_v<dedekind::relational::Graph<F>> = true;
}  // namespace dedekind::category

namespace dedekind::relational {

// ── The relation/function lattice, defined here (above the relative product)
// so
//    the composition can gate on IsFunctional.  Fuller exposition in the
//    @section graph__The_Relation_Function_Lattice below (nLab; Freyd--Scedrov;
//    Bird--de Moor). ──────────────────────────────────────────────────────────
// FUNCTIONAL (single-valued, right-unique).
export template <typename R>
concept IsFunctional =
    dedekind::category::is_right_unique_v<std::remove_cvref_t<R>>;
// ENTIRE (total, left-total).
export template <typename R>
concept IsEntire = dedekind::category::is_left_total_v<std::remove_cvref_t<R>>;
// A FUNCTION is a relation that is functional and entire (nLab, verbatim).
// The signature is the TRIPLE (domain A, codomain B, predicate R): R is the
// equalizing predicate f(a)=b, so a functional graph IS the equalizer subobject
// of A×B of the parallel pair (f∘π_A, π_B) --- @c category::IsEqualizer, not an
// @c IsIsomorphism arrow (see the @c graph__The_Graph_Is_An_Equalizer header
// note and the witness in @c algebra:halfspace_transport, #876).
export template <typename R, typename A, typename B>
concept IsFunction = dedekind::category::IsBinaryRelation<R, A, B> &&
                     IsFunctional<R> && IsEntire<R>;

/** @brief The arrow a function's graph atom names, the converse of @c graph:
 *  a relation @c A×B @c | @c (π_2 @c == @c t) certified a function is the
 *  arrow @c a ↦ t(a), @c t read at a pair whose first coordinate is @c a.
 *  @tparam A,B the carriers.  @tparam P the graph atom. */
export template <typename A, typename B, typename P>
  requires dedekind::sets::IsGraphAtom<P> && std::default_initializable<B>
struct GraphArrow {
  using Domain = A;
  using Codomain = B;
  P atom;
  constexpr B operator()(const A& a) const {
    return dedekind::sets::term_value(dedekind::sets::graph_term(atom),
                                      std::pair<A, B>{a, B{}});
  }
};
/** @brief @c arrow_of(R): the arrow of a relation certified a function (a
 *  graph atom, functional and entire), the converse of @c graph:
 *  @c graph(arrow_of(R)) is @c R and @c arrow_of(graph(f)) is @c f.
 *  @param r the relation over @c A×B. */
export template <typename R>
  requires dedekind::sets::IsGraphAtom<std::remove_cvref_t<
               decltype(std::declval<const R&>().predicate)>> &&
           IsFunctional<R> && IsEntire<R>
constexpr auto arrow_of(const R& r) {
  using AB = typename R::Domain;
  return GraphArrow<typename AB::first_type, typename AB::second_type,
                    std::remove_cvref_t<decltype(r.predicate)>>{r.predicate};
}
/** @brief @c arrow_of(graph(f)): the graph keeps its arrow, so it is @c f
 *  itself.  @param g the graph. */
export template <typename U, typename F>
constexpr F arrow_of(const Comprehension<U, GraphPredicate<F>>& g) {
  return g.predicate.arrow;
}

/**
 * @brief Relative product of two @b functional relations (graphs), over @b any
 *        intermediate: @f$\Gamma_f\,;\,\Gamma_g = \Gamma_{f;g}@f$.
 *
 * @details The general relative product @f$(R;S)(a,c)=\exists b.\,R(a,b)\wedge
 * S(b,c)@f$ needs @f$\exists b@f$ decidable --- which is why the @b dyadic @c ;
 * (@c :dyadic) is Boolean-middle only.  Gating on @c IsFunctional (this
 * partition's own concept) is exactly the property that lifts that restriction:
 * a @b single-valued relation has @f$b=f(a)@f$ unique, so @f$\exists b@f$ is
 * @b discharged and @f$(\Gamma_f;\Gamma_g)(a,c)=(c=g(f(a)))@f$ composes
 * decidably over @f$\mathbb{Z}/\mathbb{Q}/\mathbb{R}@f$, letting arithmetic
 * arrows sit @b between inclusion relations.  This is the allegory-arrow
 * composition on the function subcategory; it @b recovers the two arrows (via
 * @c GraphPredicate) and re-graphs their composite under the @b general arrow
 * @c ∘ --- the general-carrier sibling of the translation graphs' @c >> in
 * @c :halfspace (which adds shifts).
 *
 * @note No overload clash: for a non-@c bool intermediate the dyadic @c ; (@c
 * requires @c same_as<B,bool>) is non-viable; on a @c bool middle this overload
 * is strictly more specialised (@c GraphPredicate ⊂ any @c PR); the categorical
 * arrow @c >> (@c :morphism) is non-viable on @c Set<pair> operands.
 */
export template <typename F, typename G>
  requires IsFunctional<Graph<F>> && IsFunctional<Graph<G>> &&
           std::same_as<typename F::Codomain, typename G::Domain>
constexpr auto operator>>(const Graph<F>& r, const Graph<G>& s) {
  // Compose the two underlying functions with the GENERAL arrow ∘ (qualified so
  // it is found regardless of the arrows' namespace), then re-graph f;g : A →
  // C.
  return graph(
      dedekind::category::operator>>(r.predicate.arrow, s.predicate.arrow));
}

/**
 * @brief The preimage predicate @f$a \mapsto f(a)\in S@f$: the subobject @c S
 *        pulled back along the functional arrow @c f (@f$\chi_S\circ f@f$).
 *
 * @details @b Named (not a lambda) so it carries both the arrow @c f and the
 * codomain set @c S --- the reified @f$\chi_S\circ f@f$, the sibling of
 * @c GraphPredicate for the pullback-of-a-subobject reading of §4.
 */
export template <typename F, typename S>
struct PreimagePredicate {
  using ArrowType = std::remove_cvref_t<F>;
  using SetType = std::remove_cvref_t<S>;
  ArrowType arrow;
  SetType set;
  // Returns the CLASSIFIER value χ_S(f(a)), not a bool: a bool collapse
  // (== True) would drop @c Ternary::Unknown to false even though the result
  // Set advertises this logic species.  Preserving @c L::Ω keeps the pullback
  // of a ternary predicate honest.
  constexpr typename SetType::logic_species::Ω operator()(
      const typename ArrowType::Domain& a) const {
    return set(arrow(a));  // χ_S ∘ f, classifier value preserved
  }
};

/**
 * @brief @c preimage(f, S) = @f$\{\,a \mid f(a)\in S\,\}@f$ --- the subobject
 *        @c S ⊆ B pulled back along @c f : A → B, i.e.\ @c S's classifier
 *        composed with @c f (@f$\chi_S\circ f@f$; every mono is a pullback of
 *        @c true, so a preimage IS such a pullback).
 *
 * @details The DEFAULT preimage: apply-and-test, decided by single-valuedness
 * alone (an @c IsArrow is functional), so it reads the @f$\pi_A@f$ side and
 * never leaves the domain (§4, the relation-quantifier table).  The @b domain
 * @b type is the clip: the predicate ranges over @c A, so a bound outside @c
 * A's range (e.g.\ @c {≤−3} pulled to @c unsigned) is @c ∅ by construction ---
 * there is no out-of-range witness to test, and no negative-pivot halfspace is
 * ever built.  No enumeration: membership is the composite @f$\chi_S\circ f@f$,
 * a point-free apply-and-test.
 *
 * Closed-form specialisations refine this where a TYPED result adds
 * collapse-visible structure: the bound-MOVING affine maps
 * (@c :halfspace_transport translate/scale, the contravariant inverse of
 * @c image) --- certified to AGREE with this default pointwise.  A
 * bound-PRESERVING residue-class sibling (a @c Congruence rotation) is the
 * @c morphologies analogue (@c FIXME(#797)).
 *
 * @c S must be a SET over @c f's codomain: constrained to a set-shaped argument
 * (one exposing @c logic_species) whose classifier accepts @c F::Codomain, so
 * the documented @f$S \subseteq B@f$ contract is checked during overload
 * resolution --- a bare callable without @c logic_species, or one over an
 * unrelated ambient, is rejected here rather than hard-erroring in the body.
 */
export template <typename F, typename S>
  requires dedekind::category::IsArrow<F> &&
           // F must be an ANALYTIC ARROW, not a relation-valued Set.  A
           // relation Set<pair<A,B>, L, P> is itself IsArrow (an arrow to
           // bool), so without this a non-viable graph-specific overload (e.g.
           // an unsigned translation graph with Congruence<3u,2u>, N not a
           // power of two) would fall through here and pull S back through the
           // RELATION read as pair→bool, silently returning a set of pairs.
           // Exclude IsSet (a graph is IsSet; a Morphism arrow is not).
           (!dedekind::category::IsLSet<std::remove_cvref_t<F>>) &&
           // (PreimagePredicate::operator() const invokes the stored arrow;
           // its const-invocability is now guaranteed by IsArrow itself, #822,
           // so no separate const-call guard is needed here.)
           requires(const S& s,
                    const typename std::remove_cvref_t<F>::Codomain& b) {
             typename std::remove_cvref_t<S>::logic_species;
             {
               s(b)
             }
             -> std::same_as<typename std::remove_cvref_t<S>::logic_species::Ω>;
           }
constexpr auto preimage(F f, S s) {
  using Arr = std::remove_cvref_t<F>;
  using St = std::remove_cvref_t<S>;
  using A = typename Arr::Domain;
  return Comprehension<𝔸<A, typename St::logic_species>,
                       PreimagePredicate<Arr, St>>{
      PreimagePredicate<Arr, St>{f, s}};
}

/** @section graph__Hyperdoctrine
 *  Quantifiers are the adjoints of substitution (Lawvere).  For an arrow
 *  @f$f : A \to B@f$ the three operations on subobjects
 *  @f[ \exists_f \dashv f^{-1} \dashv \forall_f, \qquad
 *      \mathrm{Sub}(A, L) \rightleftarrows \mathrm{Sub}(B, L) @f]
 *  are arrows between the objects @c Subobjects (@c sets): the pushforward
 *  @c Image, the pullback @c Preimage (substitution, @c preimage above), and
 *  the dual image @c Dual @f$= \neg\,\exists_f\,\neg@f$, a composite of
 *  three arrows (@c category::Compose at a presented object).  Both
 *  adjunctions are witnessed through @c category::IsGaloisConnection, the
 *  arrows carrying their certificates.  The pullback is total; the images are
 *  @b partial, defined where the @f$\exists@f$ is eliminated --- the Rice wall
 *  of the effective domain (@c dom above) stated as a partial arrow:
 *  along an injection (the sum's pushforward, @c sets::Inject), along the
 *  diagonal (@c diag, the coreflexive), and along a projection whose other
 *  factor is a finite chain (Shannon's expansion, the join of the
 *  substitutions at each value: the typed twin of the run-time
 *  @c ChainValues).  The certificates (monotone, adjoint) are stated once for
 *  the whole family, as specialisations of @c category::is_monotone /
 *  @c is_adjoint on its pattern: theorems, not tags.  Frobenius reciprocity and
 * Beck--Chevalley (#909) are then equations between these arrows, checked
 * below. */

/** @brief The arrow that fixes one factor of a pair: @f$x \mapsto (c, x)@f$
 *  (@c K = 1) or @f$x \mapsto (x, c)@f$ (@c K = 2).  Substitution along it is
 *  one case of Shannon's expansion.  @tparam K the slot the constant takes.
 *  @tparam C the constant's carrier.  @tparam c the constant.  @tparam T the
 *  free factor. */
export template <std::size_t K, typename C, C c, typename T>
  requires(K == 1 || K == 2)
struct Fixed {
  using Domain = T;
  using Codomain = std::conditional_t<K == 1, std::pair<C, T>, std::pair<T, C>>;
  constexpr Codomain operator()(const T& x) const {
    if constexpr (K == 1)
      return Codomain{c, x};
    else
      return Codomain{x, c};
  }
};

namespace detail_hyperdoctrine {
/** @brief @f$\exists c \in C.\ R(\ldots c \ldots)@f$ over a finite chain:
 *  the join of the substitutions at every value, from @c c upward. */
template <std::size_t K, typename C, C c, typename T, typename R>
constexpr auto shannon(const R& r) {
  // Qualified: category::preimage (the Compose form) is reachable by ADL too.
  const auto here = dedekind::relational::preimage(Fixed<K, C, c, T>{}, r);
  if constexpr (c == dedekind::sets::chain_top<C>())
    return here;
  else
    return here | shannon<K, C, dedekind::category::Successor<C>{}(c), T>(r);
}
/** @brief Whether @c F is the projection @f$\pi_K@f$ out of a pair. */
template <typename F, std::size_t K>
concept IsPairProjection = dedekind::category::IsArrow<F> && requires {
  typename dedekind::category::Dom<F>::first_type;
  typename dedekind::category::Dom<F>::second_type;
} && std::same_as<F, dedekind::category::Π<K, dedekind::category::Dom<F>>>;
}  // namespace detail_hyperdoctrine

/** @brief @f$\exists_\iota@f$ along an injection: the sum's pushforward. */
export template <std::size_t K, typename X, typename Y,
                 dedekind::sets::IsSetObject S>
constexpr auto image_along(const dedekind::category::Injection<K, X, Y>&,
                           const S& s) {
  using L = typename S::logic_species;
  return dedekind::sets::Inject<K, 𝔸<X, L>, 𝔸<Y, L>>{}(s);
}
/** @brief @f$\exists_\Delta@f$ along the diagonal: the coreflexive
 *  @f$\Delta_S = \{(x, x) \mid x \in S\}@f$, @c diag. */
export template <typename X, dedekind::sets::IsSetObject S>
constexpr auto image_along(const dedekind::category::Copy<X>&, const S& s) {
  return dedekind::relational::diag(s);
}
/** @brief @f$\exists_{\pi_1}@f$, the second factor a finite chain: Shannon. */
export template <detail_hyperdoctrine::IsPairProjection<1> F,
                 dedekind::sets::IsSetObject R>
  requires dedekind::category::IsFiniteChain<
               typename dedekind::category::Dom<F>::second_type> &&
           dedekind::category::HasNNOStep<
               typename dedekind::category::Dom<F>::second_type>
constexpr auto image_along(const F&, const R& r) {
  using B = typename dedekind::category::Dom<F>::second_type;
  using A = typename dedekind::category::Dom<F>::first_type;
  return detail_hyperdoctrine::shannon<2, B, dedekind::sets::chain_bottom<B>(),
                                       A>(r);
}
/** @brief @f$\exists_{\pi_2}@f$, the first factor a finite chain: Shannon. */
export template <detail_hyperdoctrine::IsPairProjection<2> F,
                 dedekind::sets::IsSetObject R>
  requires dedekind::category::IsFiniteChain<
               typename dedekind::category::Dom<F>::first_type> &&
           dedekind::category::HasNNOStep<
               typename dedekind::category::Dom<F>::first_type>
constexpr auto image_along(const F&, const R& r) {
  using A = typename dedekind::category::Dom<F>::first_type;
  using B = typename dedekind::category::Dom<F>::second_type;
  return detail_hyperdoctrine::shannon<1, A, dedekind::sets::chain_bottom<A>(),
                                       B>(r);
}

export template <dedekind::category::IsArrow F,
                 dedekind::category::IsOckhamAlgebra L =
                     dedekind::category::Boole>
  requires std::default_initializable<F>
struct Preimage;
export template <dedekind::category::IsArrow F,
                 dedekind::category::IsOckhamAlgebra L =
                     dedekind::category::Boole>
  requires std::default_initializable<F>
struct Dual;

/** @brief @f$\exists_f : \mathrm{Sub}(A, L) \to \mathrm{Sub}(B, L)@f$, the
 *  image along @f$f : A \to B@f$: @f$a \mapsto f[a] = \{f(x) \mid x \in a\}@f$.
 *  Partial: defined where @c image_along eliminates the @f$\exists@f$.
 *  Monotone; the left adjoint of @c Preimage.  @tparam F the arrow, stateless.
 *  @tparam L the logic. */
export template <dedekind::category::IsArrow F,
                 dedekind::category::IsOckhamAlgebra L =
                     dedekind::category::Boole>
  requires std::default_initializable<F>
struct Image {
  using Domain = dedekind::sets::Subobjects<dedekind::category::Dom<F>, L>;
  using Codomain = dedekind::sets::Subobjects<dedekind::category::Cod<F>, L>;
  template <typename S>
    requires dedekind::category::Presents<S, Domain> &&
             requires(const S& s) { image_along(F{}, s); }
  constexpr auto operator()(const S& s) const {
    return image_along(F{}, s);
  }
};
/** @brief @f$f^{-1} : \mathrm{Sub}(B, L) \to \mathrm{Sub}(A, L)@f$,
 *  substitution along @f$f@f$: @f$b \mapsto \{x \mid f(x) \in b\}@f$
 *  (@c preimage, @f$\chi_b \circ f@f$).  Total, a lattice homomorphism,
 *  monotone; the right adjoint of @c Image and the left adjoint of @c Dual.
 *  @tparam F the arrow, stateless.  @tparam L the logic. */
export template <dedekind::category::IsArrow F,
                 dedekind::category::IsOckhamAlgebra L>
  requires std::default_initializable<F>
struct Preimage {
  using Domain = dedekind::sets::Subobjects<dedekind::category::Cod<F>, L>;
  using Codomain = dedekind::sets::Subobjects<dedekind::category::Dom<F>, L>;
  template <typename S>
    requires dedekind::category::Presents<S, Domain>
  constexpr auto operator()(const S& s) const {
    return dedekind::relational::preimage(F{}, s);
  }
};
/** @brief @f$\forall_f : \mathrm{Sub}(A, L) \to \mathrm{Sub}(B, L)@f$, the
 *  dual image: @f$a \mapsto \{y \mid \forall x.\ f(x) = y \Rightarrow x \in
 *  a\}@f$, which is @f$\neg\,\exists_f\,\neg@f$ (De Morgan: the logic is an
 *  Ockham algebra), a composite of three arrows on presented objects.  The
 *  right adjoint of @c Preimage; monotone, as anti∘co∘anti.  @tparam F the
 *  arrow, stateless.  @tparam L the logic. */
export template <dedekind::category::IsArrow F,
                 dedekind::category::IsOckhamAlgebra L>
  requires std::default_initializable<F>
struct Dual {
  using Domain = dedekind::sets::Subobjects<dedekind::category::Dom<F>, L>;
  using Codomain = dedekind::sets::Subobjects<dedekind::category::Cod<F>, L>;
  using Composite = dedekind::category::Compose<
      dedekind::category::Compose<
          dedekind::sets::Complement<dedekind::category::Dom<F>, L>,
          Image<F, L>>,
      dedekind::sets::Complement<dedekind::category::Cod<F>, L>>;
  template <typename S>
    requires dedekind::category::Presents<S, Domain> &&
             requires(const S& s) { Composite{}(s); }
  constexpr auto operator()(const S& s) const {
    return Composite{}(s);
  }
};

}  // namespace dedekind::relational
namespace dedekind::category {
/** @brief The hyperdoctrine, stated once for every arrow @c F: the three
 *  operations preserve inclusion, and @f$\exists_f \dashv f^{-1} \dashv
 *  \forall_f@f$ --- @f$f[a] \subseteq b \iff a \subseteq f^{-1}(b)@f$ and
 *  @f$f^{-1}(b) \subseteq a \iff b \subseteq \forall_f a@f$ (Lawvere; in
 *  Set, for every @f$f@f$). */
template <typename F, typename L>
struct is_monotone<dedekind::relational::Image<F, L>, std::less_equal<>>
    : std::true_type {};
template <typename F, typename L>
struct is_monotone<dedekind::relational::Preimage<F, L>, std::less_equal<>>
    : std::true_type {};
template <typename F, typename L>
struct is_monotone<dedekind::relational::Dual<F, L>, std::less_equal<>>
    : std::true_type {};
template <typename F, typename L>
struct is_adjoint<dedekind::relational::Image<F, L>,
                  dedekind::relational::Preimage<F, L>, std::less_equal<>>
    : std::true_type {};
template <typename F, typename L>
struct is_adjoint<dedekind::relational::Preimage<F, L>,
                  dedekind::relational::Dual<F, L>, std::less_equal<>>
    : std::true_type {};
}  // namespace dedekind::category
namespace dedekind::relational {
namespace detail_hyperdoctrine_witness {
using dedekind::category::Copy;
using dedekind::category::Injection;
using dedekind::category::IsGaloisConnection;
using dedekind::category::Π;
using dedekind::sets::Inject;
using dedekind::sets::Restrict;
using dedekind::sets::π;
// An arrow 𝔹 → int and its graph {(⊥, 3), (⊤, 7)} ⊆ 𝔹 × int.
struct Code {
  using Domain = bool;
  using Codomain = int;
  constexpr int operator()(const bool& b) const { return b ? 7 : 3; }
};
struct Seven {
  using Domain = bool;
  using Codomain = int;
  constexpr int operator()(const bool&) const { return 7; }
};
inline constexpr auto R = graph(Code{});
inline constexpr auto three = 𝔸<int>{} & (π == 3);
using P2 = Π<2, std::pair<bool, int>>;
using Ex = Image<P2>;
using Sb = Preimage<P2>;
using Al = Dual<P2>;
static_assert(IsGaloisConnection<Ex, Sb> && IsGaloisConnection<Sb, Al>,
              "∃_π ⊣ π* ⊣ ∀_π over 𝔹 × int: the hyperdoctrine triple, "
              "through IsGaloisConnection");
static_assert(Ex{}(R)(7) && Ex{}(R)(3) && !Ex{}(R)(5),
              "∃b. (b, n) ∈ R: the image of Code, {3, 7}, by Shannon over 𝔹");
static_assert(!Al{}(R)(7) && Al{}(graph(Seven{}))(7) &&
                  !Al{}(graph(Seven{}))(3),
              "∀b. (b, n) ∈ R: a constant arrow's value, nothing else");
static_assert(Sb{}(three)(std::pair{true, 3}) &&
                  !Sb{}(three)(std::pair{true, 5}),
              "π*a is the cylinder over a");
// Frobenius reciprocity: ∃_f(R ∧ f*a) = ∃_f R ∧ a, at points.
static_assert(Ex{}(R& Sb{}(three))(3) && !Ex{}(R & Sb{}(three))(7) &&
                  (Ex{}(R)&three)(3) && !(Ex{}(R)&three)(7),
              "Frobenius reciprocity ∃(R ∧ π*a) = ∃R ∧ a");
// The diagonal: diag ⊣ Δ⁻¹ (#6 of the sum's follow-ups; #873).
using Dg = Image<Copy<int>>;
using Dr = Preimage<Copy<int>>;
static_assert(IsGaloisConnection<Dg, Dr>,
              "diag ⊣ Δ⁻¹: a ↦ Δ_a is left adjoint "
              "to R ↦ {x | (x, x) ∈ R}");
static_assert(Dg{}(three)(std::pair{3, 3}) && !Dg{}(three)(std::pair{3, 4}) &&
                  Dr{}(Dg{}(three))(3) && !Dr{}(Dg{}(three))(4),
              "Δ_a is the coreflexive of a; its diagonal restriction is a");
static_assert(Image<Copy<int>, dedekind::category::Kleene>{}(
                  dedekind::sets::Ø<int, dedekind::category::Kleene>{})(
                  std::pair{0, 0}) == dedekind::category::Ternary::False,
              "a Kleene diagonal image keeps its logic: Ø's is False, not a "
              "bool-cast −1");
// The injection: the general ∃_ι ⊣ ι⁻¹ agrees with the sum's own arrows.
using I1 = Injection<1, bool, int>;
using Ix = Image<I1>;
using Ip = Preimage<I1>;
inline constexpr auto t = 𝔸<bool>{} & (π == true);
static_assert(IsGaloisConnection<Ix, Ip>, "∃_ι ⊣ ι⁻¹ through Image / Preimage");
static_assert(Ip{}(Ix{}(t))(true) && !Ip{}(Ix{}(t))(false) &&
                  Restrict<1, 𝔸<bool>, 𝔸<int>>{}(Ix{}(t))(true),
              "ι⁻¹ ι[a] = a: Preimage at points, Restrict by type (the "
              "reduced form)");
// Beck–Chevalley (#909) on the squares of the sum: the two injections' square
// has the empty pullback (coproducts are disjoint), so ι₂* ∃_{ι₁} = ∃_Ø Ø* = Ø,
// by type; one injection's square with itself has pullback the summand, so
// ι₁* ∃_{ι₁} = id (the unit, witnessed in sets).
static_assert(
    std::same_as<std::remove_cvref_t<decltype(Restrict<2, 𝔸<bool>, 𝔸<int>>{}(
                     Inject<1, 𝔸<bool>, 𝔸<int>>{}(t)))>,
                 dedekind::sets::Ø<int>>,
    "Beck–Chevalley on the disjointness square: ι₂* ∃_{ι₁} a = Ø, by type");
}  // namespace detail_hyperdoctrine_witness

/** @section graph__Formal_Verification */

// The graph of the identity id : int → int is the diagonal {(n, n)}.
inline constexpr auto Γ_id = graph(dedekind::category::Identity<int>{});

// It is an ETCS set object (a functional relation, a subobject of int × int).
static_assert(dedekind::category::IsSet<decltype(Γ_id)>,
              "graph(f) is an ETCS set object (a functional relation on A×B).");

// Membership is b == f(a): the diagonal holds, off the diagonal does not.
static_assert(Γ_id(std::pair{5, 5}),
              "(5,5) lies on the graph of the identity.");
static_assert(!Γ_id(std::pair{5, 6}),
              "(5,6) does not lie on the graph of the identity.");

// Drift-detector for the pullback: graph(f) membership IS
// arrow_as_relation<F>'s indicator, by construction.  This cannot fail
// unless the delegation above is broken --- which is the point.
static_assert(
    Γ_id(std::pair{7, 7}) ==
        dedekind::category::arrow_as_relation<
            dedekind::category::Identity<int>>{
            dedekind::category::Identity<int>{}}(7, 7),
    "graph(f) membership must agree with arrow_as_relation<F> pointwise.");

// ── Functional relative product: Γ_f ; Γ_g = Γ_{f;g}, over a NON-Boolean (int)
//    intermediate — decidable because each graph is single-valued (∃b = f(a)).
//    ─
namespace graph_compose_witness {
inline constexpr auto dbl =
    dedekind::category::arrow<int, int>([](const int& n) { return 2 * n; });
inline constexpr auto inc =
    dedekind::category::arrow<int, int>([](const int& n) { return n + 1; });
// (Γ_dbl ; Γ_inc)(a, c) ⟺ c == inc(dbl(a)) == 2a+1 — the ∃b discharged over ℤ.
inline constexpr auto Γ_dbl_inc = graph(dbl) >> graph(inc);
static_assert(
    dedekind::category::IsSet<decltype(Γ_dbl_inc)>,
    "the relative product of two graphs is again a functional graph.");
static_assert(Γ_dbl_inc(std::pair{3, 7}), "(3, 7) ∈ Γ_dbl;Γ_inc  (2·3+1 = 7).");
static_assert(!Γ_dbl_inc(std::pair{3, 6}), "(3, 6) ∉ Γ_dbl;Γ_inc.");
// Drift-detector: Γ_f ; Γ_g agrees pointwise with the graph of the composite.
static_assert(Γ_dbl_inc(std::pair{5, 11}) ==
                  graph(dbl >> inc)(std::pair{5, 11}),
              "Γ_f;Γ_g == Γ_{f;g} pointwise (relative product = graph of ∘).");
// graph(f) itself is now IsFunctional AND IsEntire (not only the 2-arg
// arrow_as_relation) --- the single-valuedness the relative product gates on.
// (IsFunction<R,A,B> stays the 2-arg reading on arrow_as_relation; a Set<pair>
// graph carries the 1-arg faces IsFunctional / IsEntire.)
static_assert(IsFunctional<decltype(graph(inc))>, "graph(f) is functional.");
static_assert(IsEntire<decltype(graph(inc))>, "graph(f) is entire (total).");
static_assert(IsFunctional<std::remove_cvref_t<decltype(Γ_dbl_inc)>> &&
                  IsEntire<std::remove_cvref_t<decltype(Γ_dbl_inc)>>,
              "the relative product Γ_f;Γ_g is again functional + entire.");
}  // namespace graph_compose_witness

// A set IS a function: its χ : C → Ω_L is an arrow (Domain, Codomain, a const
// call), and an arrow's two-argument relation is left-total and right-unique.
// So Set[C] implies Function[C, L] with the set itself as the arrow.
namespace set_is_function_witness {
using dedekind::sets::π;
inline constexpr auto five = dedekind::sets::𝔸<unsigned>{} & (π == 5u);
using Five = std::remove_cvref_t<decltype(five)>;
static_assert(
    dedekind::category::IsArrow<Five> &&
        IsFunction<dedekind::category::arrow_as_relation<Five>, unsigned, bool>,
    "a set is a function into its truth chain: χ : C → Ω_L");
static_assert(dedekind::category::arrow_as_relation<Five>{five}(5u, true) &&
                  dedekind::category::arrow_as_relation<Five>{five}(6u, false),
              "its graph: (5, ⊤), (6, ⊥)");
}  // namespace set_is_function_witness

/**
 * @section graph__The_Relation_Function_Lattice
 * The standard characterisation (nLab; Freyd and Scedrov, @em Categories,
 * @em Allegories; Bird and de Moor, @em Algebra @em of @em Programming): a
 * relation is @b functional (single-valued / right-unique) and @b entire
 * (total / left-total), and @em a @em function @em is @em precisely @em a
 * @em relation @em that @em is @em both @em functional @em and @em entire.
 * These property faces are thin aliases over the @c :cartesian opt-in traits
 * (one source of truth), so @c IsFunction is definitionally the same claim as
 * @c category::IsBinaryFunction and cannot drift from it.  Because
 * @c IsFunction @b refines @c IsBinaryRelation, the inclusion
 * @f$\{\text{functions}\} \subset \{\text{relations}\}@f$ is genuine C++
 * concept subsumption: a function type-checks anywhere a relation is required.
 * (@c IsFunctional / @c IsEntire / @c IsFunction are defined @b above, ahead of
 * the relative product that gates on them; this section is their exposition.)
 */

// IsGraph IS A IsRelation: the Bourbaki / graph-theory vocabulary as a
// subsuming synonym of the (nLab-canonical) IsRelation base, so "graph" stays
// available for the §6 "a graph is a relation" reading without a second
// definition to drift.
export template <typename S, typename T1, typename T2>
concept IsGraph = IsRelation<S, T1, T2>;  // IsRelation is native (:dyadic)

/** @section graph__Formal_Verification_Lattice */

// A general relation that is NOT a function: b % a == 0 relates 1 to every b,
// so it is not single-valued.  It is a relation but not functional/entire.
namespace {
struct Divides {
  constexpr bool operator()(int a, int b) const { return a != 0 && b % a == 0; }
};
}  // namespace

// The IsFunction certificate (functional ∧ entire, by construction) holds on
// both presentations of one function: the 2-argument arrow_as_relation<F>, and
// graph(f), the Set<pair> reification, which answers r(a, b) as r({a, b})
// (#1047: the product's universal property).
static_assert(
    IsFunction<dedekind::category::arrow_as_relation<
                   dedekind::category::Identity<int>>,
               int, int> &&
        IsFunction<decltype(Γ_id), int, int>,
    "id is a function on both presentations: arrow_as_relation and graph(id)");
static_assert(Γ_id(3, 3) && !Γ_id(3, 4),
              "graph(id) answers r(a, b) as r({a, b})");

// Subtyping witness: {function} ⊂ {relation}.  A function IS A relation...
static_assert(
    dedekind::category::IsBinaryRelation<dedekind::category::arrow_as_relation<
                                             dedekind::category::Identity<int>>,
                                         int, int>,
    "every function is a relation (IsFunction refines IsBinaryRelation).");

// ...but the inclusion is strict: Divides is a relation, not a function.
static_assert(dedekind::category::IsBinaryRelation<Divides, int, int>,
              "Divides is a binary relation on int × int.");
static_assert(!IsFunction<Divides, int, int>,
              "Divides is a relation but NOT a function (not single-valued).");

// Pullback / drift-detector: IsFunction (functional ∧ entire) is exactly
// category::IsBinaryFunction.  Spelled independently here, so a future change
// to either definition that broke the equivalence would fail this witness.
static_assert(
    IsFunction<dedekind::category::arrow_as_relation<
                   dedekind::category::Identity<int>>,
               int, int> ==
        dedekind::category::IsBinaryFunction<
            dedekind::category::arrow_as_relation<
                dedekind::category::Identity<int>>,
            int, int>,
    "IsFunction (functional ∧ entire) must coincide with IsBinaryFunction.");

// graph(f) participates as a relation (IsGraph = IsRelation) on its pair type.
static_assert(IsGraph<decltype(Γ_id), int, int>,
              "graph(f) is a relation (a Set of pairs on A × B).");

namespace detail_graph {
/** @brief Whether @c r and the graph @c g agree on the pair @c (a, b). */
template <typename R, typename G, typename A>
struct AgreesAt {
  const R& r;
  const G& g;
  const A& a;
  template <typename B>
  constexpr bool operator()(const B& b) const {
    return static_cast<bool>(r(std::pair{a, b})) ==
           static_cast<bool>(g(std::pair{a, b}));
  }
};
/** @brief Whether @c r and the graph @c g agree on every pair of the row
 *  @c a: ∀ b ∈ cod. */
template <typename R, typename G, typename CodRange>
struct AgreesOnRow {
  const R& r;
  G g;
  const CodRange& cod;
  template <typename A>
  constexpr bool operator()(const A& a) const {
    return std::ranges::all_of(cod, AgreesAt<R, G, A>{r, g, a});
  }
};
}  // namespace detail_graph

/**
 * @brief @c is_graph_of(r, f, dom) --- the finite witness that a relation
 *        @c r @b is the graph of the arrow @c f, by pointwise agreement with
 *        @c graph(f) over the enumerable domain @c dom.
 *
 * @details The mechanical form of "the compiler knows @c r and @c f are one
 * and the same function": over the finite @c dom it compares @c r against the
 * canonical @c graph(f) at every @c (a,b) in @c dom × dom, via the
 * range-generic @c forall (the ¬∃¬ set-operation over a domain).  Value-level,
 * not a concept: deciding graph-equality of two independently given
 * presentations is
 * undecidable in general (Rice), so the honest witness is bounded to an
 * enumerable domain.  (When @c r @b is @c graph(f) by construction no check is
 * needed --- provenance is the proof; this is for an @c r obtained
 * independently, e.g.\ an enumerated / materialised list.)  The domain is a
 * plain @c std::ranges::input_range of the arrow's @c Domain --- the general
 * concept, reached through the @c std iterator interface, not a container.
 *
 * @tparam R        A relation callable on @c std::pair.
 * @tparam F        An @c IsArrow.
 * @tparam DomRange A @c std::ranges::input_range of the arrow's @c Domain.
 * @tparam CodRange A @c std::ranges::input_range of the arrow's @c Codomain ---
 *                  the second factor of @c A × B, distinct from @c dom whenever
 *                  @c Domain and @c Codomain differ.
 */
export template <typename R, typename F, std::ranges::input_range DomRange,
                 std::ranges::input_range CodRange>
  requires dedekind::category::IsArrow<F> &&
           std::same_as<std::ranges::range_value_t<DomRange>,
                        dedekind::category::Dom<F>> &&
           std::same_as<std::ranges::range_value_t<CodRange>,
                        dedekind::category::Cod<F>>
constexpr auto is_graph_of(const R& r, F f, const DomRange& dom,
                           const CodRange& cod) {
  return std::ranges::all_of(
      dom, detail_graph::AgreesOnRow<R, decltype(graph(f)), CodRange>{
               r, graph(f), cod});
}

/** @section graph__Formal_Verification_IsGraphOf */

// The compiler knows graph(id) IS the graph of id over the finite domain [0,4).
static_assert(is_graph_of(Γ_id, dedekind::category::Identity<int>{},
                          std::views::iota(0, 4), std::views::iota(0, 4)),
              "graph(id) is the graph of the identity on [0,4).");

// ...and distinguishes it from a different function: graph(succ) is NOT the
// graph of id.  The witness genuinely decides equality on the finite sample.
static_assert(!is_graph_of(graph(dedekind::category::Successor<int>{}),
                           dedekind::category::Identity<int>{},
                           std::views::iota(0, 4), std::views::iota(0, 4)),
              "graph(succ) differs from the graph of id on [0,4).");

}  // namespace dedekind::relational
