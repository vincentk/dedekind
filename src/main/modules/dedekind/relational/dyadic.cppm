/**
 * @file dedekind/relational/dyadic.cppm
 * @partition :dyadic
 * @brief Tarski's calculus of (dyadic) relations: converse R°, the relative
 *        product R;S, union/meet, the diagonal Δ, over Set<pair> --- the BASE
 *        partition of dedekind.relational.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section dyadic__The_Base
 * Every relation in Trsk is a @c Set<std::pair<A,B>> --- a dyad.  Tarski's
 * calculus of relations (Tarski 1941, "On the Calculus of Relations") is the
 * Boolean involutive monoid on that carrier: converse @f$R^{\circ}@f$
 * (@c converse / @c SwapPred), relative product @f$R;S@f$ (@c operator>> /
 * @c ComposePred), union @f$R\cup S@f$ (the set-grammar @c | / @c Join),
 * meet @f$R\cap S@f$ (@c operator& / @c RelAnd), the diagonal @f$\Delta@f$
 * (@c diag), and the derived @c reflexive / @c symmetric closures.  The
 * reflexive-transitive closure @f$R^{*}@f$ would be the Kleene star over
 * @f$(\cup,;)@f$ --- not yet a provided operator (@c >> is Boolean-middle only;
 * FIXME(#786)).
 *
 * These combinators moved DOWN out of @c order/halfspace.cppm (#792): they are
 * pure @c Set<pair> algebra with @b no ordering, so they belong below @c order.
 * @c order keeps its projection DSL (@c π1/π2, @c ProjProj, the ordered
 * comparisons @c π1<π2, and the predicate-level meet @c && --- @c
 * structured_and → @c RelAnd, #824 --- over @c IsRelPredicate for building
 * cylinder predicates) and its relation @b witnesses, now consuming these
 * combinators via @c using @c namespace @c dedekind::relational (order imports
 * @c dedekind.relational).
 *
 * @section dyadic__Base_Of_The_Others
 * @c :graph (graphs of arrows) and @c :tables (Codd's n-ary model) both build
 * on this base: @c :dyadic → (:graph, :tables).  A graph
 * @f$\Gamma_f=\{(a,f(a))\}@f$ is a dyad; and Codd's @c natural_join
 * @f$R_1\bowtie R_2@f$ IS the relative product @f$R_1;R_2@f$ with the pivot
 * retained (one @f$\exists@f$ apart from @c >>).
 *
 * @section dyadic__Seeded_Intentions
 * Two algebraic reframings are @b seeded here as dependency + intention, not
 * yet wired (deferred to the post-architecture algebraic phase):
 *   @li FIXME(#798): re-target @c :tables' @c natural_join onto
 *       @c operator>> so Codd's ⋈ is literally Tarski's ; (tagged vs. projected
 *       relative product).
 *   @li FIXME(#799): frame n-ary products as flat @c std::tuple
 * rather than nested @c std::pair (get / apply / structured bindings for free).
 *
 * @note These @c :dyadic symbols were @c dedekind::order on the halfspace; they
 * now live in the @c dedekind::relational namespace --- the module's OWN
 * namespace, so the symbols match the module.  The carrier they operate on
 * (@c Set<pair>) stays @c dedekind::sets::Set; relations simply @b are sets of
 * pairs.  Migration impact: @c converse / @c is_relation / @c reflexive /
 * @c symmetric / @c preimage take a @c Set argument, so they @b were
 * ADL-reachable from @c dedekind::sets and now need qualification or a @c
 * using; so do the infix operators @c >> / @c & (union is the set-grammar
 * @c |).  (Only @c graph ---
 * called on an @b arrow --- and the relation/function @b concepts never
 * ADL-reached
 * @c sets.)  A consumer that wants the bare forms brings them in with @c using
 * @c namespace @c dedekind::relational once; callers that spelled
 * @c dedekind::sets::converse / @c is_relation / @c ComposePred repoint to
 * @c dedekind::relational::.
 */
module;

#include <concepts>  // std::same_as
#include <utility>   // std::pair

export module dedekind.relational:dyadic;

import dedekind.category; // IsSet, Boole
import dedekind.sets;     // Set<std::pair<...>, L, P> (:expressions)

namespace dedekind::relational {
using dedekind::sets::Set;  // relations ARE Set<pair>; the carrier stays in
                            // :sets
using dedekind::sets::𝔸;    // the declared-domain/codomain universal set

// ── The relation CORE (moved here from :sets/expressions, #792) ─────────────
// A relation is a downstream concept (category → sets → relational), so its
// TYPE and query surface belong in this module, not in :sets.  The carrier
// (@c Set<pair>) and the powerset stay in :sets; everything relation-specific
// lives here.

/**
 * @brief A Relation from A to B is a set of pairs: a subset of A × B.
 * @details ETCS reading: relations are subobjects of products.
 * @see Lambek and Scott @cite lambek1988higher
 */
export template <typename T1, typename T2, typename L, typename P>
using Relation = Set<std::pair<T1, T2>, L, P>;

/**
 * @brief A (set-level) Function is a Relation where each domain element maps
 * to exactly one codomain element.  The alias admits the same structure as a
 * Relation; functional totality and single-valuedness are enforced at the
 * call-site via witness elements.
 * @see Pierce @cite pierce1991basic
 */
export template <typename T1, typename T2, typename L, typename P>
using SetFunction = Relation<T1, T2, L, P>;

/**
 * @brief Concept: a set S whose ambient type is std::pair<T1,T2> is a valid
 *        binary relation on T1 and T2.
 *
 * @details @b A @b relation @b wears @b both @b arrow @b hats @b at @b once
 * (they are not rivals --- do not treat one as "the wrong reading"):
 *   @li as its characteristic function @f$\chi_R : A\times B \to \Omega@f$ it
 *       is a @b callable @c IsArrow (@c Domain @c = @c pair<A,B>, @c Codomain
 *       @c = @c Ω) --- this is what @c R(std::pair{a,b}) evaluates, and it is
 *       already an @c IsArrow today (the subobject-classifier reading);
 *   @li as @f$A \rightsquigarrow B@f$ it is an @b allegory @b arrow, composed
 * by the relative product @c >> and converse @c ° of this calculus. The
 * power-transpose adjunction @f$\Lambda R : A \to \mathcal{P}(B)@f$
 * (@c apply below) is the bridge: these are @b one @b datum viewed three ways.
 * A @c IsFunction (@c :graph) additionally collapses the @f$\mathcal{P}(B)@f$
 * fibre to a single value (a functional + entire relation), so it @b also reads
 * as the map @f$f : A \to B@f$ --- while remaining all of the above.
 *
 * @note This @b concept, @c IsRelation<S,T1,T2>, checks only that @c S::Domain
 * is @c pair<T1,T2> --- it does @b not encode the arrow implications above. The
 * chain @c IsLinearOperator ⟹ @c IsFunction ⟹ @c IsRelation ⟹ @c IsArrow is a
 * @b conceptual reading, reified where needed through the @b graph adapter (a
 * function's @c graph IS the @c IsRelation / @c IsFunction), @b not by direct
 * concept subsumption.  Deliberately so: @c IsRelation stays lightweight, and
 * over-constraining it (e.g.\ to @c IsArrow @c && @c IsSet @c && @c
 * IsEqualizer) would exclude valid relations and force the
 * classifier/parallel-pair arrow machinery on every use.
 *
 * The @b equalizer reading extends the same conceptual chain: a relation @c S
 * is a subobject of @c T1×T2, hence the equalizer of its classifier and @c ⊤
 * (@f$\{x \mid \chi_S(x) = \top\}@f$); a @b functional graph @f$\Gamma_f@f$ is
 * moreover the equalizer of the parallel pair @f$(f\circ\pi_A, \pi_B)@f$.  Like
 * the arrow chain, this is reified through the @b graph adapter
 * (@c category::IsEqualizer witnessed on @c graph(f) and on the affine
 * translation graph, @c algebra:halfspace_transport, #876), not baked into the
 * concept.
 */
export template <typename S, typename T1, typename T2>
concept IsRelation = requires { typename S::Domain; } &&
                     std::same_as<typename S::Domain, std::pair<T1, T2>>;

/** @brief Relation membership witness: (a,b) ∈ R. */
export template <typename T1, typename T2, typename L, typename P>
constexpr typename L::Ω relates(const Relation<T1, T2, L, P>& r, const T1& a,
                                const T2& b) {
  return r(std::pair<T1, T2>{a, b});
}

/**
 * @brief The @b declared domain of a relation @c R ⊆ A×B: the universal set
 *        @c 𝔸<A> over the first factor --- @c π₁'s codomain.
 *
 * @details A relation @b is a @c Set on the product carrier @c pair<A,B>, so
 * it @b is @c IsProduct and its projections fall out of the type: the factor
 * @b type @c A survives in @c R::Domain (@c pair<A,B>) even though the factor
 * @b set is not retained, so the @b declared domain @c 𝔸<A> is recoverable
 * total and free, with no @c ∃.  This is deliberately @b not the @b effective
 * domain @f$\{a \mid \exists b.\ R(a,b)\}@f$ (the @c π₁-image), which is a
 * separate existential carrying its own decidability certificate --- the Rice
 * wall stays quarantined to that one operation.
 */
export template <typename T1, typename T2, typename L, typename P>
constexpr auto dom(const Relation<T1, T2, L, P>&) {
  return 𝔸<T1, L>;  // preserve the relation's logic species
}

/** @brief The @b declared codomain of a relation @c R ⊆ A×B: @c 𝔸<B>, the
 *         second factor (@c π₂'s codomain).  Dual to @c dom. */
export template <typename T1, typename T2, typename L, typename P>
constexpr auto cod(const Relation<T1, T2, L, P>&) {
  return 𝔸<T2, L>;  // preserve the relation's logic species
}

/**
 * @brief Relational @b application: the image of @c x under @c R, the fibre
 *        @f$\{b \in B \mid (x,b) \in R\}@f$ as a @c Set on the codomain.
 *
 * @details This is the @b power @b transpose @f$\Lambda R : A \to
 * \mathcal{P}(B)@f$ of Bird \& de~Moor @cite birddemoor1997aop --- the relation
 * read as a set-valued map --- and it is the form on which the optimisation
 * calculus is built: @c argmax is @f$\max R \cdot \Lambda F@f$ (§3.3 / §4).
 * The @b general form.  @c R IS-A relation (and @c IsFunction refines
 * @c IsRelation), so a @b functional @c R gives a @b singleton fibre --- the
 * value @c f(x) --- and a general relation the full image; the degenerate cases
 * fall out as the fibre's @b cardinality, with no special-casing (the
 * singleton-valued specialisation is a later overload gated on the functional
 * certificate).  The fibre is @b lazy and membership-testable
 * (@c apply(R,x)(b) is @c (x,b)∈R), so it types in for any relation and the
 * existential --- is it nonempty? what is its max? --- is deferred to whoever
 * reduces it.
 *
 * @warning @b Lambda @b blackbox.  The fibre predicate is a @b capturing
 * @b lambda, so the returned @c Set carries an @b opaque, @b anonymous, @b
 * non-@c equality_comparable predicate type.  Two @c apply results are never
 * type-equal and never value-equal, and the @c Set cannot be lifted back to a
 * relation (@c as_relation @c requires @c std::equality_comparable).  Prefer a
 * @b named fibre functor with @c operator== wherever the fibre must be
 * compared, reified, or round-tripped through @c graph / @c as_relation; reach
 * for @c apply only for the throw-away, reduce-it-now case.  The
 * equality-comparable, named-fibre enumeration (the characteristic relation
 * @f$\chi : S \to 2@f$) is tracked in #840.
 */
export template <typename T1, typename T2, typename L, typename P>
constexpr auto apply(const Relation<T1, T2, L, P>& r, const T1& x) {
  auto fibre = [r, x](const T2& b) { return r(std::pair<T1, T2>{x, b}); };
  return Set<T2, L, decltype(fibre)>{fibre};
}

/**
 * @brief Point-wise single-valuedness witness for a set-function relation.
 *
 * If both y1 and y2 are related to x, they must be equal.
 */
export template <typename T1, typename T2, typename L, typename P>
constexpr typename L::Ω is_single_valued_at(const SetFunction<T1, T2, L, P>& f,
                                            const T1& x, const T2& y1,
                                            const T2& y2) {
  const auto m1 = relates(f, x, y1);
  const auto m2 = relates(f, x, y2);
  const auto both_related = L::AND(m1, m2);
  const auto equal_outputs = dedekind::category::lift_logic<L>(y1 == y2);
  // ((x,y1) ∈ f && (x,y2) ∈ f) => (y1 == y2)
  return L::OR(L::RFL(both_related), equal_outputs);
}

// ── Meet / join of relational PREDICATES (RelAnd / RelOr) ───────────────────
// Marker-preserving pointwise combinators for the predicate-level && / || on
// relpreds.  Distinct from the SET-level relation & / | (intersection / union
// of two Set<pair>): those combine whole relations, RelAnd / RelOr combine bare
// pair-predicates and stay IsRelPredicate (usable in 𝔸<pair> | relpred).
/** @brief Meet (conjunction) of two relational predicates. */
export template <typename A, typename B>
struct RelAnd {
  using is_rel_predicate = void;
  A a;
  B b;
  // @c auto (not @c bool): the result inherits the operands' own logic, so over
  // a @c Kleene relation this is the Kleene @c ∧ (a @c bool cast would
  // collapse @c Unknown).  FIXME(#780): mixed Boolean/Ternary operands (a
  // ternary relation @c & @c diag()) still need a lift on the bool side.
  template <typename P>
  constexpr auto operator()(const P& p) const {
    return a(p) && b(p);
  }
};

/** @brief Join (disjunction) of two relational PREDICATES --- the dual of
 *  @c RelAnd, and the marker-preserving carrier for the pointwise @c || on
 *  @c relpred.  Distinct from relation UNION at the SET level (@c r @c | @c s,
 *  the @c Join of two @c Set<pair>): @c RelOr composes two bare
 *  pair-PREDICATES so the result is itself an @c IsRelPredicate (usable in the
 *  comprehension @c 𝔸<pair> @c | @c relpred).  (#864 dropped the old
 *  @c operator+ spelling; this is the @c && / @c || dual re-introduced (#824)
 *  as the @c structured_or result, not a competing operator.) */
export template <typename A, typename B>
struct RelOr {
  using is_rel_predicate = void;
  A a;
  B b;
  // @c auto (not @c bool): inherit the operands' logic (Kleene @c ∨ over
  // Kleene), mirroring @c RelAnd.
  template <typename P>
  constexpr auto operator()(const P& p) const {
    return a(p) || b(p);
  }
};

// Relation UNION at the SET level is the set-grammar join @c |: two relations
// over the same product are two @c Set<pair>, so @c r @c | @c s is their
// @c Join union (@c :sets, #365).  @c ; (@c >>) is the relation product
// and @c * (closure) is @c FIXME(#786).

// ── converse and the bracket-free relation query ───────────────────────────
/** @brief The swapped predicate for @c converse: @f$R^\smile(b,a) = R(a,b)@f$.
 */
export template <typename P>
struct SwapPred {
  using is_rel_predicate = void;
  P p;
  template <typename Pair>
  constexpr auto operator()(const Pair& pr) const {
    return p(std::pair{pr.second, pr.first});
  }
};

/** @brief @c converse(R) --- the transpose @f$R^\smile \subseteq B \times A@f$
 *  of a relation @f$R \subseteq A \times B@f$ (Tarski's @f$R^\smile@f$). */
export template <typename A, typename B, typename L, typename P>
constexpr auto converse(const Set<std::pair<A, B>, L, P>& r) {
  return Set<std::pair<B, A>, L, SwapPred<P>>{SwapPred<P>{r.predicate()}};
}

/** @brief Pair-like Domain test for @c is_relation. */
template <typename D>
concept IsPairLike = requires {
  typename D::first_type;
  typename D::second_type;
};

/** @brief @c is_relation(R) --- the bracket-free query: @c R is a relation, an
 *  @c IsSet whose Domain is a product @f$A \times B@f$. */
export template <typename S>
consteval bool is_relation(const S&) {
  if constexpr (requires { typename S::Domain; })
    return dedekind::category::IsSet<S> && IsPairLike<typename S::Domain>;
  else
    return false;  // no Domain: not a set, hence not a relation (total query)
}

// ── The relative product R;S over a Boolean intermediate ────────────────────
/** @brief The composed predicate for the relative product @f$R;S@f$ over a
 *  @b Boolean intermediate, the ∃-over-the-middle enumerated on @c {false,
 *  true}. */
export template <typename PR, typename PS, typename B>
struct ComposePred {
  PR r;
  PS s;
  template <typename Pair>
  constexpr bool operator()(const Pair& ac) const {
    return (r(std::pair{ac.first, B{false}}) &&
            s(std::pair{B{false}, ac.second})) ||
           (r(std::pair{ac.first, B{true}}) &&
            s(std::pair{B{true}, ac.second}));
  }
};

/** @brief @c R @c >> @c S --- the relative product of two relations over a
 *  @b Boolean intermediate, the ∃-over-the-middle enumerated on @c {false,
 *  true}.  A larger intermediate needs the finite-quotient handle (§3.1);
 *  there is deliberately no overload, so it is an honest compile error.
 *  FIXME(#795): generalise the intermediate beyond @c bool. */
export template <typename A, typename B, typename C, typename L, typename PR,
                 typename PS>
  requires std::same_as<B, bool>
constexpr auto operator>>(const Set<std::pair<A, B>, L, PR>& r,
                          const Set<std::pair<B, C>, L, PS>& s) {
  return Set<std::pair<A, C>, L, ComposePred<PR, PS, B>>{
      ComposePred<PR, PS, B>{r.predicate(), s.predicate()}};
}

// ── Meet, the diagonal, reflexive / symmetric closures ─────────────────────
// (Union is the set-grammar @c |: see the note above @c SwapPred.)
/** @brief @c R @c & @c S --- the INTERSECTION (meet) of two relations over the
 *  same product, dual to the union @c | (@c Join): membership is both
 *  predicates (@c RelAnd).  The Boolean-lattice ∩ on relations. */
export template <typename A, typename B, typename L, typename PR, typename PS>
constexpr auto operator&(const Set<std::pair<A, B>, L, PR>& r,
                         const Set<std::pair<A, B>, L, PS>& s) {
  return Set<std::pair<A, B>, L, RelAnd<PR, PS>>{
      RelAnd<PR, PS>{r.predicate(), s.predicate()}};
}

/** @brief The equality-of-coordinates predicate for the diagonal
 *  @f$\Delta = \{(a,a)\}@f$.  A plain @c std::equality_comparable check ---
 *  the sets-level reframing of what @c order/halfspace spelled as
 *  @c ProjProj<1,Eq,2> (which needed the ordered projection DSL); the diagonal
 *  is pure equality, no ordering, so it lives here at the base. */
export template <typename A>
struct DiagPred {
  using is_rel_predicate = void;
  constexpr bool operator()(const std::pair<A, A>& p) const {
    return p.first == p.second;
  }
};

/** @brief The DIAGONAL (identity relation) @f$\Delta = \{(a,a)\}@f$ on a
 *  carrier @c A --- @c {π1==π2} --- the reflexive-closure unit and the @c 1 of
 *  the relation algebra. */
export template <typename A, typename L = dedekind::category::Boole>
constexpr auto diag() {
  return Set<std::pair<A, A>, L, DiagPred<A>>{DiagPred<A>{}};
}

/** @brief The coreflexive predicate for @f$\Delta_S = \{(x,x) \mid x \in S\}@f$
 *  --- the diagonal restricted to a unary set @c S (a @b partial identity): on
 *  the diagonal @b and in @c S.  Carries @c S by value so it stays inspectable
 *  (no lambda). */
export template <typename S>
struct CoreflexivePred {
  using is_rel_predicate = void;
  S s;
  template <typename A>
  constexpr bool operator()(const std::pair<A, A>& p) const {
    return p.first == p.second && static_cast<bool>(s(p.first));
  }
};

/** @brief @c diag(S) --- the coreflexive @f$\Delta_S = \{(x,x) \mid x \in
 *  S\}@f$, the partial identity on a unary set (Tarski's monotype).
 *  Generalizes the full diagonal (@c diag(𝔸<A>) recovers it) and is the
 *  restrictor for a relative product's endpoints, so a set pre-image is
 *  @f$\mathrm{dom}(R \mathbin{;} \mathrm{diag}(S))@f$ without leaving the
 *  point-free surface.
 *
 *  @note @b Linear-algebra @b incarnation: given a suitable domain/codomain
 *  --- a finite, indexable carrier @c A (a dimension @c D) and the logic
 *  semiring @c 𝔹 as scalar --- this coreflexive @b is a @b diagonal @b matrix
 *  @c dedekind::linear_algebra::Diagonal<D, χ_S>, whose diagonal rule is
 * exactly
 *  @c S's characteristic function (@c CoreflexivePred::s @b is the diagonal
 *  rule @c F, both point-free).  The full diagonal @c diag(𝔸<A>) is then the
 *  @c Identity<D> matrix.  So @c diag on the two surfaces is one
 * partial-identity operator.  FIXME(#873): the concrete @c coreflexive→Diagonal
 * bridge (needs the finite-carrier→dimension indexing). */
export template <typename S>
  requires dedekind::category::IsSet<S>
constexpr auto diag(const S& s) {
  using A = typename S::Domain;
  using L = typename S::logic_species;
  return Set<std::pair<A, A>, L, CoreflexivePred<S>>{CoreflexivePred<S>{s}};
}

/** @brief @c reflexive(R) = @c R @c | @c Δ --- the smallest reflexive relation
 *  containing an endorelation @c R (add the self-loops).  @c P is constrained
 * to a genuine pair-predicate (invocable on a carrier pair @f$\langle A,A
 * \rangle@f$) so a mis-typed @c R fails at the call, not deep inside the
 * union. */
export template <typename A, typename L, typename P>
  requires std::invocable<const P&, std::pair<A, A>>
constexpr auto reflexive(const Set<std::pair<A, A>, L, P>& r) {
  return r | diag<A, L>();
}

/** @brief @c symmetric(R) = @c R @c | @c R° --- the smallest symmetric relation
 *  containing @c R (add the reversed edges; @c R° is the @c converse). */
export template <typename A, typename L, typename P>
  requires std::invocable<const P&, std::pair<A, A>>
constexpr auto symmetric(const Set<std::pair<A, A>, L, P>& r) {
  return r | converse(r);
}

// ── Textbook aliases (Listing 6 spellings) ──────────────────────────────────
// Thin forwarders so callers may spell the Tarski operators by their shorthand
// or (where a valid identifier glyph exists) their blackboard symbol.  converse
// has no glyph (@c † is not an identifier), so it takes @c conv / @c dagger;
// @c diag also answers to @c Δ.
export constexpr auto conv(auto&& r) {
  return converse(std::forward<decltype(r)>(r));
}
export constexpr auto dagger(auto&& r) {
  return converse(std::forward<decltype(r)>(r));
}
export constexpr auto refl(auto&& r) {
  return reflexive(std::forward<decltype(r)>(r));
}
export constexpr auto sym(auto&& r) {
  return symmetric(std::forward<decltype(r)>(r));
}
export constexpr auto Δ(auto&& s) { return diag(std::forward<decltype(s)>(s)); }

// ── Self-contained base witnesses (no order DSL) ────────────────────────────
// The rich witnesses (≤∘≤=≤ transitivity, reflexive(<), symmetric(<)) live in
// order/halfspace, which owns the π1/π2 projection DSL and now consumes these
// combinators by ADL.  Here we witness the base laws on Δ alone
// (self-contained, no external predicate): Δ is a relation; Δ° = Δ; Δ;Δ = Δ;
// reflexive(Δ)=Δ.
static_assert(is_relation(diag<bool>()), "Δ is a relation (IsSet on ×).");
static_assert(diag<bool>()(std::pair{true, true}), "Δ contains (a,a).");
static_assert(!diag<bool>()(std::pair{true, false}), "Δ excludes (a,b≠a).");
static_assert(converse(diag<bool>())(std::pair{true, true}),
              "Δ° = Δ: the diagonal is its own converse.");
static_assert((diag<bool>() >> diag<bool>())(std::pair{true, true}),
              "Δ;Δ = Δ: the diagonal is the ; unit.");
static_assert(!(diag<bool>() >> diag<bool>())(std::pair{true, false}),
              "Δ;Δ excludes off-diagonal.");
static_assert(reflexive(diag<bool>())(std::pair{false, false}),
              "reflexive(Δ) = Δ still contains the diagonal.");
static_assert(symmetric(diag<bool>())(std::pair{true, true}),
              "symmetric(Δ) = Δ ∪ Δ° = Δ.");
// Coreflexive Δ_S = diag(S): on the diagonal AND in S.  Δ_{{true}} restricts
// to the single self-loop (true, true).
static_assert(diag(dedekind::sets::η(true))(std::pair{true, true}),
              "Δ_S contains (x,x) for x ∈ S");
static_assert(!diag(dedekind::sets::η(true))(std::pair{false, false}),
              "Δ_S excludes (x,x) for x ∉ S");
static_assert(!diag(dedekind::sets::η(true))(std::pair{true, false}),
              "Δ_S excludes off-diagonal pairs");

}  // namespace dedekind::relational

// ── Trait registry: relation-property certificates for :dyadic's predicates ──
// These are STRUCTURE-INDEPENDENT relation-algebra facts about the predicates
// defined in THIS partition (Δ = @c DiagPred, the relative product =
// @c ComposePred), so they live here: a client importing only
// @c dedekind.relational sees them, without pulling in @c order / @c algebra.
// The @c ProjProj / @c ProjAddConstProj DSL spellings keep their OWN
// certificates in @c order/halfspace + @c algebra/halfspace_transport (those
// depend on the ordered projection machinery).  Moved here in PR #797 (Copilot
// review) so relation semantics are self-contained; a dropped certificate is
// invisible to a build, so each is witnessed below (compile error on regress).
namespace dedekind::category {

// LEAF: the diagonal Δ = {(a,a)} is a TOTAL FUNCTION (a ↦ a) --- single-valued
// (right-unique) AND entire (left-total).
template <typename A, typename L>
inline constexpr bool is_right_unique_v<dedekind::sets::Set<
    std::pair<A, A>, L, dedekind::relational::DiagPred<A>>> = true;
template <typename A, typename L>
inline constexpr bool is_left_total_v<dedekind::sets::Set<
    std::pair<A, A>, L, dedekind::relational::DiagPred<A>>> = true;

// NODE: the relative product R;S propagates BOTH properties through @c >> ---
// it is functional iff both factors are, and entire iff both factors are (§3.2
// Table 3's containments composing; the retained intermediate @c B reconstructs
// the two factor relations).  Functionality was @c order-level, entireness
// @c algebra-level before the extraction; both are pure relation-algebra, so
// both live here now.
template <typename A, typename C, typename L, typename PR, typename PS,
          typename B>
inline constexpr bool is_right_unique_v<dedekind::sets::Set<
    std::pair<A, C>, L, dedekind::relational::ComposePred<PR, PS, B>>> =
    is_right_unique_v<dedekind::sets::Set<std::pair<A, B>, L, PR>> &&
    is_right_unique_v<dedekind::sets::Set<std::pair<B, C>, L, PS>>;
template <typename A, typename C, typename L, typename PR, typename PS,
          typename B>
inline constexpr bool is_left_total_v<dedekind::sets::Set<
    std::pair<A, C>, L, dedekind::relational::ComposePred<PR, PS, B>>> =
    is_left_total_v<dedekind::sets::Set<std::pair<A, B>, L, PR>> &&
    is_left_total_v<dedekind::sets::Set<std::pair<B, C>, L, PS>>;

static_assert(is_right_unique_v<decltype(dedekind::relational::diag<bool>())>,
              "Δ is FUNCTIONAL (single-valued).");
static_assert(is_left_total_v<decltype(dedekind::relational::diag<bool>())>,
              "Δ is ENTIRE (total): a ↦ a for every a.");
static_assert(is_right_unique_v<decltype(dedekind::relational::diag<bool>() >>
                                         dedekind::relational::diag<bool>())>,
              "Δ;Δ is FUNCTIONAL: the NODE rule composes through >>.");
static_assert(is_left_total_v<decltype(dedekind::relational::diag<bool>() >>
                                       dedekind::relational::diag<bool>())>,
              "Δ;Δ is ENTIRE: the NODE rule composes through >>.");
}  // namespace dedekind::category
