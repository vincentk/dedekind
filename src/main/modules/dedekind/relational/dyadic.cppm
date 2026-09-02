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
 * @c ComposePred), union @f$R\cup S@f$ (@c operator+ / @c RelOr), meet
 * @f$R\cap S@f$ (@c operator& / @c RelAnd), the diagonal @f$\Delta@f$
 * (@c diagonal), and the derived @c reflexive / @c symmetric closures.  The
 * reflexive-transitive closure @f$R^{*}@f$ is the Kleene star over @f$(+,;)@f$.
 *
 * These combinators moved DOWN out of @c order/halfspace.cppm (#792): they are
 * pure @c Set<pair> algebra with @b no ordering, so they belong below @c order.
 * @c order keeps its projection DSL (@c π1/π2, @c ProjProj, the ordered
 * comparisons @c π1<π2, and the predicate-level @c operator& / @c
 * IsRelPredicate for building cylinder predicates) and its relation @b
 * witnesses, now consuming these combinators by ADL on their @c
 * dedekind::sets::Set arguments (order imports @c dedekind.relational).
 *
 * @section dyadic__Base_Of_The_Others
 * @c :graph (endorelations) and @c :tables (Codd's n-ary model) both build on
 * this base: @c :dyadic → (:graph, :tables).  A graph
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
 * @note Namespace stays @c dedekind::sets (ADL on Set/Relation arguments; the
 * unqualified operators @c >> / @c + / @c & require it).  Only the module
 * boundary moved from @c order to @c relational (the transport-op precedent,
 * #785).
 */
module;

#include <concepts>  // std::same_as
#include <utility>   // std::pair

export module dedekind.relational:dyadic;

import dedekind.category; // IsSet, ClassicalLogic
import dedekind.sets;     // Set<std::pair<...>, L, P> (:expressions)

namespace dedekind::sets {

// ── Meet / join of relational predicates (the carriers of & and +) ─────────
/** @brief Meet (conjunction) of two relational predicates. */
export template <typename A, typename B>
struct RelAnd {
  using is_rel_predicate = void;
  A a;
  B b;
  // @c auto (not @c bool): the result inherits the operands' own logic, so over
  // a @c TernaryLogic relation this is the Kleene @c ∧ (a @c bool cast would
  // collapse @c Unknown).  FIXME(#780): mixed Boolean/Ternary operands (a
  // ternary relation @c & @c diagonal()) still need a lift on the bool side.
  template <typename P>
  constexpr auto operator()(const P& p) const {
    return a(p) && b(p);
  }
};

/** @brief Join (disjunction) of two relational predicates --- the relation
 *  UNION carrier, dual to @c RelAnd.  It is the Kleene @c + of the relation
 *  algebra @f$(+, ;, {}^{*})@f$: @c ; is the relative product @c >>, and @c *
 *  the reflexive-transitive closure. */
export template <typename A, typename B>
struct RelOr {
  using is_rel_predicate = void;
  A a;
  B b;
  // @c auto (not @c bool): the result inherits the operands' own logic, so over
  // a @c TernaryLogic relation this is the Kleene @c ∨ (a @c bool cast would
  // collapse @c Unknown).  FIXME(#780): mixed Boolean/Ternary operands still
  // need a lift on the bool side.
  template <typename P>
  constexpr auto operator()(const P& p) const {
    return a(p) || b(p);
  }
};

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

// ── Union (Kleene +), meet, the diagonal, reflexive / symmetric closures ────
/** @brief @c R @c + @c S --- the UNION of two relations over the same product
 *  @f$A \times B@f$: membership is either predicate (@c RelOr).  The Kleene @c
 * +
 *  (@c ; is the relative product @c >>). */
export template <typename A, typename B, typename L, typename PR, typename PS>
constexpr auto operator+(const Set<std::pair<A, B>, L, PR>& r,
                         const Set<std::pair<A, B>, L, PS>& s) {
  return Set<std::pair<A, B>, L, RelOr<PR, PS>>{
      RelOr<PR, PS>{r.predicate(), s.predicate()}};
}

/** @brief @c R @c & @c S --- the INTERSECTION (meet) of two relations over the
 *  same product, dual to the union @c +: membership is both predicates
 *  (@c RelAnd).  The Boolean-lattice ∩ on relations. */
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
export template <typename A, typename L = dedekind::category::ClassicalLogic>
constexpr auto diagonal() {
  return Set<std::pair<A, A>, L, DiagPred<A>>{DiagPred<A>{}};
}

/** @brief @c reflexive(R) = @c R @c + @c Δ --- the smallest reflexive relation
 *  containing an endorelation @c R (add the self-loops). */
export template <typename A, typename L, typename P>
constexpr auto reflexive(const Set<std::pair<A, A>, L, P>& r) {
  return r + diagonal<A, L>();
}

/** @brief @c symmetric(R) = @c R @c + @c R° --- the smallest symmetric relation
 *  containing @c R (add the reversed edges; @c R° is the @c converse). */
export template <typename A, typename L, typename P>
constexpr auto symmetric(const Set<std::pair<A, A>, L, P>& r) {
  return r + converse(r);
}

// ── Self-contained base witnesses (no order DSL) ────────────────────────────
// The rich witnesses (≤∘≤=≤ transitivity, reflexive(<), symmetric(<)) live in
// order/halfspace, which owns the π1/π2 projection DSL and now consumes these
// combinators by ADL.  Here we witness the base laws on Δ alone
// (self-contained, no external predicate): Δ is a relation; Δ° = Δ; Δ;Δ = Δ;
// reflexive(Δ)=Δ.
static_assert(is_relation(diagonal<bool>()), "Δ is a relation (IsSet on ×).");
static_assert(diagonal<bool>()(std::pair{true, true}), "Δ contains (a,a).");
static_assert(!diagonal<bool>()(std::pair{true, false}), "Δ excludes (a,b≠a).");
static_assert(converse(diagonal<bool>())(std::pair{true, true}),
              "Δ° = Δ: the diagonal is its own converse.");
static_assert((diagonal<bool>() >> diagonal<bool>())(std::pair{true, true}),
              "Δ;Δ = Δ: the diagonal is the ; unit.");
static_assert(!(diagonal<bool>() >> diagonal<bool>())(std::pair{true, false}),
              "Δ;Δ excludes off-diagonal.");
static_assert(reflexive(diagonal<bool>())(std::pair{false, false}),
              "reflexive(Δ) = Δ still contains the diagonal.");
static_assert(symmetric(diagonal<bool>())(std::pair{true, true}),
              "symmetric(Δ) = Δ ∪ Δ° = Δ.");

}  // namespace dedekind::sets
