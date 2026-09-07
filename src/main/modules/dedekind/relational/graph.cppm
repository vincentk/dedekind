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

#include <concepts>     // std::equality_comparable, std::same_as
#include <ranges>       // std::ranges::input_range, range_value_t
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::pair

export module dedekind.relational:graph;

import dedekind.category; // IsArrow, Dom, Cod, arrow_as_relation, ClassicalLogic
import dedekind.sets;     // Set, Relation (:expressions); forall (:quantifier)
import :dyadic;           // the Tarski BASE: a graph Γ_f IS a dyadic relation

// Namespace stays dedekind::sets (ADL on Set/Relation arguments); only the
// module moved — a graph belongs with the relations.  See :tables.

namespace dedekind::sets {

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
using Graph = Set<std::pair<typename F::Domain, typename F::Codomain>,
                  dedekind::category::ClassicalLogic, GraphPredicate<F>>;

/**
 * @brief @c graph(f) --- the graph of a function @c f : A → B as the @c Set
 *        of pairs @f$\{\,(a,b) \mid b = f(a)\,\} \subseteq A\times B@f$.
 *
 * @tparam F An @c IsArrow whose @c Codomain has decidable equality.
 * @param  f The analytic arrow.
 * @return A @c Graph<F> (a @c :expressions Relation) whose @c GraphPredicate<F>
 *         membership carries @c f for composition.
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

}  // namespace dedekind::sets

// A graph IS a functional (single-valued) AND entire (total) relation --- it is
// the graph of a total function.  Mark both faces on the @c Graph<F> type (the
// @c Set<pair> form), the same convention @c arrow_as_relation already carries,
// so @c IsFunctional / @c IsEntire hold on @c graph(f) itself, not only on the
// 2-argument indicator.  (@c IsFunction<R,A,B> stays the 2-argument reading ---
// it needs the @c r(a,b) call shape, which the 1-argument @c Set<pair> graph
// does not have.)  Closes the gap that a graph was a relation but not yet a
// marked function, and lets the relative product below gate on @c IsFunctional
// at this partition's own level.
namespace dedekind::category {
template <typename F>
inline constexpr bool is_left_total_v<dedekind::sets::Graph<F>> = true;
template <typename F>
inline constexpr bool is_right_unique_v<dedekind::sets::Graph<F>> = true;
}  // namespace dedekind::category

namespace dedekind::sets {

// ── The relation/function lattice, defined here (above the relative product)
// so
//    the composition can gate on IsFunctional.  Fuller exposition in the
//    @section graph__The_Relation_Function_Lattice below (nLab; Freyd--Scedrov;
//    Bird--de Moor). ──────────────────────────────────────────────────────────
// FUNCTIONAL (single-valued, right-unique).
export template <typename R>
concept IsFunctional = dedekind::category::is_right_unique_v<R>;
// ENTIRE (total, left-total).
export template <typename R>
concept IsEntire = dedekind::category::is_left_total_v<R>;
// A FUNCTION is a relation that is functional and entire (nLab, verbatim).
export template <typename R, typename A, typename B>
concept IsFunction = dedekind::category::IsBinaryRelation<R, A, B> &&
                     IsFunctional<R> && IsEntire<R>;

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
 * @c ∘ --- the general-carrier sibling of the affine @c ProjAddConstProj @c ;
 * in
 * @c :halfspace (which adds pivots).
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
      dedekind::category::operator>>(r.predicate().arrow, s.predicate().arrow));
}

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
concept IsGraph = dedekind::sets::IsRelation<S, T1, T2>;

/** @section graph__Formal_Verification_Lattice */

// A general relation that is NOT a function: b % a == 0 relates 1 to every b,
// so it is not single-valued.  It is a relation but not functional/entire.
namespace {
struct Divides {
  constexpr bool operator()(int a, int b) const { return a != 0 && b % a == 0; }
};
}  // namespace

// The IsFunction certificate (functional ∧ entire, by construction) is carried
// by arrow_as_relation<F> --- the 2-arg form to which graph(f)'s membership is
// definitionally equal pointwise (Γ_id witness above).  graph(f) itself is the
// Set<pair> reification, certified IsSet (a subobject of A × B); reifying
// IsFunction on the unary-on-pair Set form is deferred.
// FIXME(#783): forward is_right_unique / is_left_total onto the graph type so
// this assertion lands on graph(f) directly, not only on the adapter.
static_assert(
    IsFunction<dedekind::category::arrow_as_relation<
                   dedekind::category::Identity<int>>,
               int, int>,
    "arrow_as_relation<Identity> is a function: functional ∧ entire.");

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
  const auto g = graph(f);
  return forall(dom, [&r, &g, &cod](const auto& a) {
    return forall(cod, [&r, &g, &a](const auto& b) {
      return static_cast<bool>(r(std::pair{a, b})) ==
             static_cast<bool>(g(std::pair{a, b}));
    });
  });
}

/** @section graph__Formal_Verification_IsGraphOf */

namespace {
struct Succ {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(int x) const { return x + 1; }
};
}  // namespace

// The compiler knows graph(id) IS the graph of id over the finite domain [0,4).
static_assert(is_graph_of(Γ_id, dedekind::category::Identity<int>{},
                          std::views::iota(0, 4), std::views::iota(0, 4)),
              "graph(id) is the graph of the identity on [0,4).");

// ...and distinguishes it from a different function: graph(succ) is NOT the
// graph of id.  The witness genuinely decides equality on the finite sample.
static_assert(!is_graph_of(graph(Succ{}), dedekind::category::Identity<int>{},
                           std::views::iota(0, 4), std::views::iota(0, 4)),
              "graph(succ) differs from the graph of id on [0,4).");

}  // namespace dedekind::sets
