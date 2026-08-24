/**
 * @file dedekind/sets/graph.cppm
 * @partition :graph
 * @brief The graph of a function as a relation: the analytic arrow @c f and
 *        its set of pairs are one object.
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
 * @c Unknown (Rice's theorem).  That finite witness is @c IsGraphOf
 * (forthcoming in this partition).
 *
 * @build_order after :expressions, :relational
 * @dependency :category, :expressions
 *
 * @see dedekind.sequences:path (@c as_relation --- the @c Path special case)
 * @see dedekind.category:cartesian (@c arrow_as_relation --- the 2-arg form)
 */
module;

#include <concepts>     // std::equality_comparable
#include <type_traits>  // std::remove_cvref_t
#include <utility>      // std::pair

export module dedekind.sets:graph;

import dedekind.category; // IsArrow, Dom, Cod, arrow_as_relation, ClassicalLogic
import :expressions;      // Set, Relation

namespace dedekind::sets {

/**
 * @brief @c graph(f) --- the graph of a function @c f : A → B as the @c Set
 *        of pairs @f$\{\,(a,b) \mid b = f(a)\,\} \subseteq A\times B@f$.
 *
 * @tparam F An @c IsArrow whose @c Codomain has decidable equality.
 * @param  f The analytic arrow.
 * @return A @c Set<std::pair<Dom<F>, Cod<F>>> (an @c :expressions Relation)
 *         whose membership delegates to @c arrow_as_relation<F>.
 */
export template <typename F>
  requires dedekind::category::IsArrow<F> &&
           std::equality_comparable<typename std::remove_cvref_t<F>::Codomain>
constexpr auto graph(F f) {
  using A = dedekind::category::Dom<F>;
  using B = dedekind::category::Cod<F>;
  using Pair = std::pair<A, B>;
  // Encode-the-pullback: the graph's membership IS arrow_as_relation's
  // indicator, so the Set<pair> form cannot diverge from the 2-arg form.
  const dedekind::category::arrow_as_relation<std::remove_cvref_t<F>> indicator{
      f};
  auto pred = [indicator](const Pair& p) -> bool {
    return indicator(p.first, p.second);
  };
  return Set<Pair, dedekind::category::ClassicalLogic, decltype(pred)>{pred};
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
 */

// FUNCTIONAL (single-valued, right-unique) --- nLab / allegory "functional",
// Bird and de Moor "simple".
export template <typename R>
concept IsFunctional = dedekind::category::is_right_unique_v<R>;

// ENTIRE (total, left-total) --- nLab / allegory / Bird and de Moor "entire".
export template <typename R>
concept IsEntire = dedekind::category::is_left_total_v<R>;

// A FUNCTION is a relation that is functional and entire (nLab, verbatim).
// Refines IsBinaryRelation, so IsFunction subsumes it: {function} ⊂ {relation}.
export template <typename R, typename A, typename B>
concept IsFunction = dedekind::category::IsBinaryRelation<R, A, B> &&
                     IsFunctional<R> && IsEntire<R>;

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

// The graph of an arrow is a function (functional ∧ entire, by construction).
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

}  // namespace dedekind::sets
