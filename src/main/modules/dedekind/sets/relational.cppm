/**
 * @file dedekind/sets/relational.cppm
 * @partition :relational
 * @brief Relational Algebra — combinators for Sets and Relations.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section relational__Description
 * This partition provides the five primitive operators of Codd's relational
 * algebra, lifted to the typed, intensional Set model of dedekind.sets:
 *
 *  - select      (σ) -- filter elements of a set by an additional predicate.
 *  - set_union   (∪) -- elements belonging to either of two same-typed sets.
 *  - set_difference (∖) -- elements of A that are absent from B.
 *  - set_intersection (∩) -- elements common to both sets (derived operator).
 *  - natural_join (⋈) -- composition of two binary relations on a shared type.
 *
 * Cross-product (×) is already provided by :expressions as cartesian_product.
 * Projection (π) requires existential quantification over an infinite domain
 * and is therefore intentionally omitted from this MVP.
 * Rename (ρ) reduces to a type alias in C++ and requires no runtime support.
 *
 * @section relational__Canonical_Examples
 * ```cpp
 * // Two relations: parent ⊆ Person × Person, ancestor ⊆ Person × Person
 * const auto grandparent = natural_join(parent, parent);       // ⋈
 * const auto either = set_union(mother, father);               // ∪
 * const auto adults = select(people, [](const Person& p) {     // σ
 *   return p.age >= 18;
 * });
 * const auto adults_only = set_difference(people, minors);     // ∖
 * ```
 *
 * @section relational__References
 * - Codd, E.F. (1970) "A Relational Model of Data for Large Shared Data
 *   Banks", Comm. ACM 13(6). https://doi.org/10.1145/362384.362685
 * - Wikipedia: Relational algebra
 *   https://en.wikipedia.org/wiki/Relational_algebra
 *
 * @quote
 * "Matter does not engage their attention, they are interested in form
 * alone."
 * -- Henri Poincare, Science and Hypothesis (1901)
 *
 * @note "All laws are deduced from experiment; but to enunciate them, a
 * special language is needful."
 *       -- Henri Poincare, The Value of Science (1905)
 */
module;

#include <cstddef>
#include <functional>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <unordered_set>
#include <utility>

export module dedekind.sets:relational;

import dedekind.category;
import :expressions;
import :extensional;

namespace dedekind::sets {
using namespace dedekind::category;

struct CanonicalPairPredicate {
  constexpr bool operator()(const std::pair<int, int>&) const { return true; }
};

using CanonicalIntRelation =
    Relation<int, int, ClassicalLogic, CanonicalPairPredicate>;
using CanonicalIntRelationDomain = typename CanonicalIntRelation::Domain;

static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<CanonicalIntRelationDomain>(
            CanonicalIntRelation{CanonicalPairPredicate{}}))>,
    "Relation aliases must lift to ETCS set objects.");
static_assert(
    dedekind::category::IsProduct<CanonicalIntRelationDomain, int, int>,
    "Relation domain must satisfy categorical IsProduct.");

/**
 * @brief Selection (σ): filter elements of a set by an additional predicate.
 *
 * σ_f(S) = {x ∈ S | f(x)}
 *
 * Constructs a new Set whose membership is the conjunction of membership in
 * @p s and satisfaction of @p pred. Predicate outputs are normalized through
 * @c dedekind::category::lift_logic<L>, so both native logical witnesses
 * (@c L::Ω) and bool-valued predicates are accepted.
 *
 * @tparam T  Element type.
 * @tparam L  Logic species.
 * @tparam P  Existing predicate type of @p s.
 * @tparam Pred  Additional filter predicate type.
 */
export template <typename T, typename L, typename P, typename Pred>
  requires std::invocable<const std::decay_t<Pred>&, const T&> &&
           requires(
               std::invoke_result_t<const std::decay_t<Pred>&, const T&> v) {
             {
               dedekind::category::lift_logic<L>(v)
             } -> std::same_as<typename L::Ω>;
           }
constexpr auto select(const Set<T, L, P>& s, Pred&& pred) {
  auto lifted = [p = std::forward<Pred>(pred)](const T& v) -> typename L::Ω {
    return dedekind::category::lift_logic<L>(std::invoke(p, v));
  };
  auto combined = [base = s, f = std::move(lifted)](const T& v) ->
      typename L::Ω { return L::AND(base(v), f(v)); };
  return Set<T, L, decltype(combined)>{std::move(combined)};
}

/**
 * @brief Union (∪): elements belonging to at least one of two same-typed sets.
 *
 * A ∪ B = {x | x ∈ A ∨ x ∈ B}
 *
 * Named alias for the @c operator| on Set, provided for relational-algebra
 * readability.
 */
export template <typename T, typename L, typename P1, typename P2>
constexpr auto set_union(const Set<T, L, P1>& a, const Set<T, L, P2>& b) {
  return a | b;
}

/**
 * @brief Difference (∖): elements of A that are absent from B.
 *
 * A ∖ B = {x | x ∈ A ∧ x ∉ B}
 *
 * Expressed as the conjunction of membership in @p a and non-membership in
 * @p b.
 */
export template <typename T, typename L, typename P1, typename P2>
constexpr auto set_difference(const Set<T, L, P1>& a, const Set<T, L, P2>& b) {
  return a & !b;
}

/**
 * @brief Intersection (∩): elements common to both sets.
 *
 * A ∩ B = {x | x ∈ A ∧ x ∈ B}
 *
 * Named alias for the @c operator& on Set.  Intersection is a derived
 * operator in minimal relational algebra (A ∩ B = A ∖ (A ∖ B)), but is
 * provided here for ergonomics.
 */
export template <typename T, typename L, typename P1, typename P2>
constexpr auto set_intersection(const Set<T, L, P1>& a,
                                const Set<T, L, P2>& b) {
  return a & b;
}

/**
 * @brief Natural join (⋈): compose two binary relations on a shared middle
 * type.
 *
 * Given R1 ⊆ T1 × T2 and R2 ⊆ T2 × T3,
 * R1 ⋈ R2 = {(t1, t2, t3) | (t1, t2) ∈ R1 ∧ (t2, t3) ∈ R2}
 *
 * The result is a Set<std::tuple<T1,T2,T3>, L, ...>.  Membership of a triple
 * (t1,t2,t3) is the conjunction of membership of (t1,t2) in R1 and (t2,t3)
 * in R2.
 *
 * @tparam T1  Type of the first component (left relation domain).
 * @tparam T2  Shared join type (right component of R1, left component of R2).
 * @tparam T3  Type of the third component (right relation codomain).
 * @tparam L   Logic species shared by both relations.
 */
export template <typename T1, typename T2, typename T3, typename L, typename P1,
                 typename P2>
constexpr auto natural_join(const Relation<T1, T2, L, P1>& r1,
                            const Relation<T2, T3, L, P2>& r2) {
  using Triple = std::tuple<T1, T2, T3>;
  auto pred = [r1, r2](const Triple& t) {
    const auto in_r1 = r1(std::pair<T1, T2>{std::get<0>(t), std::get<1>(t)});
    const auto in_r2 = r2(std::pair<T2, T3>{std::get<1>(t), std::get<2>(t)});
    return L::AND(in_r1, in_r2);
  };
  return Set<Triple, L, decltype(pred)>{pred};
}

using CanonicalNaturalJoin =
    decltype(natural_join(CanonicalIntRelation{CanonicalPairPredicate{}},
                          CanonicalIntRelation{CanonicalPairPredicate{}}));
using CanonicalNaturalJoinDomain = typename CanonicalNaturalJoin::Domain;

static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<CanonicalNaturalJoinDomain>(
            std::declval<const CanonicalNaturalJoin&>()))>,
    "natural_join output must lift to an ETCS set object.");

/**
 * @brief Take (LIMIT N): bounded materialisation of an iterable source.
 *
 * @details Walks @p source up to the first @p n distinct elements and
 * returns them as an @c ExtensionalSet<T>.  This is the realisation-
 * boundary primitive that operationalises the @em "Intensional first,
 * realize when you mean it" discipline of @c sec:intensional in
 * @c paper.tex: the user explicitly names the point at which a Path-
 * walkable source crosses into an enumerated container.
 *
 * @section relational__take_Gates
 *
 * @c take requires @p source to be a @c std::ranges::input_range.  This
 * subsumes every realistic source today:
 *
 *  - @c std::set<T> / @c std::unordered_set<T> (extensional carriers)
 *  - @c ExtensionalSet<T> (the project's small-finite extensional carrier
 *    in @c sets:extensional; exposes @c begin() / @c end())
 *  - @c FinitePath<T> (the bounded sequence in @c sequences:path; also
 *    exposes @c begin() / @c end() per @c IsSequence's
 *    @c Iterator_Range_Anchor)
 *  - @c std::views::filter -style range adaptors over any of the above
 *
 * Sources that satisfy the carrier-side @c IsCountableSet concept (which
 * exposes @c as_sequence() returning an @c IsSequence) are covered
 * indirectly via @c std::ranges::input_range on the sequence: callers
 * pass @c s.as_sequence() to @c take .  A future overload may take the
 * @c IsCountableSet directly and call @c as_sequence() internally; this
 * MVP keeps the interface uniform on the range surface.
 *
 * Intensional sources without a Path or a range surface (e.g.\ a raw
 * @c Set<T, L, P> over a transfinite ambient with no canonical
 * enumeration on @c T) trip @em Honest Rejection: there is no
 * @c std::ranges::input_range overload, the template substitution fails,
 * and the compiler refuses with a named concept-failure diagnostic.
 *
 * @section relational__take_Categorical_Anchors
 *
 * @c take is the bounded hylomorphism @em anamorphism then catamorphism:
 *
 *  - The unfold (anamorphism) walks the source as an @c F-coalgebra for
 *    the list/stream functor @f$F(X) = 1 + T \times X@f$.  This is what
 *    the codebase's @c IsFCoalgebra concept names
 *    (@c category:functor.cppm).
 *  - The fold (catamorphism) accumulates into @c ExtensionalSet<T>, the
 *    @c F-algebra side.  The depth budget @p n bounds the unfold.
 *
 * The duality is exact: @c argmax (@c sec:lp-centrepiece) is the
 * F-algebra-shaped side, witnessed by @c IsFAlgebra ; @c take is the
 * F-coalgebra-shaped side, witnessed by the source carrying an
 * @c IsFCoalgebra (or, indirectly, by satisfying the
 * @c std::ranges::input_range / @c IsCountableSet surface).
 *
 * @section relational__take_Compile_Time
 *
 * @c take is intentionally @b runtime-only in this MVP.  The compile-
 * time path (folding the entire reduction into a typed constant) would
 * require an output type that is @c constexpr -constructible with
 * populated contents; the current @c ExtensionalSet uses
 * @c std::unordered_set internally which is not (in C++23, with
 * non-empty contents).  A constexpr-friendly extensional output type or
 * an NTTP-encoded list output is the follow-up that unlocks
 * @c if @c consteval dispatch; see issue #753.
 *
 * @section relational__take_Realisation_Boundary
 *
 * Among @c sets:relational 's operators, @c take is the only one that
 * materialises a result: every other primitive (@c select , @c
 * set_union , @c set_intersection , @c set_difference , @c
 * cartesian_product , @c natural_join ) composes predicates without
 * touching elements.  @c take is therefore the explicit crossing of the
 * realisation boundary the paper §3 names — the point at which the
 * user has decided that an extensional answer is wanted.
 *
 * @tparam SourceRange  Any @c std::ranges::input_range whose
 *         @c value_type is the element type @c T .
 *
 * @param  source  The source to walk.  Iterated front-to-back up to
 *                 the first @p n distinct elements.
 * @param  n       The maximum number of elements to take.  The result's
 *                 size is @c min(n, |source|).
 *
 * @return An @c ExtensionalSet<T> containing the first @p n distinct
 *         elements of @p source in @p source 's iteration order
 *         (modulo the @c std::unordered_set 's hash-bucket order in
 *         the resulting container).
 */
export template <std::ranges::input_range SourceRange>
constexpr auto take(SourceRange&& source, std::size_t n) {
  using T = std::remove_cvref_t<std::ranges::range_value_t<SourceRange>>;
  std::unordered_set<T> picked;
  picked.reserve(n);
  for (auto&& element : source) {
    if (picked.size() >= n) break;
    // emplace (rather than insert(forward<>(element))) for proxy-reference
    // safety: std::ranges::input_range may yield proxy references (e.g.\
    // std::vector<bool>::reference) whose conversion-to-T inside insert
    // is not guaranteed.  emplace constructs in place from the element
    // expression directly, routing through T's constructor unambiguously.
    picked.emplace(std::forward<decltype(element)>(element));
  }
  return ExtensionalSet<T>{std::move(picked)};
}

/**
 * @brief Limit: a named alias for @c take following SQL ergonomics.
 *
 * @details @c SELECT @c * @c FROM @c S @c LIMIT @c N is the SQL spelling
 * of the intensional-SQL operator @c take(S, @c N) .  This alias is
 * provided so the SQL-fluent reading reads verbatim.  Behaviour and gates
 * are identical to @c take .
 */
export template <std::ranges::input_range SourceRange>
constexpr auto limit(SourceRange&& source, std::size_t n) {
  return take(std::forward<SourceRange>(source), n);
}

}  // namespace dedekind::sets
