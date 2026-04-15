/**
 * @file dedekind/sets/relational.cppm
 * @partition :relational
 * @brief Level 1.3: Relational Algebra -- canonical combinators for Sets and
 * Relations.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
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
 * @section Canonical_Examples
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
 * @section References
 * - Codd, E.F. (1970) "A Relational Model of Data for Large Shared Data
 *   Banks", Comm. ACM 13(6). https://doi.org/10.1145/362384.362685
 * - Wikipedia: Relational algebra
 *   https://en.wikipedia.org/wiki/Relational_algebra
 *
 * @quote
 * "The relational model is not just a data model — it is a branch of
 *  mathematical logic applied to data management."
 * ("Le modèle relationnel n'est pas qu'un modèle de données — c'est une
 *  branche de la logique mathématique appliquée à la gestion des données.")
 * -- E.F. Codd, paraphrase
 */
module;

#include <functional>
#include <tuple>
#include <type_traits>
#include <utility>

export module dedekind.sets:relational;

import dedekind.category;
import :expressions;

namespace dedekind::sets {
using namespace dedekind::category;

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

}  // namespace dedekind::sets
