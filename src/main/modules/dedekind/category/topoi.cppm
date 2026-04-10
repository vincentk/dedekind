/**
 * @file dedekind/category/topoi.cppm
 * @partition :topoi
 * @brief Level 1: The Internal Logic of the Topos.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "Grothendieck has made the category (rather than the space) the central
 *  aspect that we should explicitly axiomatise."
 *  — Myles Tierney
 *
 * @section The_Subobject_Classifier
 * The Lawvere-Tierney object Ω (Omega) is the central object that classifies
 * subobjects. Every elementary topos carries an internal logic which
 * generalizes standard set theory into objective categorical form.
 *
 * Wikipedia: Lawvere–Tierney topology, Subobject classifier, Topos
 */

module;

#include <concepts>
#include <utility>

export module dedekind.category:topoi;

import :logic;
import :morphism;
import :cartesian;

namespace dedekind::category {

/**
 * @concept IsPredicate
 * @brief Categorification of a "Logical Rule" as a Morphism A → Ω.
 */
export template <typename P>
concept IsPredicate = IsArrow<P> && LogicalValue<Cod<P>>;

/**
 * @brief Characteristic Morphism Factory (classify)
 * @details Lifts a raw mapping into a formal Predicate Morphism A → Ω.
 */
export template <typename A, typename F>
constexpr auto classify(F&& f) {
  return arrow<A>(std::forward<F>(f));
}

/**
 * @section Universal_Truth
 * @brief The 'true' morphism: 1 → Ω.
 */
export template <typename L = ClassicalLogic>
  requires LogicalSpecies<L>
auto logical_true() {
  return arrow<One, typename L::type>([](One) { return L::True; });
}

/**
 * @brief Logic Composition: Conjunction (AND) of Predicates.
 * @details Realises the internal logic of the Topos via Morphism composition.
 */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator&&(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;

  return arrow<A>([p = std::forward<P>(p), q = std::forward<Q>(q)](const A& x) {
    return L::AND(p(x), q(x));
  });
}

}  // namespace dedekind::category
