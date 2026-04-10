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

/** @section The Point-Free Composition Engine */

/**
 * @brief The Predicate Morphism (A Mapping from Domain to Truth).
 *
 * In the Dedekind topos, a Set is not a container, but a Rule. `IsPredicate`
 * formalizes this rule as a mapping from a Domain object (T) to a Logical
 * Species (Ω).
 *
 * @tparam P The Predicate candidate (typically a Lambda or a Functor).
 * @tparam T The Domain of the predicate (the type of object being tested).
 *
 * @req { p(x) } The candidate must be callable with an instance of the
 * Domain.
 * @req SpeciesTraits<Res>::species The return type must be registered in the
 *      Ontology Bridge.
 * @req LogicalSpecies<S> The resolved logic species must satisfy the
 *      formal algebraic requirements (AND/OR/NOT).
 *
 * @note By enforcing this contract, `dedekind` ensures that logical
 * composition (p1 && p2) only occurs between rules that share a common
 * mathematical foundation.
 */
export template <typename P>
concept IsPredicate = IsArrow<P> && LogicalValue<Cod<P>> &&
                      !std::same_as<std::remove_cvref_t<P>, bool>;

/**
 * @concept IsCharacteristic
 * @brief Categorical alias for a Predicate mapping to Ω.
 */
export template <typename P>
concept IsCharacteristic = IsPredicate<P>;

/**
 * @brief Characteristic Morphism Factory (classify)
 * @details Lifts a raw mapping into a formal Predicate Morphism A → Ω.
 *
 * @tparam A The Domain object.
 * @tparam F A mapping that must result in a LogicalValue (bool, Ternary, etc.).
 */
export template <typename A, typename F>
// 1. First, verify the logic of the mapping itself
  requires LogicalValue<std::invoke_result_t<std::decay_t<F>, A>>
constexpr auto classify(F&& f) {
  // 2. Now call the skeletal factory
  auto result = arrow<A>(std::forward<F>(f));

  // 3. Optional: static_assert here instead of 'requires'
  // to avoid circular dependency in template resolution
  static_assert(
      IsPredicate<decltype(result)>,
      "Dedekind Logic Error: result of classify must be an IsPredicate.");

  return result;
}

/** @brief Logical Conjunction (Intersection): Synthesizes a rule for A ∩ B.
 */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator&&(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;

  return classify<A>([p = std::forward<P>(p), q = std::forward<Q>(q)](
                         const A& x) { return L::AND(p(x), q(x)); });
}

/** @brief Logical Disjunction (Union): Synthesizes a rule for A ∪ B. */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator||(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;

  return classify<A>([p = std::forward<P>(p), q = std::forward<Q>(q)](
                         const A& x) { return L::OR(p(x), q(x)); });
}

/** @brief Logical Negation (Complement): Synthesizes a rule for ¬A. */
export template <IsPredicate P>
auto operator!(P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;

  // Return a formal Morphism A -> Ω
  return classify<A>(
      [p = std::forward<P>(p)](const A& x) { return L::NOT(p(x)); });
}

/**
 * @brief The 'true' morphism: 1 → Ω.
 */
export template <typename L = ClassicalLogic>
  requires LogicalSpecies<L>
auto logical_true() {
  return arrow<One, typename L::type>([](One) { return L::True; });
}

/**
 * @brief The 'false' morphism: 1 → Ω.
 */
export template <typename L = ClassicalLogic>
  requires LogicalSpecies<L>
auto logical_false() {
  return arrow<One, typename L::type>([](One) { return L::False; });
}

/**
 * @brief Power Object Alias
 * @details In a Topos, P(A) is the exponential object Ω^A.
 */
template <typename A, typename L = ClassicalLogic>
using PowerObject = Exponential<A, typename L::type>;

}  // namespace dedekind::category
