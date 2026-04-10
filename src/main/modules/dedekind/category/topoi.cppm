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
export template <typename P, typename T>
concept IsPredicate = IsArrow<P> && LogicalValue<Cod<P>> &&
    !std::same_as<std::remove_cvref_t<P>, bool> &&  // Don't match raw bools
    requires(P p, T x) {
      { p(x) } -> LogicalValue;
      typename SpeciesTraits<decltype(p(x))>::species;
    } &&
    LogicalSpecies<
        typename SpeciesTraits<decltype(p(std::declval<T>()))>::species>;

/**
 * @concept IsCharacteristic
 * @brief Categorical alias for a Predicate mapping to Ω.
 */
export template <typename P, typename T>
concept IsCharacteristic = IsPredicate<P, T>;

/**
 * @section The Point-Free Infix Engine (Lattice Morphisms)
 * @brief Symbolic composition of predicates into higher-order rules.
 *
 * These operators lift standard logical connectives (&&, ||, !) into the
 * category of Predicates. Instead of evaluating truth values immediately,
 * they synthesize a new "Composite Rule" that lazily evaluates the
 * underlying logic only when a domain object is presented.
 *
 * @tparam T  The shared Domain of the predicates.
 * @tparam P1 The first Predicate morphism (A -> Ω).
 * @tparam P2 The second Predicate morphism (A -> Ω).
 *
 * @performance Zero-Cost Abstraction. By using perfect forwarding and
 * inline-friendly lambdas, the compiler can prune these symbolic trees
 * during the optimization pass, often emitting the same assembly as
 * a nested 'if' statement.
 */

/** @brief Logical Conjunction (Intersection): Synthesizes a rule for A ∩ B.
 */
export template <typename P1, typename P2, typename T = Dom<P1>>
  requires IsPredicate<P1, T> && IsPredicate<P2, T>
auto operator&&(P1&& p1, P2&& p2) {
  // We use the arrow factory to ensure the result is a formal Morphism
  return arrow<T>([p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2)](const T& x) {
    using S = typename SpeciesTraits<decltype(p1(x))>::species;
    return S::AND(p1(x), p2(x));
  });
}

/** @brief Logical Disjunction (Union): Synthesizes a rule for A ∪ B. */
export template <typename T, typename P1, typename P2>
  requires IsPredicate<P1, T> && IsPredicate<P2, T>
auto operator||(P1&& p1, P2&& p2) {
  return [p1 = std::forward<P1>(p1),
          p2 = std::forward<P2>(p2)](auto&& x) mutable -> decltype(p1(x)) {
    // 1. Resolve the Logic Species (Ω) from the return type
    using Res = decltype(p1(x));
    using S = typename SpeciesTraits<Res>::species;

    // 2. Synthesize the Supremum (Join)
    return S::OR(p1(std::forward<decltype(x)>(x)),
                 p2(std::forward<decltype(x)>(x)));
  };
}

/** @brief Logical Negation (Complement): Synthesizes a rule for ¬A. */
export template <typename T, typename P1>
  requires IsPredicate<P1, T>
auto operator!(P1&& p1) {
  return [p1 = std::forward<P1>(p1)](auto&& x) mutable -> decltype(p1(x)) {
    using S = typename SpeciesTraits<decltype(p1(x))>::species;
    return S::NOT(p1(std::forward<decltype(x)>(x)));
  };
}


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
