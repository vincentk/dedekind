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
#include <type_traits>
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
 *
 * @req { p(x) } The candidate must be callable with an instance of the
 * Domain.
 * @req SpeciesTraits<Res>::species The return type must be registered in the
 *      Ontology Bridge.
 * @req IsLogicalSpecies<S> The resolved logic species must satisfy the
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
 * @concept IsSubobject
 * @brief The categorical witness of a monomorphism ι: S ↣ A.
 *
 * @tparam S The Subobject Species (The "Body").
 * @tparam A The Ambient Species (The "Space").
 */
export template <typename S, typename A>
concept IsSubobject = requires(S s, typename S::Member m) {
  /**
   * @brief ι: S ↣ A
   * The canonical inclusion morphism. Every member of S must
   * uniquely map to a member of the ambient species A.
   */
  { s.ι(m) } -> std::same_as<A>;

  /**
   * @brief χ: A ⟶ Ω
   * Every subobject in a Topos is uniquely classified by a
   * morphism into the subobject classifier Ω.
   */
  { s.χ } -> IsPredicate;

  // Metadata verification: The declared ambient must match A.
  typename S::Ambient;
  requires std::same_as<typename S::Ambient, A>;

  // Characteristic morphism verification: χ must classify elements of A.
  requires std::same_as<Dom<decltype(s.χ)>, A>;
};

/**
 * @brief The Subobject Species S ↣ A.
 * @details Represents a subset of the ambient species A, defined by the
 * characteristic morphism χ: A ⟶ Ω.
 *
 * @tparam A The Ambient (Super) Species.
 * @tparam Chi The type of the characteristic morphism χ.
 */
export template <typename A, typename Chi>
struct Subobject {
  using Ambient = A;
  Chi χ;  // The Rule: A ⟶ Ω

  /**
   * @brief The Internal Member of the Subobject.
   * Wraps a value from the ambient space that satisfies the classifier.
   */
  struct Member {
    A value;
  };

  /** @brief ι: S ↣ A (The Textbook Inclusion) */
  constexpr A ι(const Member& m) const { return m.value; }

  /**
   * @section Pullback_Projections
   * These projections are only valid when the Ambient space A satisfies
   * the IsProduct concept (X × Y).
   */

  /** @brief π₁: S ⟶ X */
  constexpr auto π1(const Member& m) const
    requires requires { m.value.first; }
  {
    return m.value.first;
  }

  /** @brief π2: S ⟶ Y */
  constexpr auto π2(const Member& m) const
    requires requires { m.value.second; }
  {
    return m.value.second;
  }
};

/**
 * @brief Characteristic Morphism Factory (classify)
 * Returns the Subobject Species (The Body) carrying the rule (The Arrow).
 */
export template <typename A, typename F>
  requires(!IsArrow<std::remove_cvref_t<F>>) &&
          std::invocable<std::decay_t<F>, const A&> &&
          LogicalValue<std::invoke_result_t<std::decay_t<F>, const A&>>
constexpr auto classify(F&& f) {
  auto rule = arrow<A>(std::forward<F>(f));
  return Subobject<A, decltype(rule)>{std::move(rule)};
}

/**
 * @brief Idempotent classify: Passthrough for existing Arrows.
 */
export template <typename A, IsArrow F>
  requires IsPredicate<F> && std::same_as<Dom<F>, A>
constexpr auto classify(F&& f) {
  return Subobject<A, std::remove_cvref_t<F>>{std::forward<F>(f)};
}

/** @brief Logical Conjunction (Intersection): Synthesizes a rule for A ∩ B.
 */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator&&(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  return arrow<A, Ω>([p = std::forward<P>(p), q = std::forward<Q>(q)](
                         const A& x) { return L::AND(p(x), q(x)); });
}

/** @brief Logical Disjunction (Union): Synthesizes a rule for A ∪ B. */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator||(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  return arrow<A, Ω>([p = std::forward<P>(p), q = std::forward<Q>(q)](
                         const A& x) { return L::OR(p(x), q(x)); });
}

/** @brief Logical Negation (Complement): Synthesizes a rule for ¬A. */
export template <IsPredicate P>
auto operator!(P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;

  // Return a formal Morphism A -> Ω
  return arrow<A>(
      [p = std::forward<P>(p)](const A& x) { return L::NOT(p(x)); });
}

/**
 * @brief The 'true' morphism: 1 → Ω.
 */
export template <typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
auto logical_true() {
  return arrow<One, typename L::Ω>([](One) { return L::True; });
}

/**
 * @brief The 'false' morphism: 1 → Ω.
 */
export template <typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
auto logical_false() {
  return arrow<One, typename L::Ω>([](One) { return L::False; });
}

/**
 * @brief Power Object Alias
 * @details In a Topos, P(A) is the exponential object Ω^A.
 */
export template <typename A, typename L = ClassicalLogic>
using PowerObject = Exponential<A, typename L::Ω>;

}  // namespace dedekind::category
