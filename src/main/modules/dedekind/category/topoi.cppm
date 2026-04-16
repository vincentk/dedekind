/**
 * @file dedekind/category/topoi.cppm
 * @partition :topoi
 * @brief The Internal Logic of the Topos.
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
 * Textbook defaults in this partition:
 * - `operator&&` models meet/intersection of characteristic predicates.
 * - `operator||` models join/union of characteristic predicates.
 * - `operator!` models complement.
 *
 * Wikipedia: Lawvere–Tierney topology, Subobject classifier, Topos
 * @see Lawvere (1964), An elementary theory of the category of sets.
 * @see Lambek & Scott (1988), Introduction to Higher-Order Categorical Logic.
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

template <typename>
inline constexpr bool always_false_v = false;

template <typename C>
concept IsClassifierConstant =
    std::same_as<std::remove_cvref_t<C>, bool> ||
    std::same_as<std::remove_cvref_t<C>, Ternary> || requires {
      typename std::remove_cvref_t<C>::logic_species;
      typename std::remove_cvref_t<C>::machine_type;
    };

template <typename OmegaTarget, typename Constant>
constexpr auto lift_classifier_constant(Constant&& value) {
  using C = std::remove_cvref_t<Constant>;
  if constexpr (std::same_as<C, OmegaTarget>) {
    return value;
  } else if constexpr (std::same_as<OmegaTarget, Ternary> &&
                       std::same_as<C, bool>) {
    return value ? Ternary::True : Ternary::False;
  } else {
    static_assert(always_false_v<OmegaTarget>,
                  "Unsupported classifier constant lift between logic species");
  }
}

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
concept IsPredicate =
    IsArrow<P> && LogicalValue<Cod<P>> && LogicalMap<P, Dom<P>> &&
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
 *  @note Textbook term: meet (∧) in the internal Heyting/Boolean algebra of Ω.
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

/** @brief Conjunction of a constant logical value with a predicate. */
export template <IsClassifierConstant C, IsPredicate P>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator&&(C&& constant, P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  const Ω lifted = lift_classifier_constant<Ω>(std::forward<C>(constant));
  return arrow<A, Ω>([lifted, p = std::forward<P>(p)](const A& x) {
    return L::AND(lifted, p(x));
  });
}

/** @brief Conjunction of a predicate with a constant logical value. */
export template <IsPredicate P, IsClassifierConstant C>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator&&(P&& p, C&& constant) {
  return std::forward<C>(constant) && std::forward<P>(p);
}

/** @brief Logical Disjunction (Union): Synthesizes a rule for A ∪ B.
 *  @note Textbook term: join (∨) in the internal Heyting/Boolean algebra of Ω.
 */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator||(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  return arrow<A, Ω>([p = std::forward<P>(p), q = std::forward<Q>(q)](
                         const A& x) { return L::OR(p(x), q(x)); });
}

/** @brief Disjunction of a constant logical value with a predicate. */
export template <IsClassifierConstant C, IsPredicate P>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator||(C&& constant, P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  const Ω lifted = lift_classifier_constant<Ω>(std::forward<C>(constant));
  return arrow<A, Ω>([lifted, p = std::forward<P>(p)](const A& x) {
    return L::OR(lifted, p(x));
  });
}

/** @brief Disjunction of a predicate with a constant logical value. */
export template <IsPredicate P, IsClassifierConstant C>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator||(P&& p, C&& constant) {
  return std::forward<C>(constant) || std::forward<P>(p);
}

/** @brief Logical Negation (Complement): Synthesizes a rule for ¬A.
 *  @note Textbook term: pseudocomplement/complement depending on Ω.
 */
export template <IsPredicate P>
auto operator!(P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;

  // Return a formal Morphism A -> Ω
  return arrow<A>(
      [p = std::forward<P>(p)](const A& x) { return L::NOT(p(x)); });
}

/**
 * @brief Compose two plain callable predicates over a declared domain A.
 * @details
 * This is a practical bridge for std::predicate-style lambdas/functions that
 * have not been lifted to Arrow/Predicate objects yet.
 */
export template <typename A, typename P, typename Q>
  requires std::invocable<const std::decay_t<P>&, const A&> &&
           std::invocable<const std::decay_t<Q>&, const A&> &&
           LogicalValue<std::remove_cvref_t<
               std::invoke_result_t<const std::decay_t<P>&, const A&>>> &&
           std::same_as<
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<P>&, const A&>>,
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<Q>&, const A&>>>
constexpr auto predicate_and(P&& p, Q&& q) {
  return classify<A>(std::forward<P>(p)).χ && classify<A>(std::forward<Q>(q)).χ;
}

/** @brief Disjunction bridge for plain callable predicates over domain A. */
export template <typename A, typename P, typename Q>
  requires std::invocable<const std::decay_t<P>&, const A&> &&
           std::invocable<const std::decay_t<Q>&, const A&> &&
           LogicalValue<std::remove_cvref_t<
               std::invoke_result_t<const std::decay_t<P>&, const A&>>> &&
           std::same_as<
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<P>&, const A&>>,
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<Q>&, const A&>>>
constexpr auto predicate_or(P&& p, Q&& q) {
  return classify<A>(std::forward<P>(p)).χ || classify<A>(std::forward<Q>(q)).χ;
}

/** @brief Negation bridge for plain callable predicates over domain A. */
export template <typename A, typename P>
  requires std::invocable<const std::decay_t<P>&, const A&> &&
           LogicalValue<std::remove_cvref_t<
               std::invoke_result_t<const std::decay_t<P>&, const A&>>>
constexpr auto predicate_not(P&& p) {
  return !classify<A>(std::forward<P>(p)).χ;
}

/**
 * @brief Constant classifier factory over domain A: A -> Ω.
 */
export template <typename A, typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
constexpr auto constant_classifier(typename L::Ω value) {
  return arrow<A, typename L::Ω>([value](const A&) { return value; });
}

/** @brief Default true classifier over domain A. */
export template <typename A, typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
constexpr auto classifier_true() {
  return constant_classifier<A, L>(L::True);
}

/** @brief Default false classifier over domain A. */
export template <typename A, typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
constexpr auto classifier_false() {
  return constant_classifier<A, L>(L::False);
}

/** @brief Default unknown classifier over domain A (only for logics with
 * Unknown). */
export template <typename A, typename L = TernaryLogic>
  requires IsLogicalSpecies<L> && requires { L::Unknown; }
constexpr auto classifier_unknown() {
  return constant_classifier<A, L>(L::Unknown);
}

/**
 * @brief Constant-to-predicate composition via the Highway notation.
 * @details Interpreted as conjunction with the constant classifier.
 */
export template <IsClassifierConstant C, IsPredicate P>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator>>(C&& constant, P&& p) {
  return std::forward<C>(constant) && std::forward<P>(p);
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
