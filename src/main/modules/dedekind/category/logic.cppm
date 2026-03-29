/**
 * @file ontology:logic.cppm
 * @brief Level -1: The Rules of Thought (Boolean Algebra and Predicate
 * Synthesis).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :logic
 * @build_order 0
 * @dependency None (The Absolute Foundation)
 *
 * @section Logic: The Internal Language of the Topos
 * Before we can define an "Action" (Category) or a "Body" (Set), we must
 * establish the "Subobject Classifier" (Ω). This partition defines the
 * internal logic that governs all structuralist predicates.
 *
 * @details
 * Unlike the "External Logic" of the C++ compiler (which is strictly binary),
 * this partition allows for "Pluggable Truths":
 * - Classical Logic: The standard {True, False} boolean lightbulb.
 * - Kleene Logic: The {True, False, Unknown} propositional engine for
 *   handling undecidability and partial information.
 * - Lattice Morphisms: Infix operators (&&, ||, !) are anchored here as
 *   Point-Free composition rules, turning Predicates into a Bounded Lattice.
 *
 * @section Structural_Inference
 * By defining logic at Level -1, we enable "Decoupled Truth". A Set's
 * membership rule can switch from Boolean to Ternary without altering
 * the Mereological code in the partitions above.
 *
 * @anchors C++ Logical Primitives: bool, Ternary (Kleene), &&, ||, !.
 *
 * Wikipedia: Subobject classifier, Kleene logic, Internal logic, Point-free
 */
module;

#include <algorithm>
#include <concepts>

export module dedekind.category:logic;

namespace dedekind::category {

/**
 * @brief The Logical Species Concept (The Algebraic Signature of Truth).
 *
 * A type fulfills `LogicalSpecies` if it defines a consistent internal logic
 * over a specific 'type' of truth value. In categorical terms, this defines
 * the structure of the Subobject Classifier (Ω).
 *
 * @tparam L The Logic Species (e.g., ClassicalLogic, TernaryLogic).
 *
 * @req L::type The underlying data representation (e.g., bool, enum).
 * @req L::AND(a, b) The infimum (conjunction) morphism.
 * @req L::OR(a, b)  The supremum (disjunction) morphism.
 * @req L::NOT(a)    The negation (complement) morphism.
 *
 * @note This concept uses `std::same_as` to enforce strict species integrity;
 * logic operations must not result in type-decay or "species-leak."
 */
export template <typename L>
concept LogicalSpecies = requires(typename L::type a, typename L::type b) {
  { L::AND(a, b) } -> std::same_as<typename L::type>;
  { L::OR(a, b) } -> std::same_as<typename L::type>;
  { L::NOT(a) } -> std::same_as<typename L::type>;

  // The Categorical Constants (True/False)
  { L::True } -> std::convertible_to<typename L::type>;
  { L::False } -> std::convertible_to<typename L::type>;
};

/**
 * @section Species 1: Classical Logic (The Binary Prime)
 * @brief The internal logic of the Classical Topos ({True, False}).
 *
 * ClassicalLogic defines the standard Boolean algebra where the Law of
 * Excluded Middle holds. It maps the structuralist AND/OR/NOT morphisms
 * directly to C++ hardware-level logical primitives.
 *
 * @note This species is the "Zero-Cost" foundation for standard set operations.
 * Because it uses `bool`, the compiler can often resolve these operations
 * into single bitwise assembly instructions during DAG pruning.
 */
export struct ClassicalLogic final {
  using type = bool;
  static constexpr bool True = true;
  static constexpr bool False = false;

  static constexpr bool AND(bool a, bool b) { return a && b; }
  static constexpr bool OR(bool a, bool b) { return a || b; }
  static constexpr bool NOT(bool a) { return !a; }
};

// STATIC "IS A" CHECK:
static_assert(LogicalSpecies<ClassicalLogic>,
              "ClassicalLogic must fulfill LogicalSpecies");

/**
 * @section Species 2: Kleene Ternary Logic (The Logic of Indeterminacy)
 * @brief A three-valued propositional logic for handling partial information.
 *
 * Unlike ClassicalLogic, Ternary logic allows for an 'Unknown' state,
 * modeling undecidability or missing knowledge within a predicate.
 * This implementation follows Kleene's strong logic of indeterminacy (K3).
 *
 * @values
 * - False (-1): The absolute negative.
 * - Unknown (0): The indeterminate or undecidable state.
 * - True (1): The absolute positive.
 */
export enum class Ternary : int8_t { False = -1, Unknown = 0, True = 1 };

/**
 * @brief The internal logic of the Ternary Topos.
 *
 * Maps logical morphisms to numerical min/max/negation operations
 * over the {-1, 0, 1} lattice. This ensures that 'Unknown' acts as
 * a neutral element in specific contexts while 'False' remains
 * an annihilator for conjunction.
 *
 * The intention is to allow the type system to say that some predicate is not
 * computable.
 */
export struct TernaryLogic final {
  using type = Ternary;

  static constexpr Ternary True = Ternary::True;
  static constexpr Ternary False = Ternary::False;
  static constexpr Ternary Unknown = Ternary::Unknown;

  /** @brief Kleene Conjunction: Returns the minimum truth value. */
  static constexpr Ternary AND(Ternary a, Ternary b) {
    return static_cast<Ternary>(
        std::min(static_cast<int8_t>(a), static_cast<int8_t>(b)));
  }

  /** @brief Kleene Disjunction: Returns the maximum truth value. */
  static constexpr Ternary OR(Ternary a, Ternary b) {
    return static_cast<Ternary>(
        std::max(static_cast<int8_t>(a), static_cast<int8_t>(b)));
  }

  /** @brief Kleene Negation: Returns the additive inverse (rotation about
   * Unknown). */
  static constexpr Ternary NOT(Ternary a) {
    return static_cast<Ternary>(-static_cast<int8_t>(a));
  }
};

// STATIC "IS A" CHECK:
static_assert(LogicalSpecies<TernaryLogic>,
              "TernaryLogic must fulfill LogicalSpecies");

export constexpr Ternary operator&&(Ternary a, Ternary b) {
  return TernaryLogic::AND(a, b);
}
export constexpr Ternary operator||(Ternary a, Ternary b) {
  return TernaryLogic::OR(a, b);
}
export constexpr Ternary operator!(Ternary a) { return TernaryLogic::NOT(a); }

/**
 * @brief The Logic Species Registry (The Ontology Bridge).
 *
 * `SpeciesTraits` is the central mapping mechanism that connects a raw data
 * representation (the 'Codomain') to its governing Logic Species (the 'Rules').
 *
 * @section Extension_Point Registering Custom Logics
 * To introduce a new logical system (e.g., Fuzzy Logic, Łukasiewicz Logic,
 * or Quantum Logic) into the Dedekind framework, users must:
 * 1. Define a struct that fulfills the `LogicalSpecies` concept.
 * 2. Provide a template specialization of `SpeciesTraits` for the new
 *    underlying type.
 *
 * Once registered, the Point-Free Infix Engine will automatically detect
 * and apply the correct logical morphisms during DAG synthesis.
 *
 * @tparam T The raw truth-value type (e.g., bool, float, Ternary).
 */
template <typename T>
struct SpeciesTraits;

/** @brief Specialization for the Classical (Boolean) Topos. */
template <>
struct SpeciesTraits<bool> {
  using species = ClassicalLogic;
};

/** @brief Specialization for the Kleene (Ternary) Topos. */
template <>
struct SpeciesTraits<Ternary> {
  using species = TernaryLogic;
};

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
 * @req { p(x) } The candidate must be callable with an instance of the Domain.
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
concept IsPredicate =
    requires(P p, T x) {
      // 1. Must be callable with T
      { p(x) };
      // 2. The result MUST have a mapping in our Logic Ontology
      typename SpeciesTraits<decltype(p(x))>::species;
    } &&
    LogicalSpecies<
        typename SpeciesTraits<decltype(p(std::declval<T>()))>::species>;

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

/** @brief Logical Conjunction (Intersection): Synthesizes a rule for A ∩ B. */
export template <typename T, typename P1, typename P2>
  requires IsPredicate<P1, T> && IsPredicate<P2, T>
auto operator&&(P1&& p1, P2&& p2) {
  return [p1 = std::forward<P1>(p1),
          p2 = std::forward<P2>(p2)](auto&& x) mutable -> decltype(p1(x)) {
    using S = typename SpeciesTraits<decltype(p1(x))>::species;
    return S::AND(p1(std::forward<decltype(x)>(x)),
                  p2(std::forward<decltype(x)>(x)));
  };
}

/** @brief Logical Disjunction (Union): Synthesizes a rule for A ∪ B. */
export template <typename T, typename P1, typename P2>
  requires IsPredicate<P1, T> && IsPredicate<P2, T>
auto operator||(P1&& p1, P2&& p2) {
  return [p1 = std::forward<P1>(p1),
          p2 = std::forward<P2>(p2)](auto&& x) mutable -> decltype(p1(x)) {
    using S = typename SpeciesTraits<decltype(p1(x))>::species;
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

/** @subsection Test 1: Boolean (Classical) Invariants */
static_assert((true && true) == true);
static_assert((true && false) == false);
static_assert(!true == false);
static_assert((true || false) == true);

/** @subsection Test 2: Ternary (Kleene) Invariants */
static_assert(
    [] {
      using enum Ternary;

      bool ok = true;
      ok &= ((True && Unknown) == Unknown);
      ok &= ((False && Unknown) == False);
      ok &= ((True || Unknown) == True);
      ok &= ((False || Unknown) == Unknown);
      ok &= (!Unknown == Unknown);
      ok &= (!True == False);
      ok &= (!False == True);

      // De Morgan's Law Proof: !(A && B) == !A || !B
      ok &= (!(True && Unknown) == (!True || !Unknown));

      return ok;
    }(),
    "Dedekind: Ternary Logic Invariants failed!");

}  // namespace dedekind::ontology
