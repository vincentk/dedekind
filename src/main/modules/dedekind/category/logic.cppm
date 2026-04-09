/**
 * @file ontology:logic.cppm
 * @partition :logic
 * @brief Level -1: The Rules of Thought (Ω).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Algebraic_Logic
 * "Metoda algebraiczna w logice polega na traktowaniu każdego systemu
 *  logicznego jako pewnego określonego rodzaju algebry abstrakcyjnej."
 *  (The algebraic method in logic consists in treating every logical system
 *  as a specific type of abstract algebra.)
 *  — Helena Rasiowa
 *
 * @details
 * Before we can define a "Body" (Set) or a "Path" (Sequence), we must
 * establish the "Rules of Presence." This partition defines the logic species
 * that act as the truth-value objects (Ω) for all categorical predicates.
 *
 * By reifying logic into the @ref Truth wrapper, we prevent the "leaky
 * abstractions" of C++ machine types (such as integral promotion of bool)
 * while allowing for pluggable logical universes:
 * - ClassicalLogic: The Boolean Topos ({True, False}).
 * - TernaryLogic: The Kleene Topos ({True, False, Unknown}).
 *
 * @section Structural_Invariants
 * Logics in Dedekind are treated as Rigs (Semirings).
 * - Addition (+) is the Supremum/Join (OR).
 * - Multiplication (*) is the Infimum/Meet (AND).
 * - Successor (S) is the mapping x ∨ 1 (The Archimedean Step).
 *
 * Wikipedia: Subobject classifier, Topos theory, Kleene logic
 * @see Rasiowa, H. (1974). An Algebraic Approach to Non-Classical Logics.
 */
module;

#include <algorithm>
#include <cmath>
#include <concepts>
#include <functional>

export module dedekind.category:logic;

import :mereology;
import :morphism;
import :species;

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

/** @brief Helper to resolve logic species without hard errors */
export template <typename T>
struct GetLogic {
  using type = ClassicalLogic;
};

export template <typename T>
  requires requires { typename T::logic_species; }
struct GetLogic<T> {
  using type = typename T::logic_species;
};

/**
 * @concept LogicalValue
 * @brief Any type that serves as the Omega (Ω) for a Logical Species.
 * This is open-ended: if you register a FuzzyLogic, its 'type'
 * automatically becomes a LogicalValue.
 */
export template <typename T>
concept LogicalValue = requires {
  // We check if there exists a Logic Species L that uses T as its
  // representation.
  typename GetLogic<T>::type;
  requires LogicalSpecies<typename GetLogic<T>::type>;
};

/** @section Cardinality_Ontology_Tokens */
export enum class CardinalityTag { Finite, Countable, Continuum };

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
concept IsPredicate =
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
export template <typename P1, typename P2, typename T>
  requires IsPredicate<P1, T> && IsPredicate<P2, T>
auto operator&&(P1&& p1, P2&& p2) {
  return [p1 = std::forward<P1>(p1),
          p2 = std::forward<P2>(p2)](const T& x) mutable {
    using S = typename SpeciesTraits<decltype(p1(x))>::species;
    return S::AND(p1(x), p2(x));
  };
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

export template <typename TargetLogic, typename T>
constexpr auto lift_logic(T value) {
  if constexpr (std::is_same_v<TargetLogic, TernaryLogic> &&
                std::is_same_v<T, bool>) {
    return value ? Ternary::True : Ternary::False;
  } else {
    return value;
  }
}

/**
 * @class Truth
 * @brief The Monic Wrapper for a Logical Species (Ω).
 * @details Elevates raw types (bool, Ternary) into algebraic Rigs
 *          to prevent machine-level integral promotion.
 */
export template <typename L = ClassicalLogic>
struct Truth {
  using logic_species = L;
  using machine_type = typename L::type;

  machine_type value;

  /** @section Monic_Construction */
  // Removed 'explicit' to allow seamless return from lambdas/expressions
  constexpr Truth(machine_type v) noexcept : value(v) {}
  constexpr Truth() noexcept : value(L::False) {}

  // Unary Negation: Ensures !Boolean returns a Boolean, not a raw bool
  friend constexpr Truth operator!(Truth a) noexcept {
    return {L::NOT(a.value)};
  }

  /** @section Rig_Operations */

  // Addition as the Supremum (OR)
  friend constexpr Truth operator+(Truth a, Truth b) noexcept {
    return {L::OR(a.value, b.value)};
  }

  // Multiplication as the Infimum (AND)
  friend constexpr Truth operator*(Truth a, Truth b) noexcept {
    return {L::AND(a.value, b.value)};
  }

  friend constexpr bool operator<=(Truth a, Truth b) noexcept {
    // Universal Lattice Order: a <= b iff the Join of a and b is b.
    return (a + b) == b;
  }

  /** @section Identity_Discovery */
  template <typename Op>
  static constexpr auto identity_v = []() {
    if constexpr (std::is_same_v<Op, std::plus<Truth>> ||
                  std::is_same_v<Op, std::plus<void>>) {
      return Truth{L::False};
    } else if constexpr (std::is_same_v<Op, std::multiplies<Truth>> ||
                         std::is_same_v<Op, std::multiplies<void>>) {
      return Truth{L::True};
    }
  }();

  // The Archimedean Anchor (Successor = x + 1)
  static constexpr Truth one() { return {L::True}; }

  /** @section Conversion */
  constexpr explicit operator machine_type() const noexcept { return value; }
  constexpr bool operator==(const Truth&) const = default;
};

/** @section Logic_Species_Aliases */

/** @brief The Boolean Species (The Binary Prime). */
export using Boolean = Truth<ClassicalLogic>;

/** @brief The Kleene Species (The Indeterminacy). */
export using Kleene = Truth<TernaryLogic>;

/** @brief Bridge the Monic Wrapper to the Logic Species Registry. */
export template <typename L>
struct SpeciesTraits<Truth<L>> {
  using species = L;
  using Domain = typename L::type;
  using Codomain = typename L::type;
  static constexpr auto cardinality = CardinalityTag::Finite;
};

/**
 * @section Logic_Atlas_Bridge
 * Maps a Species to its specific Truth Object (Omega).
 * This bridge consumes the facts from the :species Atlas to determine
 * which logical system governs a given type.
 */

/** @brief Primary Template: Default to Classical (Boolean) Logic */
export template <typename T>
struct LogicTraits {
  using type = ClassicalLogic;
};

/** @brief Specialization for Floating-Point Species (IEEE 754 NaN handling) */
template <std::floating_point T>
struct LogicTraits<T> {
  using type = TernaryLogic;
};

/**
 * @brief Specialization for Signed Integrals (Lipschitz Boundary handling)
 * Note: We use Ternary here to represent the 'Unknown' state of an overflow.
 */
template <std::signed_integral T>
struct LogicTraits<T> {
  using type = TernaryLogic;
};

/** @brief Shorthand for the Logic Species of a type */
export template <typename T>
using LogicOf = typename LogicTraits<T>::type;

/** @brief Shorthand for the Subobject Classifier (Omega) of a species */
export template <typename T>
using Omega = typename LogicOf<T>::type;

/**
 * @section The_Subobject_Classifier
 * Formal elevation from Machine Result -> Omega.
 */
export template <IsSpecies T>
struct SubobjectClassifier {
  using L = typename LogicTraits<T>::type;
  using Omega = typename L::type;

  /** @brief Lipschitz Boundary Check for Signed Integers */
  template <typename Op>
    requires std::signed_integral<T>
  static constexpr Omega evaluate_arithmetic(T a, T b, Op) {
    // Use compiler built-ins for overflow detection (The Guardrail)
    T result;
    if (__builtin_add_overflow(a, b, &result)) {
      return L::Unknown;  // Lipschitz boundary breached
    }
    return L::True;  // Operation is safe/contained
  }

  /** @brief NaN Truth-Hole Check for IEEE 754 */
  template <typename Op>
    requires std::floating_point<T>
  static constexpr Omega evaluate_relational(T a, T b, Op rel) {
    if (std::isnan(a) || std::isnan(b)) {
      return L::Unknown;  // Singularity detected
    }
    return rel(a, b) ? L::True : L::False;
  }
};

/**
 * FIXME: Extension Point for Option-Logic.
 * In a future sprint (post-ETCS), consider specializing lift_logic for
 * std::optional<bool>. This would bridge the C++ 'missing value' semantics
 * with the Kleene 'Unknown' state, providing a functorial mapping from
 * the Standard Library to the Ternary Topos.
 */

}  // namespace dedekind::category
