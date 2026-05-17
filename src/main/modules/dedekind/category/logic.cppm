/**
 * @file dedekind/category/logic.cppm
 * @partition :logic
 * @brief The Rules of Thought (Ω).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section logic__Algebraic_Logic
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
 * @section logic__Structural_Invariants
 * Logics in Dedekind are treated as Rigs (Semirings).
 * - Addition (+) is the Supremum/Join (OR).
 * - Multiplication (*) is the Infimum/Meet (AND).
 * - Successor (S) is the mapping x ∨ 1 (The Archimedean Step).
 *
 * Textbook defaults in this partition:
 * - Classical two-valued logic uses C++ `operator&&` / `operator||`.
 * - Kleene K3 uses lattice operations `std::ranges::min` / `std::ranges::max`
 *   over {-1, 0, 1}.
 *
 * Wikipedia: Subobject classifier, Topos theory, Kleene logic
 * @see Rasiowa, H. (1974). An Algebraic Approach to Non-Classical Logics.
 * @see Lambek, J.; Scott, P. J. (1988). Introduction to Higher-Order
 * Categorical Logic.
 *
 * @note "Every kind of science, if it has only reached a certain degree of
 * maturity, automatically becomes a part of mathematics."
 *       -- David Hilbert, Axiomatic Thought (1918)
 */
module;

#include <algorithm>
#include <cmath>
#include <compare>  // std::strong_ordering — operator<=> on Ternary.
#include <concepts>
#include <cstdint>  // std::int8_t — Ternary int cast for the <=> body.
#include <functional>

export module dedekind.category:logic;

import :mereology;
import :morphism;
import :species;

namespace dedekind::category {

/**
 * @brief The Logical Species Concept (The Algebraic Signature of Truth).
 *
 * A type fulfills `IsLogicalSpecies` if it defines a consistent internal logic
 * over a specific 'type' of truth value. In categorical terms, this defines
 * the structure of the Subobject Classifier (Ω).
 *
 * @tparam L The Logic Species (e.g., ClassicalLogic, TernaryLogic).
 *
 * @req L::Ω The underlying data representation (e.g., bool, enum).
 * @req L::AND(a, b) The infimum (conjunction) morphism.
 * @req L::OR(a, b)  The supremum (disjunction) morphism.
 * @req L::NOT(a)    The negation (complement) morphism.
 *
 * @note This concept uses `std::same_as` to enforce strict species integrity;
 * logic operations must not result in type-decay or "species-leak."
 */
export template <typename L>
concept IsLogicalSpecies = requires(typename L::Ω a, typename L::Ω b) {
  typename L::Ω;
  { L::AND(a, b) } -> std::same_as<typename L::Ω>;
  { L::OR(a, b) } -> std::same_as<typename L::Ω>;
  { L::NOT(a) } -> std::same_as<typename L::Ω>;

  // The Categorical Constants (True/False)
  { L::True } -> std::convertible_to<typename L::Ω>;
  { L::False } -> std::convertible_to<typename L::Ω>;
};

/**
 * @section logic__Species
 * @brief The internal logic of the Classical Topos ({True, False}).
 *
 * ClassicalLogic defines the standard Boolean algebra where the Law of
 * Excluded Middle holds. It maps the structuralist AND/OR/NOT morphisms
 * directly to C++ hardware-level logical primitives.
 *
 * @note This species is the "Zero-Cost" foundation for standard set operations.
 * Because it uses `bool`, the compiler can often resolve these operations
 * into single bitwise assembly instructions during DAG pruning.
 *
 * Textbook term: the two-element Boolean algebra.
 */
export struct ClassicalLogic final {
  using Ω = bool;  // Renamed from 'type'
  static constexpr bool True = true;
  static constexpr bool False = false;

  static constexpr bool AND(bool a, bool b) { return a && b; }
  static constexpr bool OR(bool a, bool b) { return a || b; }
  static constexpr bool NOT(bool a) { return !a; }
};

// STATIC "IS A" CHECK:
static_assert(IsLogicalSpecies<ClassicalLogic>,
              "ClassicalLogic must fulfill IsLogicalSpecies");

/**
 * @section logic__Species_2
 * Indeterminacy)
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
 *
 * Textbook term: Kleene's strong three-valued logic (K3).
 */
export struct TernaryLogic final {
  using Ω = Ternary;  // Renamed from 'type'

  static constexpr Ternary True = Ternary::True;
  static constexpr Ternary False = Ternary::False;
  static constexpr Ternary Unknown = Ternary::Unknown;

  /** @brief Kleene Conjunction: Returns the minimum truth value. */
  static constexpr Ternary AND(Ternary a, Ternary b) {
    return static_cast<Ternary>(
        std::ranges::min(static_cast<int8_t>(a), static_cast<int8_t>(b)));
  }

  /** @brief Kleene Disjunction: Returns the maximum truth value. */
  static constexpr Ternary OR(Ternary a, Ternary b) {
    return static_cast<Ternary>(
        std::ranges::max(static_cast<int8_t>(a), static_cast<int8_t>(b)));
  }

  /** @brief Kleene Negation: Returns the additive inverse (rotation about
   * Unknown). */
  static constexpr Ternary NOT(Ternary a) {
    return static_cast<Ternary>(-static_cast<int8_t>(a));
  }
};

// STATIC "IS A" CHECK:
static_assert(IsLogicalSpecies<TernaryLogic>,
              "TernaryLogic must fulfill IsLogicalSpecies");

export constexpr Ternary operator&&(Ternary a, Ternary b) {
  return TernaryLogic::AND(a, b);
}
export constexpr Ternary operator||(Ternary a, Ternary b) {
  return TernaryLogic::OR(a, b);
}
export constexpr Ternary operator!(Ternary a) { return TernaryLogic::NOT(a); }

/** @brief Truth-order @c <=> on @c Ternary: the chain
 *         @c False @c (-1) @c < @c Unknown @c (0) @c < @c True @c (1).
 *
 *  Enables stdlib niebloids (@c std::ranges::min, @c std::ranges::max)
 *  to compute the Kleene meet / join on @c Ternary directly, so the
 *  Form-chain @c Meet / @c Join slots reuse stdlib infrastructure
 *  rather than carrying named Ternary-specific function-object struct
 *  types (#698 Slice 8 review).  @c min on the chain is Kleene AND;
 *  @c max is Kleene OR — identical to @c TernaryLogic::AND / @c OR
 *  (which were already defined via @c std::ranges::min / @c max on
 *  the int8_t cast).
 *
 *  @note Returns @c std::strong_ordering, not @c Ternary — comparison
 *  between two @c Ternary values is itself classically decided (the
 *  truth ordering is total).  @c Unknown values in @c Ternary arise
 *  from undecidable predicates over an intensional ambient, not from
 *  comparing two @c Ternary values directly. */
export constexpr std::strong_ordering operator<=>(Ternary a,
                                                  Ternary b) noexcept {
  return static_cast<std::int8_t>(a) <=> static_cast<std::int8_t>(b);
}

/** @brief Helper to resolve logic species without hard errors */
export template <typename T>
struct GetLogic {
  using type = ClassicalLogic;
};

export template <>
struct GetLogic<Ternary> {
  using type = TernaryLogic;
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
  requires IsLogicalSpecies<typename GetLogic<T>::type>;
};

/**
 * @concept LogicalMap
 * @brief A callable Pred that maps T -> Ω for some LogicalValue Ω.
 * Captures the notion of a predicate valued in an arbitrary logic species.
 */
export template <typename Pred, typename T>
concept LogicalMap =
    std::invocable<const std::decay_t<Pred>&, const T&> &&
    LogicalValue<std::remove_cvref_t<
        std::invoke_result_t<const std::decay_t<Pred>&, const T&>>>;

/** @brief Extract the Ω-type of a LogicalMap. */
export template <typename Pred, typename T>
  requires LogicalMap<Pred, T>
using OmegaOf = std::remove_cvref_t<
    std::invoke_result_t<const std::decay_t<Pred>&, const T&>>;

/** @section logic__Cardinality_Ontology_Tokens */
export enum class CardinalityTag { Finite, Countable, Continuum };

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
  using machine_type = typename L::Ω;

  machine_type value;

  /** @section logic__Monic_Construction */
  // Removed 'explicit' to allow seamless return from lambdas/expressions
  constexpr Truth(machine_type v) noexcept : value(v) {}
  constexpr Truth() noexcept : value(L::False) {}

  // Unary Negation: Ensures !Boolean returns a Boolean, not a raw bool
  friend constexpr Truth operator!(Truth a) noexcept {
    return {L::NOT(a.value)};
  }

  /** @section logic__Rig_Operations */

  // Addition as the Supremum (OR)
  friend constexpr Truth operator+(Truth a, Truth b) noexcept {
    return {L::OR(a.value, b.value)};
  }

  // Multiplication as the Infimum (AND)
  friend constexpr Truth operator*(Truth a, Truth b) noexcept {
    return {L::AND(a.value, b.value)};
  }

  friend constexpr Truth operator<=(Truth a, Truth b) noexcept {
    // Universal Lattice Order: a <= b iff the Join of a and b is b.
    return {lift_logic<L>((a + b) == b)};
  }

  /** @section logic__Identity_Discovery */
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

  /** @section logic__Conversion */
  constexpr explicit operator machine_type() const noexcept { return value; }
  constexpr bool operator==(const Truth&) const = default;
};

/** @section logic__Logic_Species_Aliases */

/** @brief The Boolean Species (The Binary Prime). */
export using Boolean = Truth<ClassicalLogic>;

/** @brief The Kleene Species (The Indeterminacy). */
export using Kleene = Truth<TernaryLogic>;

/**
 * @brief Semantic truth projection for assertion contexts.
 * @details
 * Textbook alignment: in an internal logic, formulas denote Ω-values.
 * `holds` projects that Ω-value to meta-level proof truth.
 */
export template <typename L>
constexpr bool holds(Truth<L> proposition) noexcept {
  return proposition.value == L::True;
}

/**
 * @brief Semantic falsity projection for assertion contexts.
 * @details
 * In non-classical logics (e.g., Kleene K3), `refutes` is distinct from
 * `!holds`: Unknown is neither proven true nor proven false.
 */
export template <typename L>
constexpr bool refutes(Truth<L> proposition) noexcept {
  return proposition.value == L::False;
}

/** @brief Bridge the Monic Wrapper to the Logic Species Registry. */
export template <typename L>
struct SpeciesTraits<Truth<L>> {
  using species = L;
  using Domain = typename L::Ω;
  using Codomain = typename L::Ω;
  static constexpr auto cardinality = CardinalityTag::Finite;
};

/**
 * @section logic__Logic_Atlas_Bridge
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
using Omega = typename LogicOf<T>::Ω;

/**
 * @section logic__The_Subobject_Classifier
 * Formal elevation from Machine Result -> Omega.
 */
export template <IsSpecies T>
struct SubobjectClassifier {
  using L = typename LogicTraits<T>::type;
  using Omega = typename L::Ω;

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

/**
 * @concept HasLogicalOperators
 * @brief @b Pure @b syntactic @b shape: T supports the logical
 *        operators @c &&, @c ||, @c ! with closed results.
 *
 * @details
 * Use this concept where the callsite needs Boolean-flavoured logical
 * operators (rather than the bitwise lattice operators of
 * @c dedekind::order::HasLatticeOperators) --- @c bool, @c Ternary,
 * predicate carriers, Kleene three-valued logic.  No axiomatic claim
 * is made about truth tables or excluded-middle.  Note also that
 * short-circuit evaluation of @c && / @c || is guaranteed only for
 * the @b built-in operators on @c bool; once @c && / @c || are
 * @b overloaded for a user-defined @c T they evaluate like ordinary
 * functions (both operands always evaluated, in unspecified order),
 * so this concept makes no short-circuit claim either.  Sibling of
 * @c dedekind::algebra::HasRingOperators (in @c algebra:ring) and
 * @c dedekind::order::HasLatticeOperators (in @c order:lattice) in
 * the shape-concept family --- introduced under #393.
 */
export template <typename T>
concept HasLogicalOperators = requires(T a, T b) {
  { a && b } -> std::same_as<T>;
  { a || b } -> std::same_as<T>;
  { !a } -> std::same_as<T>;
};

/** @section logic__Formal_Verification */

// Pure-syntactic-shape witness: bool is the canonical fit because
// bool && bool, bool || bool, !bool all return bool.  int does NOT
// satisfy this concept --- a && b on ints yields bool, not int ---
// which is the correct behaviour for a strictly-closing shape concept.
static_assert(HasLogicalOperators<bool>,
              "bool has the syntactic logical-operator surface "
              "(&&, ||, ! all close to bool; short-circuit evaluation "
              "is the built-in-operator behaviour, not a concept claim).");

}  // namespace dedekind::category
