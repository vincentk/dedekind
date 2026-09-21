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
import :involution;  // is_involutive / IsInvolution: witness that the logic
                     // negation ¬ = L::RFL is an involution (¬¬ = id)

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
 * @req L::RFL(a)    The reflection: the order-reversing involution (a
 *                   complement only on the two-chain 𝔹).
 *
 * @note This concept uses `std::same_as` to enforce strict species integrity;
 * logic operations must not result in type-decay or "species-leak."
 */
export template <typename L>
concept IsLogicalSpecies = requires(typename L::Ω a, typename L::Ω b) {
  typename L::Ω;
  { L::AND(a, b) } -> std::same_as<typename L::Ω>;
  { L::OR(a, b) } -> std::same_as<typename L::Ω>;
  { L::RFL(a) } -> std::same_as<typename L::Ω>;

  // The Categorical Constants (True/False)
  { L::True } -> std::convertible_to<typename L::Ω>;
  { L::False } -> std::convertible_to<typename L::Ω>;
};

/**
 * @section logic__Species
 * @brief The internal logic of the Classical Topos ({True, False}).
 *
 * ClassicalLogic defines the standard Boolean algebra where the Law of
 * Excluded Middle holds. It maps the structuralist AND/OR/RFL morphisms
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
  static constexpr bool RFL(bool a) { return !a; }
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
export enum class Ternary : std::int8_t { False = -1, Unknown = 0, True = 1 };

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
    return static_cast<Ternary>(std::ranges::min(static_cast<std::int8_t>(a),
                                                 static_cast<std::int8_t>(b)));
  }

  /** @brief Kleene Disjunction: Returns the maximum truth value. */
  static constexpr Ternary OR(Ternary a, Ternary b) {
    return static_cast<Ternary>(std::ranges::max(static_cast<std::int8_t>(a),
                                                 static_cast<std::int8_t>(b)));
  }

  /** @brief Kleene reflection: the order-reversing involution, here the
   * additive inverse (reflection about Unknown; fixes U, swaps ⊥/⊤). */
  static constexpr Ternary RFL(Ternary a) {
    return static_cast<Ternary>(-static_cast<std::int8_t>(a));
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
export constexpr Ternary operator!(Ternary a) { return TernaryLogic::RFL(a); }

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

/**
 * @concept IsΩ
 * @brief A truth-object: a type that can serve as the classifier Ω for a
 *        logical species.
 *
 * @details Satisfied when @c T either carries the @b closed logical operators
 * (@c && / @c || / @c ! all returning @c T, as for @c bool and @c Ternary), or
 * is a registered logic wrapper declaring a valid @c logic_species (e.g.\ @c
 * Truth<L>).  @c int and @c std::string satisfy neither (@c int's @c && yields
 * @c bool, and neither declares a @c logic_species), so @c IsΩ does not
 * over-accept them.  This is deliberately decoupled from @c GetLogic, whose
 * permissive default maps any type to @c ClassicalLogic and would otherwise let
 * @c IsΩ accept arbitrary types.
 *
 * @note @c bool and @c Ternary are @e peer inhabitants: neither is a privileged
 * "the Ω".  @c bool is distinguished only at the dominance layer (as the
 * decided core @f$\mathbb{B}@f$ that decidable maps factor through; see @c
 * lift_logic), not as a logic.  Register a new species (say a fuzzy logic) and
 * its truth-type joins as another peer.  @c Ternary (Kleene @f$K_3@f$) is the
 * one non-trivial inhabitant currently shipped, the honest default, not the
 * only possible Ω.
 */
export template <typename T>
concept IsΩ =
    // Raw truth-type: the logical operators close on T (bool, Ternary)...
    HasLogicalOperators<T> ||
    // ...or a registered logic wrapper declaring a valid logic_species
    // (e.g. Truth<L>), which need not overload the operators directly.
    requires {
      typename T::logic_species;
      requires IsLogicalSpecies<typename T::logic_species>;
    };

/**
 * @concept IsPst
 * @brief A truth-object that is a @b bounded @b chain: an @c IsΩ classifier
 * whose values are totally ordered.
 *
 * @details @f$\mathbf{Pst} := \mathbf{Jlt} \cap \mathbf{Chain}@f$ (named for
 * Post's many-valued logics): the @c IsΩ truth-objects whose order is total, so
 * @f$\wedge = \min@f$, @f$\vee = \max@f$ on the chain @f$\bot < \cdots <
 * \top@f$, and @f$\neg@f$ is the order-reversing reflection.  @c bool
 * (@f$\mathbb{B}@f$, the two-chain) and @c Ternary (Kleene @f$K_3@f$, the
 * three-chain) both satisfy it; a truth-object valued in a non-chain lattice (a
 * four-element Boolean algebra, Belnap's bilattice) would be @c IsΩ but @b not
 * @c IsPst.  This is the classifier layer's carrier constraint: @c IsPst pins
 * @e which chains can serve as @f$\Omega@f$ from a type constraint alone (see
 * @c lift_logic for the dominance @f$\Sigma \hookrightarrow \Omega@f$ these
 * chains support).
 *
 * @note This is a @b shape gate, exactly as @c IsΩ is: it certifies the @e
 * surface (a truth-object that is @c std::totally_ordered), the definitional
 * @f$\mathbf{Jlt} \cap \mathbf{Chain}@f$.  It admits @e any bounded chain, of
 * any cardinality: order alone makes the chain well-behaved, so finiteness is
 * @b not required.  @c bool and @c Ternary are the shipped (finite) chains; a
 * purpose-built @f$[0,1]@f$ fuzzy/G\"odel carrier under @f$\min/\max@f$ would
 * satisfy it too.  A truly non-chain logic (a four-element Boolean algebra,
 * Belnap's bilattice) is excluded because it is not @c std::totally_ordered.
 *
 * What the concept does @b not gate is the @b semantic @b chain @b law: that
 * this order @e is the truth-order, so @c AND / @c OR are @f$\min/\max@f$ under
 * it, @c RFL is the order-reversing reflection, and the bounds are
 * @f$\bot/\top@f$.  That law quantifies over values, so it cannot be a concept;
 * it is witnessed at compile time by the @b chain-law static_asserts below
 * (@c bool and @c Ternary) and by the §3.1 listing.  The residual gap is only a
 * pathological type carrying an @e unrelated total order.
 * FIXME(#854): the principled gate (a consolidated faithful-semilattice
 * primitive tying @c AND / @c OR to @f$\min/\max@f$ under the order and
 * supplying the bounds/reflection law) supersedes this shape gate; #854's
 * acceptance criteria are extended to cover the @c IsPst chain case.
 */
export template <typename T>
concept IsPst = IsΩ<T> && std::totally_ordered<T>;

/**
 * @concept LogicalMap
 * @brief A callable Pred that maps T -> Ω for some IsΩ Ω.
 * Captures the notion of a predicate valued in an arbitrary logic species.
 */
export template <typename Pred, typename T>
concept LogicalMap =
    std::invocable<const std::decay_t<Pred>&, const T&> &&
    IsΩ<std::remove_cvref_t<
        std::invoke_result_t<const std::decay_t<Pred>&, const T&>>>;

/** @brief Extract the Ω-type of a LogicalMap. */
export template <typename Pred, typename T>
  requires LogicalMap<Pred, T>
using OmegaOf = std::remove_cvref_t<
    std::invoke_result_t<const std::decay_t<Pred>&, const T&>>;

/** @section logic__Cardinality_Ontology_Tokens */
export enum class CardinalityTag { Finite, Countable, Continuum };

/**
 * @brief The Rosolini dominance inclusion @f$\iota : \mathbb{B} \hookrightarrow
 *        \Omega@f$, the decided core into a classifier.
 * @details The dominance is general in the classifier @f$\Omega@f$.
 *          @f$\mathbb{B}@f$ = @c ClassicalLogic::Ω = @c bool is the two-valued
 *          @b decided @b core @f$\{\top,\bot\}@f$ that sits inside @e every
 *          answer-lattice @f$\Omega@f$ (every @c IsΩ), and @f$\iota@f$
 *          is its inclusion.  So @f$\mathbb{B}@f$ is @e primus @e inter @e
 * pares among the truth-objects: a peer of any other @f$\Omega@f$ at the object
 * layer, but the one target every decidable map factors through (the Rosolini
 * dominance @f$\Sigma@f$).  @c Ternary (Kleene
 *          @f$K_3@f$) is the single non-trivial @f$\Omega@f$ we currently ship:
 * a peer of @f$\mathbb{B}@f$, @b not the canonical @f$\Omega@f$; for it
 * @f$\iota@f$ is the concrete map @c bool @c ↪ @c Ternary
 *          (@c Ternary = @f$\mathbb{B} + 1@f$, adjoining @c Unknown), while the
 *          concept @c IsDominanceInclusion fixes only the shape
 *          @f$\mathbb{B} \to \Omega@f$; a future @f$\Omega@f$ is admitted by
 * that shape but supplies its own @f$\mathbb{B}@f$-inclusion (@c lift_logic
 *          currently embeds only @c bool @c ↪ @c Ternary, returning other
 *          values unchanged).  @f$\top \in \mathbb{B}@f$ and
 *          @f$\mathbb{B}@f$ is closed under dependent conjunction, so a
 *          @f$\mathbb{B}@f$-valued map is @b decidable: a set whose
 *          characteristic map factors as
 *          @f$A \to \mathbb{B} \xrightarrow{\iota} \Omega@f$ never answers
 *          @c Unknown.  @c HasDecidableMembership is a @b sound, @b
 * conservative certificate of that factorisation (see @c
 * sets/computability.cppm), not a decision of it (Rice): a ternary-tagged map
 * that never returns
 *          @c Unknown factors through @f$\mathbb{B}@f$ yet the observable stays
 *          false.  ETCS proper is the degenerate case @f$\mathbb{B} =
 * \Omega@f$.
 * @see Giuseppe Rosolini, @e Continuity @e and @e Effectiveness @e in @e Topoi,
 *      D.Phil. thesis, University of Oxford, 1986 --- the origin of the
 *      @b dominance @f$\Sigma@f$ (Rosolini is at the Università di Genova).
 *      E. Robinson & G. Rosolini, @e Categories @e of @e Partial @e Maps,
 *      Information and Computation 79(2):95--130, 1988.
 * @see Design note and Rosolini↔codebase dictionary (the theory this concept
 *      instantiates, not an improvisation):
 *      https://github.com/vincentk/dedekind/issues/267#issuecomment-5711242416
 *      The factorisation @f$A \to \Sigma \to \Omega@f$ is named
 *      @c sets::IsDecidableSet / @c HasDecidableMembership; the classifier
 *      arrows are @c IsCharacteristic / @c IsDecidableCharacteristic and this
 *      inclusion is @c IsDominanceInclusion (all #846).
 */
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
    return {L::RFL(a.value)};
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

/** @section logic__Formal_Verification */

// Pure-syntactic-shape witness: bool is the canonical fit because
// bool && bool, bool || bool, !bool all return bool.  int does NOT
// satisfy this concept --- a && b on ints yields bool, not int ---
// which is the correct behaviour for a strictly-closing shape concept.
static_assert(HasLogicalOperators<bool>,
              "bool has the syntactic logical-operator surface "
              "(&&, ||, ! all close to bool; short-circuit evaluation "
              "is the built-in-operator behaviour, not a concept claim).");

// IsΩ gate (the truth-object concept): raw truth-types qualify via closed
// operators; the Truth<L> wrappers via their registered logic_species; and
// non-truth types (int, ...) qualify by neither.
static_assert(IsΩ<bool> && IsΩ<Ternary>,
              "raw truth-types are Ω (their &&/||/! close on the type)");
static_assert(IsΩ<Boolean> && IsΩ<Kleene>,
              "Truth<L> wrappers are Ω via their registered logic_species "
              "(they overload +/* and !, not &&/||)");
static_assert(!IsΩ<int>,
              "int is not Ω: its && yields bool (not int) and it declares no "
              "logic_species");

// IsPst gate (Pst = Jlt ∩ Chain): the truth-objects that are bounded chains.
// bool (𝔹) and Ternary (K₃) are totally ordered classifiers; int is totally
// ordered but not IsΩ, so the intersection excludes it.
static_assert(IsPst<bool> && IsPst<Ternary>,
              "𝔹 and K₃ are bounded chains: IsΩ and totally ordered");
static_assert(!IsPst<int>,
              "int is a chain but not a truth-object (not IsΩ), so not Pst");

// Chain-law witnesses: the *semantics* IsPst names but cannot gate.  On each
// shipped chain, under the truth-order, AND = min, OR = max, RFL is the
// order-reversing reflection, and the bounds are ⊥/⊤.  A regression in a
// species operator (say AND stops being min) surfaces here at compile time.
// 𝔹 = {false < true}: RFL swaps the endpoints (a genuine complement).
static_assert(false < true);  // ⊥ < ⊤
static_assert((true && false) == false && (true && true) == true,
              "𝔹: AND = min");
static_assert((false || true) == true && (false || false) == false,
              "𝔹: OR = max");
static_assert(!false == true && !true == false, "𝔹: RFL reflects the 2-chain");
// K₃ = {False < Unknown < True}: RFL reflects about Unknown (¬U = U).
static_assert(Ternary::False < Ternary::Unknown &&
                  Ternary::Unknown < Ternary::True,
              "K₃: ⊥ < U < ⊤");
static_assert(TernaryLogic::AND(Ternary::True, Ternary::Unknown) ==
                      Ternary::Unknown &&
                  TernaryLogic::AND(Ternary::False, Ternary::Unknown) ==
                      Ternary::False,
              "K₃: AND = min");
static_assert(TernaryLogic::OR(Ternary::False, Ternary::Unknown) ==
                      Ternary::Unknown &&
                  TernaryLogic::OR(Ternary::True, Ternary::Unknown) ==
                      Ternary::True,
              "K₃: OR = max");
static_assert(TernaryLogic::RFL(Ternary::True) == Ternary::False &&
                  TernaryLogic::RFL(Ternary::False) == Ternary::True &&
                  TernaryLogic::RFL(Ternary::Unknown) == Ternary::Unknown,
              "K₃: RFL reflects about U (¬U = U)");

/** @brief The logic negation ¬ = @c L::RFL as a callable object.  It exists so
 *  the @c :involution machinery can witness that the negation is an involution
 *  (@c ¬¬ = @c id).  @c :sets consults the witness to eliminate double negation
 *  at the type level (@c !!A ≡ A). */
export template <typename L>
struct logic_complement {
  constexpr typename L::Ω operator()(typename L::Ω a) const {
    return L::RFL(a);
  }
};

/** @brief Witness: Boolean negation is an involution.  @c ClassicalLogic's
 *  @c RFL is @c std::logical_not on @c bool (@c !!b = b). */
template <>
struct is_involutive<logic_complement<ClassicalLogic>, bool> : std::true_type {
};

/** @brief Witness: Kleene negation is an involution.  @c TernaryLogic's @c RFL
 *  reflects the K₃ chain about @c Unknown, so @c ¬¬a = a on all three values
 *  (the static_asserts above prove it). */
template <>
struct is_involutive<logic_complement<TernaryLogic>, Ternary> : std::true_type {
};

/** @brief @c true iff the logic negation ¬ = @c L::RFL is a certified
 *  involution.  Both shipped De Morgan logics (@c 𝔹, @c K₃) qualify; a future
 *  intuitionistic species whose ¬¬ is only a closure would not.  Downstream
 *  double-negation elimination gates on this so it stays honest per logic. */
export template <typename L>
inline constexpr bool logic_negation_is_involutive_v =
    IsInvolution<logic_complement<L>, typename L::Ω>;

static_assert(logic_negation_is_involutive_v<ClassicalLogic>,
              "𝔹: ¬ is an involution, so !!A = A is sound");
static_assert(logic_negation_is_involutive_v<TernaryLogic>,
              "K₃: ¬ is an involution, so !!A = A is sound");

/**
 * @brief Membership in the decided core @f$\Sigma \sqcup \neg\Sigma = \{\top,
 *        \bot\}@f$ of an answer-lattice @f$\Omega@f$.
 *
 * @details The Rosolini dominance @f$\Sigma \hookrightarrow \Omega@f$ detects
 * the @f$\top@f$ endpoint alone; it is a meet-semilattice, not the full decided
 * core.  Its Kleene reflection @f$\neg\Sigma@f$ detects the @f$\bot@f$
 * endpoint, and the two join to @f$\{\top, \bot\}@f$: the two-valued fragment a
 * decidable map factors through.  This is the reflection completing Rosolini's
 * semilattice to the full lattice.
 *
 * The @f$\bot@f$-detection is spelled @c L::RFL(value) @c == @c L::True rather
 * than @c value @c == @c L::False on purpose: the two agree @b because @c RFL
 * is an order-reversing @b involution (@c logic_negation_is_involutive_v, the
 * @c :involution witness from the double-negation work), which the @c requires
 * clause demands.  A logic whose @c RFL were only a closure would not admit the
 * reflection-completion, and is excluded.
 *
 * The answer is itself classical: a bound either is or is not reached, never
 * @c Unknown.  So @c is_decided co-restricts any @f$\Omega@f$ to @c bool, the
 * value-level observable behind @c HasDecidableMembership (the type-level,
 * conservative certificate).  On @c ClassicalLogic (@f$\Sigma = \Omega@f$) it
 * is constantly @c true; on @c K₃ it is @c true off @c Unknown.
 *
 * @see Giuseppe Rosolini, @e Continuity @e and @e Effectiveness @e in @e Topoi
 *      (Oxford D.Phil., 1986); @c lift_logic (the inclusion @f$\Sigma
 *      \hookrightarrow \Omega@f$), #846 / #267, and the #847
 * recognised-vs-actual sub-quadrant this closes at the value level.
 */
export template <typename L>
  requires IsLogicalSpecies<L> && logic_negation_is_involutive_v<L>
constexpr bool is_decided(typename L::Ω value) {
  return value == L::True || L::RFL(value) == L::True;
}

static_assert(is_decided<ClassicalLogic>(true) &&
                  is_decided<ClassicalLogic>(false),
              "𝔹: Σ = Ω, so every answer is decided");
static_assert(is_decided<TernaryLogic>(Ternary::True) &&
                  is_decided<TernaryLogic>(Ternary::False),
              "K₃: the two endpoints ⊤, ⊥ are the decided core");
static_assert(!is_decided<TernaryLogic>(Ternary::Unknown),
              "K₃: the interior Unknown is undecided (outside Σ ⊔ ¬Σ)");

}  // namespace dedekind::category
