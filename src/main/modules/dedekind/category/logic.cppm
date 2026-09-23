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
 * - Boole: The Boolean Topos ({True, False}).
 * - Kleene: The Kleene Topos ({True, False, Unknown}).
 *
 * @section logic__Structural_Invariants
 * The @b shipped logic species (@c Boole, @c Kleene, @c Chain<T>) are @b De
 * @b Morgan @b algebras: a bounded distributive lattice with an order-reversing
 * involution, named by their morphisms @c AND / @c OR / @c RFL.  (The bare
 * @c IsOckhamAlgebra signature admits other logics too, e.g. a future
 * non-involutive intuitionistic species; De Morgan is a property of the shipped
 * family, not of the signature.)
 * - @c OR is the supremum / join (∨).
 * - @c AND is the infimum / meet (∧).
 * - @c RFL is the reflection / De Morgan involution (¬); a genuine complement
 *   only on the two-chain 𝔹.
 * The shipped species are bounded chains (@c Boole = 2 grades, @c Kleene = 3,
 * @c Chain<T> = |T|), hence Kleene lattices; see @c IsDeMorganAlgebra /
 * @c IsBoundedDeMorganChain / @c IsBooleanLogic below (#901).  The former rig
 * (@c + / @c *) surface on @c Truth was retired: a truth value is a lattice
 * element, not a semiring element.
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
#include <limits>       // std::numeric_limits: Chain<T> bounds (INT_MIN/MAX).
#include <type_traits>  // std::remove_cv_t (reject cv-qualified Chain carriers).

export module dedekind.category:logic;

import :mereology;
import :morphism;
import :species;
import :involution;  // is_involutive / IsInvolution: witness that the logic
                     // negation ¬ = L::RFL is an involution (¬¬ = id)

namespace dedekind::category {

/**
 * @brief The IsOckhamAlgebra Concept (The Algebraic Signature of Truth).
 *
 * A type fulfills `IsOckhamAlgebra` if it defines a consistent internal logic
 * over a specific 'type' of truth value. In categorical terms, this defines
 * the structure of the Subobject Classifier (Ω).
 *
 * @note IsOckhamAlgebra is a conservative SHAPE gate: it certifies the
 * (AND, OR, RFL, True, False) signature only. The Ockham laws proper (the
 * reflection reversing the bounds, distributivity, and the all-values
 * dual-endomorphism / De Morgan law) are NOT gated in the concept. An
 * in-concept attempt to require even the boundary pole-reflection law
 * (@c ¬⊤=⊥, @c ¬⊥=⊤) snagged on C++ overload resolution (value category plus
 * RFL-overload selection), so it was reverted (see #918): the codebase's
 * posture holds, a shape gate here with the laws witnessed downstream by the
 * @c static_assert tower and gated properly in #907 (which needs the
 * static-method-ops to :species-trait bridge, #923).
 *
 * @tparam L The Logic Species (e.g., Boole, Kleene).
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
concept IsOckhamAlgebra = requires(typename L::Ω a, typename L::Ω b) {
  typename L::Ω;
  { L::AND(a, b) } -> std::same_as<typename L::Ω>;
  { L::OR(a, b) } -> std::same_as<typename L::Ω>;
  { L::RFL(a) } -> std::same_as<typename L::Ω>;

  // The Categorical Constants (True/False)
  { L::True } -> std::convertible_to<typename L::Ω>;
  { L::False } -> std::convertible_to<typename L::Ω>;
};

/**
 * @section logic__Op_Type_Bridge (#923)
 * @brief Bridge the species' @b static-method ops (@c L::AND / @c L::OR /
 *        @c L::RFL) to the @b function-object @b types the @c :species /
 *        @c :total law registry is keyed on.
 *
 * @details The @c :species / @c :total law machinery
 * (@c is_distributive_v<T, Join, Meet>, @c identity_v<T, Op>,
 * @c IsDistributiveLattice / @c IsBoundedLattice) keys every law on the
 * operation's @b type (a function object such as @c std::logical_or<bool>,
 * @c dedekind::category::Sup, @c dedekind::category::Inf).  The logic species
 * carry their ops as @b static @b methods on @c L, which have no type the
 * registry can look up.  So each species publishes the op-@b types its
 * @c AND / @c OR / @c RFL @e are, as the member aliases
 * @c JoinOp @c (∨) / @c MeetOp @c (∧) / @c RflOp @c (¬):
 *
 *   - @c Boole:      @c JoinOp @c = @c std::logical_or<bool>, @c MeetOp @c =
 *     @c std::logical_and<bool> (its @c OR / @c AND @b are @c || / @c &&, and
 *     the Boolean-algebra + complement laws are registered under those types).
 *   - @c Kleene / @c Chain<T> / @c Percent: @c JoinOp @c = @c Sup,
 *     @c MeetOp @c = @c Inf (their @c OR / @c AND @b are @c max / @c min, the
 *     value-returning chain ops from @c :species, #912/#933).
 *   - @c RflOp @c = @c logic_complement<L> for every species: the reflection
 *     ¬ = @c L::RFL as a callable (already the @c :involution witness, see
 *     below), forward-declared here so the alias resolves.
 *
 * With these, @c is_distributive_v<L::Ω, L::JoinOp, L::MeetOp> and the whole
 * @c :total lattice ladder resolve @b structurally from the species' declared
 * ops --- no bespoke @c logic_is_distributive_v<L> tag ("inference composes,
 * tagging explodes").  The carrier-level variety witnesses
 * (@c IsDistributiveLattice / @c IsBoundedLattice) live in @c :total
 * (downstream), so they are pinned in @c logic_lattice_structure_test.cpp; the
 * @c :species-level trait witnesses are asserted below.  Consuming this bridge
 * to gate the full Ockham laws in the @c IsOckhamAlgebra family is #907.
 */
export template <typename L>
struct logic_complement;

/**
 * @section logic__Species
 * @brief The internal logic of the Classical Topos ({True, False}).
 *
 * Boole defines the standard Boolean algebra where the Law of
 * Excluded Middle holds. It maps the structuralist AND/OR/RFL morphisms
 * directly to C++ hardware-level logical primitives.
 *
 * @note This species is the "Zero-Cost" foundation for standard set operations.
 * Because it uses `bool`, the compiler can often resolve these operations
 * into single bitwise assembly instructions during DAG pruning.
 *
 * Textbook term: the two-element Boolean algebra.
 */
export struct Boole final {
  using Ω = bool;  // Renamed from 'type'
  static constexpr bool True = true;
  static constexpr bool False = false;

  static constexpr bool AND(bool a, bool b) { return a && b; }
  static constexpr bool OR(bool a, bool b) { return a || b; }
  static constexpr bool RFL(bool a) { return !a; }

  /** @section logic__Boole_Op_Types (#923)
   *  @brief The op-@b types @c OR / @c AND / @c RFL @b are, for the
   *  @c :species / @c :total law registry.  @c 𝔹's join / meet @b are
   *  @c || / @c && (@c std::logical_or / @c std::logical_and on @c bool),
   *  under which the distributive-, bounded- and Boolean-algebra laws (incl.
   *  the complement) are registered; ¬ is @c logic_complement<Boole>. */
  using JoinOp = std::logical_or<bool>;
  using MeetOp = std::logical_and<bool>;
  using RflOp = logic_complement<Boole>;
};

// STATIC "IS A" CHECK:
static_assert(IsOckhamAlgebra<Boole>, "Boole must fulfill IsOckhamAlgebra");

/**
 * @section logic__Species_2
 * Indeterminacy)
 * @brief A three-valued propositional logic for handling partial information.
 *
 * Unlike Boole, Kleene's three-valued logic allows for an 'Unknown' state,
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
 * @brief The internal logic of the Kleene Topos.
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
export struct Kleene final {
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

  /** @brief Kleene reflection: the order-reversing De Morgan involution
   * (fixes U, swaps ⊥/⊤).  Implemented as sign-flip on the balanced
   * @c {-1,0,+1} @c int8 encoding, so the @c -x below is the @b encoding's
   * additive inverse --- @b not a rig inverse: the rig addition is join
   * (@c max), which is idempotent and has no inverses. */
  static constexpr Ternary RFL(Ternary a) {
    return static_cast<Ternary>(-static_cast<std::int8_t>(a));
  }

  /** @section logic__Kleene_Op_Types (#923)
   *  @brief The op-@b types @c OR / @c AND / @c RFL @b are.  Kleene's join /
   *  meet @b are @c max / @c min on the @c K₃ chain, i.e. the value-returning
   *  @c Sup / @c Inf from @c :species (@b not the @c std::ranges::max / @c min
   *  niebloids, which return @c const @c T& and fail @c IsClosedUnder; #912/
   *  #933).  ¬ is @c logic_complement<Kleene>. */
  using JoinOp = Sup;
  using MeetOp = Inf;
  using RflOp = logic_complement<Kleene>;
};

// STATIC "IS A" CHECK:
static_assert(IsOckhamAlgebra<Kleene>, "Kleene must fulfill IsOckhamAlgebra");

/**
 * @section logic__Species_3
 * @brief @c Chain<T>: the finite Kleene chain over an integral carrier @c T,
 *        the full-@c T-range many-valued logic.  @c Boole (2 grades) and
 *        @c Kleene (3) are the small distinguished members; @c Chain<int> spans
 *        the implementation's full @c int range.
 *
 * @details Meet @c AND = @c min, join @c OR = @c max (the numeric order @b is
 * the truth order); reflection @c RFL = @c ~a (bitwise NOT), the
 *          order-reversing involution swapping @c ⊥ = @c numeric_limits::min
 * and
 *          @c ⊤ = @c numeric_limits::max.  A bounded De Morgan chain, hence
 *          @b Kleene (chain implies normality), @b not Boolean: only the two
 *          poles are complemented.
 *
 *          Signed @b and unsigned integrals both work (unsigned poles @c 0 /
 *          @c UMAX; signed @c INT_MIN / @c INT_MAX, where @c ~x @c = @c -1-x on
 *          two's complement).  @c bool is excluded (its @c ~ promotes, breaking
 *          @c RFL: @c Boole @b is the 2-chain).  Floating point is excluded by
 *          @c std::integral: totality needs @c min / @c max / @c ¬ everywhere,
 *          and @c NaN breaks the total order.
 *
 *          The bounds are the carrier's own @c min / @c max, so the value set
 *          @b is the chain.  A sub-interval like @c [0,100] (a "Percentage"
 *          confidence, @c ¬-fixed at 50) is @b not sound here: @c Ω would still
 *          be all of @c T, so @c True would not be the top and @c ¬ could leave
 *          the interval.  A genuinely bounded @c [Lo,Hi] chain needs a carrier
 *          that @e enforces the range: see @c Percent / @c Percentage below
 *          (#906).  The species names
 *          the ops (@c min / @c max / @c ¬); raw @c T keeps its own boolean
 *          @c &&, so the concepts gate the species' named ops (register-
 *          agnostic).  See #901.
 */
export template <std::integral T>
  requires(std::same_as<T, std::remove_cv_t<T>> && !std::same_as<T, bool>)
struct Chain final {
  using Ω = T;
  /** @brief The top pole @c ⊤ = the carrier maximum. */
  static constexpr T True = std::numeric_limits<T>::max();
  /** @brief The bottom pole @c ⊥ = the carrier minimum. */
  static constexpr T False = std::numeric_limits<T>::min();

  /** @brief Meet @c ∧ = numeric minimum (the numeric order @b is the truth
   *  order). */
  static constexpr T AND(T a, T b) { return std::ranges::min(a, b); }
  /** @brief Join @c ∨ = numeric maximum. */
  static constexpr T OR(T a, T b) { return std::ranges::max(a, b); }
  /** @brief ¬a = ~a (bitwise NOT): the order-reversing involution swapping the
   *  poles (⊥ ↔ ⊤).  Overflow-free (no arithmetic). */
  static constexpr T RFL(T a) { return static_cast<T>(~a); }

  /** @section logic__Chain_Op_Types (#923)
   *  @brief The op-@b types @c OR / @c AND / @c RFL @b are: join / meet @b are
   *  @c max / @c min (the numeric order @b is the truth order), i.e. the
   *  value-returning @c Sup / @c Inf from @c :species; ¬ is
   *  @c logic_complement<Chain<T>>.  @c Chain<T> reaches @c
   * IsDistributiveLattice but @b not @c IsBoundedLattice: @c ℤ has no @c Sup /
   * @c Inf identity registered (the carrier is unbounded). */
  using JoinOp = Sup;
  using MeetOp = Inf;
  using RflOp = logic_complement<Chain<T>>;
};

// STATIC "IS A" CHECK:
static_assert(IsOckhamAlgebra<Chain<int>>,
              "Chain<int> must fulfill IsOckhamAlgebra");

/**
 * @section logic__Species_4
 * @brief @c Percentage: a range-enforcing @c [0,100] carrier ("50:50").
 * @details The @b sound way to get a @e bounded chain.  @c Chain<T>'s @c Ω is
 *          the @b whole integral range, so a sub-interval like @c [0,100] is
 *          unsound there (@c True would not be the top, @c ¬ could leave the
 *          interval, #906).  Here the invariant @c 0≤v≤100 is @b enforced by
 * the constructor (saturating clamp), so @c 100 / @c 0 genuinely ARE the poles
 * and the reflection @c 100-v never leaves the range.
 */
export struct Percentage final {
  /** @brief The confidence value.  @b Naked (public) per the Juliet Posture:
   *  the @c [0,100] range is a @b saturating @b convention, not encapsulated
   *  private state.  The constructor clamps and the algebra (@c min / @c max /
   *  @c ¬) preserves the range, so every value that arrives @e through the API
   *  is in @c [0,100]; a caller who writes an out-of-range @c v directly is on
   *  their own, exactly as one who casts a fourth value into the @c Ternary
   *  enum.  Structural transparency over an OO invariant guard. */
  std::uint8_t v;
  /** @brief Saturating construction into @c [0,100]: an out-of-range input pins
   *  to the nearest pole.  The clamp is done in a signed @b wide type @b before
   *  narrowing, so @c Percentage{256} = @c 100 and @c Percentage{-1} = @c 0
   *  (not the wrap-around a narrowing-first clamp would give). */
  constexpr Percentage(int p) noexcept
      : v(static_cast<std::uint8_t>(p < 0 ? 0 : (p > 100 ? 100 : p))) {}
  constexpr std::strong_ordering operator<=>(const Percentage&) const = default;
  constexpr bool operator==(const Percentage&) const = default;
};

/**
 * @brief The bounded confidence chain: a 101-grade De Morgan (Kleene) chain,
 *        self-dual about @c 50.
 * @details Meet @c ∧ / join @c ∨ = numeric @c min / @c max on @c [0,100] (the
 *          numeric order @b is the confidence order); reflection @c ¬p @c =
 *          @c 100-p is the order-reversing involution swapping the poles
 *          (@c 0 ↔ @c 100), fixing the self-dual midpoint @c 50.  @b Not
 *          Boolean: the interior grades are uncomplemented (@c p∧¬p ≠ @c 0).
 */
export struct Percent final {
  using Ω = Percentage;
  /** @brief The top pole @c ⊤ = full confidence @c 100. */
  static constexpr Percentage True{100};
  /** @brief The bottom pole @c ⊥ = no confidence @c 0. */
  static constexpr Percentage False{0};
  /** @brief Meet @c ∧ = numeric minimum. */
  static constexpr Percentage AND(Percentage a, Percentage b) noexcept {
    return {std::ranges::min(a.v, b.v)};
  }
  /** @brief Join @c ∨ = numeric maximum. */
  static constexpr Percentage OR(Percentage a, Percentage b) noexcept {
    return {std::ranges::max(a.v, b.v)};
  }
  /** @brief ¬p = @c 100-p: reflection about the self-dual midpoint @c 50. */
  static constexpr Percentage RFL(Percentage a) noexcept { return {100 - a.v}; }

  /** @section logic__Percent_Op_Types (#923)
   *  @brief The op-@b types @c OR / @c AND / @c RFL @b are: join / meet @b are
   *  @c max / @c min on @c [0,100], i.e. @c Sup / @c Inf from @c :species; ¬ is
   *  @c logic_complement<Percent>.  Being a @b finite bounded chain (unlike
   *  @c Chain<T>), @c Percent reaches @c IsBoundedLattice once its bounds are
   *  registered (below). */
  using JoinOp = Sup;
  using MeetOp = Inf;
  using RflOp = logic_complement<Percent>;
};

static_assert(IsOckhamAlgebra<Percent>, "Percent must fulfill IsOckhamAlgebra");

/** @section logic__Percent_Bounds (#923)
 *  @brief The @c Percent lattice bounds, backed by a computed witness over the
 *  poles.  @c ⊥ @c = @c Percentage{0} is the join (@c ∨ @c = @c Sup) identity,
 *  @c ⊤ @c = @c Percentage{100} the meet (@c ∧ @c = @c Inf) identity, lifting
 *  the finite @c [0,100] chain to @c IsBoundedLattice.  Mirrors the @c Ternary
 *  bounds registration (#912). */
static_assert(Sup{}(Percentage{0}, Percentage{0}) == Percentage{0} &&
                  Sup{}(Percentage{0}, Percentage{50}) == Percentage{50} &&
                  Sup{}(Percentage{0}, Percentage{100}) == Percentage{100},
              "Percent: ⊥ = 0 is the join (∨ = Sup) identity");
static_assert(Inf{}(Percentage{100}, Percentage{0}) == Percentage{0} &&
                  Inf{}(Percentage{100}, Percentage{50}) == Percentage{50} &&
                  Inf{}(Percentage{100}, Percentage{100}) == Percentage{100},
              "Percent: ⊤ = 100 is the meet (∧ = Inf) identity");

/** @brief ∨-identity (⊥) of the Percent lattice: @c Percentage{0}. */
template <>
struct identity_trait<Percentage, Sup> {
  using value_type = Percentage;
  static constexpr Percentage value{0};
};
/** @brief ∧-identity (⊤) of the Percent lattice: @c Percentage{100}. */
template <>
struct identity_trait<Percentage, Inf> {
  using value_type = Percentage;
  static constexpr Percentage value{100};
};

static_assert(identity_v<Percentage, Sup> == Percentage{0},
              "Percent: registered ∨-identity is ⊥ = 0");
static_assert(identity_v<Percentage, Inf> == Percentage{100},
              "Percent: registered ∧-identity is ⊤ = 100");

/** @section logic__Op_Type_Bridge_Witnesses (#923)
 *  @brief The bridge, pinned at the @c :species trait level (upstream of
 *  @c :total).  Each species' declared @c JoinOp / @c MeetOp resolve the
 *  distributivity / absorption / bounds traits on its carrier @c Ω, so the
 *  downstream @c :total lattice-ladder concepts (@c IsDistributiveLattice /
 *  @c IsBoundedLattice) resolve @b structurally through the aliases --- pinned
 *  in @c logic_lattice_structure_test.cpp.  The reflection @c RflOp is bridged
 *  separately: @c logic_complement<L> is the @c :involution witness that ¬ is
 *  an involution (@c logic_negation_is_involutive_v below). */

// Distributivity (both directions) resolves through the declared op-types.
static_assert(is_distributive_v<Boole::Ω, Boole::JoinOp, Boole::MeetOp> &&
                  is_distributive_v<Boole::Ω, Boole::MeetOp, Boole::JoinOp>,
              "𝔹: the OR/AND op-types distribute (logical_or / logical_and)");
static_assert(is_distributive_v<Kleene::Ω, Kleene::JoinOp, Kleene::MeetOp> &&
                  is_distributive_v<Kleene::Ω, Kleene::MeetOp, Kleene::JoinOp>,
              "K₃: the OR/AND op-types distribute (Sup / Inf on Ternary)");
static_assert(
    is_distributive_v<Chain<int>::Ω, Chain<int>::JoinOp, Chain<int>::MeetOp> &&
        is_distributive_v<Chain<int>::Ω, Chain<int>::MeetOp,
                          Chain<int>::JoinOp>,
    "Chain<int>: the OR/AND op-types distribute (Sup / Inf on int)");
static_assert(
    is_distributive_v<Percent::Ω, Percent::JoinOp, Percent::MeetOp> &&
        is_distributive_v<Percent::Ω, Percent::MeetOp, Percent::JoinOp>,
    "Percent: the OR/AND op-types distribute (Sup / Inf on Percentage)");

// Absorption resolves through the declared op-types (a ∨ (a ∧ b) = a).
static_assert(is_absorptive_v<Boole::Ω, Boole::JoinOp, Boole::MeetOp> &&
                  is_absorptive_v<Kleene::Ω, Kleene::JoinOp, Kleene::MeetOp> &&
                  is_absorptive_v<Chain<int>::Ω, Chain<int>::JoinOp,
                                  Chain<int>::MeetOp> &&
                  is_absorptive_v<Percent::Ω, Percent::JoinOp, Percent::MeetOp>,
              "shipped species: OR/AND op-types are mutually absorptive");

// Bounds resolve through the declared op-types for the FINITE species (⊥ =
// ∨-identity, ⊤ = ∧-identity).  Chain<int> is deliberately absent: ℤ has no
// Sup/Inf identity registered, so it reaches IsDistributiveLattice but NOT
// IsBoundedLattice (the carrier is unbounded).
static_assert(identity_v<Kleene::Ω, Kleene::JoinOp> == Kleene::False &&
                  identity_v<Kleene::Ω, Kleene::MeetOp> == Kleene::True,
              "K₃ bounds via op-types: ⊥ = False (∨-id), ⊤ = True (∧-id)");
static_assert(identity_v<Percent::Ω, Percent::JoinOp> == Percent::False &&
                  identity_v<Percent::Ω, Percent::MeetOp> == Percent::True,
              "Percent bounds via op-types: ⊥ = 0 (∨-id), ⊤ = 100 (∧-id)");

export constexpr Ternary operator&&(Ternary a, Ternary b) {
  return Kleene::AND(a, b);
}
export constexpr Ternary operator||(Ternary a, Ternary b) {
  return Kleene::OR(a, b);
}
export constexpr Ternary operator!(Ternary a) { return Kleene::RFL(a); }

/** @brief Truth-order @c <=> on @c Ternary: the chain
 *         @c False @c (-1) @c < @c Unknown @c (0) @c < @c True @c (1).
 *
 *  Enables stdlib niebloids (@c std::ranges::min, @c std::ranges::max)
 *  to compute the Kleene meet / join on @c Ternary directly, so the
 *  Form-chain @c Meet / @c Join slots reuse stdlib infrastructure
 *  rather than carrying named Ternary-specific function-object struct
 *  types (#698 Slice 8 review).  @c min on the chain is Kleene AND;
 *  @c max is Kleene OR — identical to @c Kleene::AND / @c OR
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
  using type = Boole;
};

export template <>
struct GetLogic<Ternary> {
  using type = Kleene;
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
 * (@c && / @c || / @c ! all returning @c T, as for @c bool, @c Ternary, and the
 * @c Truth<L> wrappers now that the meet/join register has landed), or is a
 * registered logic wrapper declaring a valid @c logic_species.  @c Truth<L> now
 * qualifies by @b both branches; the @c logic_species branch remains the
 * fallback for any wrapper that does not overload the operators.  @c int and
 * @c std::string satisfy neither (@c int's @c && yields
 * @c bool, and neither declares a @c logic_species), so @c IsΩ does not
 * over-accept them.  This is deliberately decoupled from @c GetLogic, whose
 * permissive default maps any type to @c Boole and would otherwise let
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
    // ...or a registered logic wrapper declaring a valid logic_species.  This
    // is the fallback for a wrapper that does not close the operators; Truth<L>
    // now does (see the meet/join register), so it also matches the branch
    // above.
    requires {
      typename T::logic_species;
      requires IsOckhamAlgebra<typename T::logic_species>;
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
 *          @f$\mathbb{B}@f$ = @c Boole::Ω = @c bool is the two-valued
 *          @b decided @b core @f$\{\top,\bot\}@f$ that sits inside @e every
 *          answer-lattice @f$\Omega@f$ (every @c IsΩ), and @f$\iota@f$
 *          is its inclusion.  So @f$\mathbb{B}@f$ is @e primus @e inter @e
 * pares among the truth-objects: a peer of any other @f$\Omega@f$ at the object
 * layer, but the one target every decidable map factors through (the Rosolini
 * dominance @f$\Sigma@f$).  @c Ternary (Kleene
 *          @f$K_3@f$) and @c Chain<T> are the non-trivial @f$\Omega@f$ we
 * currently ship: peers of @f$\mathbb{B}@f$, @b not the canonical
 * @f$\Omega@f$; for @c Ternary @f$\iota@f$ is the concrete map @c bool @c ↪
 * @c Ternary (@c Ternary = @f$\mathbb{B} + 1@f$, adjoining @c Unknown), while
 * the concept @c IsDominanceInclusion fixes only the shape
 *          @f$\mathbb{B} \to \Omega@f$; a future @f$\Omega@f$ is admitted by
 * that shape but supplies its own @f$\mathbb{B}@f$-inclusion (@c lift_logic
 *          embeds @c bool @c ↪ @f$\Omega@f$ generically, sending
 *          @f$\bot/\top@f$ to the target species' poles: @c Ternary::{False,
 *          True}, @c Chain<T>'s @c numeric_limits @c {min,max}; a value already
 *          in @f$\Omega@f$ passes through unchanged).  @f$\top \in
 * \mathbb{B}@f$ and
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
  // The dominance inclusion 𝔹 ↪ Ω: a decided @c bool answer embeds as the
  // target species' poles (@c false ↦ @c ⊥, @c true ↦ @c ⊤).  This is uniform
  // across every @c IsOckhamAlgebra: @c Boole maps to itself (its poles ARE
  // the bools), @c Kleene to @c Ternary::{False,True}, @c Chain<T> to
  // @c numeric_limits<T>::{min,max}.  Without this, @c Truth<Chain<T>> would
  // store the raw @c 0 / @c 1 (interior chain values), not @c ⊥ / @c ⊤.  A
  // value already in the species (@c T = @c Ω, not @c bool) passes through.
  // The endpoints are cast to @c Ω explicitly: @c IsOckhamAlgebra only asks
  // @c True / @c False to be @e convertible to @c Ω, so a species declaring
  // them at a narrower type (e.g. @c int constants for a wrapper @c Ω) must not
  // leak that declaration type out of the codomain-preserving inclusion.
  if constexpr (std::is_same_v<T, bool> && IsOckhamAlgebra<TargetLogic>) {
    using Ω = typename TargetLogic::Ω;
    return value ? static_cast<Ω>(TargetLogic::True)
                 : static_cast<Ω>(TargetLogic::False);
  } else {
    return value;
  }
}

/** @brief The dominance order on logic species: @c From @c ⊑ @c To iff @c
 * From's answers embed into @c To's (@c lift_logic is that inclusion).
 * Reflexive, and
 *  @c 𝔹 @c ⊑ @c K₃ (@c bool @c ↪ @c Ternary).  A cross-species combine / lift
 * is defined @b only along this order: a species pair with no registered
 * inclusion is rejected at the API gate rather than failing inside @c
 * lift_logic (which would silently pass a wrong codomain).  Extend by
 * specialising @c lifts_to_v for a new inclusion. */
export template <typename From, typename To>
inline constexpr bool lifts_to_v = false;
template <typename L>
inline constexpr bool lifts_to_v<L, L> = true;
template <>
inline constexpr bool lifts_to_v<Boole, Kleene> = true;
// NB: the 𝔹 ↪ Chain<T> inclusion is real (lift_logic implements it: decided
// answers land on the poles) but is deliberately NOT registered here.  The
// cross-species set combine that consumes lifts_to_v routes through
// sets::join_logic_t, which only selects Boole/Kleene; a Boole/Chain mix would
// pick Boole and then demand the false reverse edge Chain ↪ Boole.  Registering
// a half-edge the set layer cannot honour would be a misleading claim.  Chain
// as a set codomain (a general dominance join) is a separate increment.
export template <typename From, typename To>
concept LiftsTo = lifts_to_v<From, To>;

/**
 * @class Truth
 * @brief The Monic Wrapper for a Logical Species (Ω).
 * @details Wraps a raw truth type (bool, Ternary) as a De Morgan-lattice
 *          element, carrying meet @c && , join @c || , the involution @c ! and
 *          the lattice order @c <= , while preventing machine-level integral
 *          promotion.  The former rig (@c + / @c *) surface was retired: a
 * truth value is a lattice element, not a semiring element (#901).
 */
export template <typename L = Boole>
struct Truth {
  using logic_species = L;
  using machine_type = typename L::Ω;

  machine_type value;

  /** @section logic__Monic_Construction
   *  @brief Wrap a raw carrier value as a @c Truth (the monic promotion).
   *  @details @b explicit by design: a raw carrier does @b not implicitly
   *  become a @c Truth.  This keeps @c Truth<Boole> @c && @c bool decaying to
   *  the built-in @c bool @c && (short-circuit preserved, which matters if the
   *  raw operand has side effects), rather than binding a strict eager wrapper
   *  overload.  Opt into the lattice register by wrapping explicitly:
   *  @c Truth<L>{v}.
   *  @note This is a deliberate @b source-breaking change from the previous
   *  implicit constructor: a @c Truth<L>-returning function can no longer
   *  @c return a bare carrier (write @c return @c Truth<L>{v}).  No in-tree
   *  caller relied on the implicit form, and per the project's
   *  experimental-API-break posture no compatibility shim is kept. */
  constexpr explicit Truth(machine_type v) noexcept : value(v) {}
  constexpr Truth() noexcept : value(L::False) {}

  // Unary Negation: Ensures !Boolean returns a Boolean, not a raw bool
  friend constexpr Truth operator!(Truth a) noexcept {
    return Truth{L::RFL(a.value)};
  }

  /** @section logic__Lattice_Register
   *  @brief Meet @c ∧ = @c L::AND and join @c ∨ = @c L::OR on two @b wrappers,
   *  spelled @c && / @c || to complete the @c && / @c || / @c ! register
   *  (matching the raw carriers @c bool / @c Ternary).
   *  @note @b Same-species only (@c Truth<L> @c op @c Truth<L>).  A @b mixed
   *  @c Truth @c op @c carrier expression is deliberately @e not captured:
   *  because the carrier ctor is @c explicit the raw operand does not promote
   * to
   *  @c Truth, so @c Truth<Boole> @c && @c bool falls to the built-in @c bool
   *  @c && @c bool, preserving short-circuit.  Wrap explicitly to stay in the
   *  lattice.  The register is @b strict (no short-circuit): on two wrappers
   *  both operands are already-computed, side-effect-free values, so eager
   *  meet/join is extensionally the built-in behaviour. */
  friend constexpr Truth operator&&(Truth a, Truth b) noexcept(
      noexcept(L::AND(a.value, b.value))) {
    return Truth{L::AND(a.value, b.value)};
  }
  /** @brief Join @c ∨ of two wrappers.  @overload */
  friend constexpr Truth operator||(Truth a, Truth b) noexcept(
      noexcept(L::OR(a.value, b.value))) {
    return Truth{L::OR(a.value, b.value)};
  }

  /** @section logic__Lattice_Order
   *  @brief The truth order: @c a @c <= @c b iff the join @c a @c ∨ @c b is
   *  @c b.  Meet / join / reflection themselves are the species morphisms
   *  @c L::AND / @c L::OR / @c L::RFL, spelled @c && / @c || / @c ! on the
   *  wrapper.  The rig @c + / @c * surface was @b retired (#901): a truth value
   *  is a @b De @b Morgan-lattice element, not a semiring element, so it
   * carries the involution and the lattice order, not @c + / @c * / @c one().
   */
  friend constexpr Truth operator<=(Truth a, Truth b) noexcept {
    return Truth{lift_logic<L>(L::OR(a.value, b.value) == b.value)};
  }

  /** @section logic__Conversion */
  constexpr explicit operator machine_type() const noexcept { return value; }
  constexpr bool operator==(const Truth&) const = default;
};

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
  using type = Boole;
};

/** @brief Specialization for Floating-Point Species (IEEE 754 NaN handling) */
template <std::floating_point T>
struct LogicTraits<T> {
  using type = Kleene;
};

/**
 * @brief Specialization for Signed Integrals (Lipschitz Boundary handling)
 * Note: We use Ternary here to represent the 'Unknown' state of an overflow.
 */
template <std::signed_integral T>
struct LogicTraits<T> {
  using type = Kleene;
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
// operators; the Truth<L> wrappers now qualify via BOTH branches (the meet/join
// register closes &&/||/! AND they declare a logic_species); non-truth types
// (int, ...) qualify by neither.
static_assert(IsΩ<bool> && IsΩ<Ternary>,
              "raw truth-types are Ω (their &&/||/! close on the type)");
static_assert(
    HasLogicalOperators<Truth<Boole>> && HasLogicalOperators<Truth<Kleene>>,
    "the meet/join register closes &&/||/! on Truth<L> (all return Truth<L>), "
    "so Truth<L> is Ω by the operator branch too, not only via logic_species");
static_assert(
    IsΩ<Truth<Boole>> && IsΩ<Truth<Kleene>>,
    "Truth<L> wrappers are Ω (via the closed operators and their registered "
    "logic_species; they carry the involution ! and the lattice order <=, "
    "meet/join are the species AND/OR, and the rig +/* surface was retired)");
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
static_assert(Kleene::AND(Ternary::True, Ternary::Unknown) ==
                      Ternary::Unknown &&
                  Kleene::AND(Ternary::False, Ternary::Unknown) ==
                      Ternary::False,
              "K₃: AND = min");
static_assert(Kleene::OR(Ternary::False, Ternary::Unknown) ==
                      Ternary::Unknown &&
                  Kleene::OR(Ternary::True, Ternary::Unknown) == Ternary::True,
              "K₃: OR = max");
static_assert(Kleene::RFL(Ternary::True) == Ternary::False &&
                  Kleene::RFL(Ternary::False) == Ternary::True &&
                  Kleene::RFL(Ternary::Unknown) == Ternary::Unknown,
              "K₃: RFL reflects about U (¬U = U)");

/** @section logic__Kleene_Carrier_Total_Registration (#912)
 *
 * @brief Register the Kleene carrier @c Ternary in the @c :total / @c :posetal
 *        machinery so the lattice-ladder and order concepts resolve on it,
 *        the same way @c bool and @c int are registered.
 *
 * @details @c Ternary is the finite 3-chain @c False @c < @c Unknown @c < @c
 * True, with join @c ∨ @c = @c Sup and meet @c ∧ @c = @c Inf (the
 * value-returning chain lattice ops from @c :species; @b not the
 * @c std::ranges::max / @c min niebloids, which return @c const @c T& and so
 * cannot satisfy @c IsClosedUnder's @c same_as<T> --- see @c Sup / @c Inf and
 * @c FIXME(#934)).  Their lattice-law traits (@c is_idempotent_v /
 * @c is_associative_v / @c is_commutative_v / @c is_distributive_v /
 * @c is_absorptive_v) are registered generically over every carrier in
 * @c :species, so @c IsDistributiveLattice<Ternary, Sup, Inf> follows once the
 * two pieces below are supplied:
 *
 *   1. the @b bounds (@c identity_v): @c ⊥ @c = @c False (∨-identity) and
 *      @c ⊤ @c = @c True (∧-identity), lifting the finite chain from
 *      @c IsDistributiveLattice up to @c IsBoundedLattice / @c IsPointed
 *      (@c Chain<int> cannot reach this rung: @c ℤ is unbounded);
 *   2. the @b order traits (reflexive / transitive / antisymmetric under
 *      @c <=), which the @c :species blanket registers only for
 *      @c std::integral carriers --- @c Ternary is a scoped enum, so it needs
 *      the explicit void-form @c std::less_equal<> registration mirrored here.
 *
 * The carrier-level variety witnesses (@c IsDistributiveLattice /
 * @c IsBoundedLattice) live in @c :total, which imports @c :logic transitively
 * (via @c :posetal), so they cannot be asserted from this upstream partition;
 * they are pinned in @c logic_lattice_structure_test.cpp and @c order_test.cpp
 * alongside the @c Boole / @c Chain<int> siblings.
 */

// The bounds, backed by a computed witness over the whole finite carrier.
// ∨ = Sup: ⊥ = False is the identity (Sup(False, t) = t for every t).
static_assert(Sup{}(Ternary::False, Ternary::False) == Ternary::False &&
                  Sup{}(Ternary::False, Ternary::Unknown) == Ternary::Unknown &&
                  Sup{}(Ternary::False, Ternary::True) == Ternary::True,
              "K₃: ⊥ = False is the join (∨ = Sup) identity");
// ∧ = Inf: ⊤ = True is the identity (Inf(True, t) = t for every t).
static_assert(Inf{}(Ternary::True, Ternary::False) == Ternary::False &&
                  Inf{}(Ternary::True, Ternary::Unknown) == Ternary::Unknown &&
                  Inf{}(Ternary::True, Ternary::True) == Ternary::True,
              "K₃: ⊤ = True is the meet (∧ = Inf) identity");

/** @brief ∨-identity (⊥) of the K₃ lattice: @c False.  Mirrors the
 *  @c identity_v<bool, std::logical_or<bool>> @c == @c false (⊥) registration
 *  in @c :species (the join / OR identity is the bottom, not the top). */
template <>
struct identity_trait<Ternary, Sup> {
  using value_type = Ternary;
  static constexpr Ternary value = Ternary::False;
};
/** @brief ∧-identity (⊤) of the K₃ lattice: @c True. */
template <>
struct identity_trait<Ternary, Inf> {
  using value_type = Ternary;
  static constexpr Ternary value = Ternary::True;
};

static_assert(identity_v<Ternary, Sup> == Ternary::False,
              "K₃: registered ∨-identity is ⊥ = False");
static_assert(identity_v<Ternary, Inf> == Ternary::True,
              "K₃: registered ∧-identity is ⊤ = True");

/** @brief K₃'s total order under @c <=.  @c Ternary is the 3-chain
 *  @c False @c < @c Unknown @c < @c True; being a scoped enum (not
 *  @c std::integral) it is outside the @c :species blanket, so the void-form
 *  @c std::less_equal<> registrations are mirrored here. */
template <>
inline constexpr bool is_reflexive_v<Ternary, std::less_equal<>> = true;
template <>
inline constexpr bool is_transitive_v<Ternary, std::less_equal<>> = true;
template <>
inline constexpr bool is_antisymmetric_v<Ternary, std::less_equal<>> = true;

static_assert(is_reflexive_v<Ternary, std::less_equal<>> &&
                  is_transitive_v<Ternary, std::less_equal<>> &&
                  is_antisymmetric_v<Ternary, std::less_equal<>>,
              "K₃ is a partial (indeed total) order under <=");

/** @brief K₃'s total order under the @b typed relation @c std::less_equal<T>.
 *  The @c order:: concepts (@c IsPreOrdered / @c IsTotallyOrdered) query the
 *  @b transparent @c std::less_equal<> registered above, but the @c category::
 *  order concepts (@c IsPartRelation / @c IsPosetal / @c IsTotalOrder / @c
 *  IsThinCategory) default @c Rel to the @b typed @c std::less_equal<Ternary>
 *  and query the traits for @b that exact type, so the transparent form does
 *  not reach them.  Reflexivity is already supplied by the @c :species
 *  @c totally_ordered @c std::less_equal<T> struct specialisation (@c Ternary
 *  is @c std::totally_ordered); only transitivity / antisymmetry are outside
 *  the @c :species integral blanket and are registered here.  This is also the
 *  order the @c :species @c SupInfLattice law gate certifies against (a genuine
 *  total order cannot contain NaN). */
template <>
inline constexpr bool is_transitive_v<Ternary, std::less_equal<Ternary>> = true;
template <>
inline constexpr bool is_antisymmetric_v<Ternary, std::less_equal<Ternary>> =
    true;

static_assert(is_reflexive_v<Ternary, std::less_equal<Ternary>> &&
                  is_transitive_v<Ternary, std::less_equal<Ternary>> &&
                  is_antisymmetric_v<Ternary, std::less_equal<Ternary>>,
              "K₃ is a total order under the typed std::less_equal<Ternary> "
              "(reflexivity via the :species totally_ordered specialisation)");

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

/** @brief Witness: Boolean negation is an involution.  @c Boole's
 *  @c RFL is @c std::logical_not on @c bool (@c !!b = b). */
template <>
struct is_involutive<logic_complement<Boole>, bool> : std::true_type {};

/** @brief Witness: Kleene negation is an involution.  @c Kleene's @c RFL
 *  reflects the K₃ chain about @c Unknown, so @c ¬¬a = a on all three values
 *  (the static_asserts above prove it). */
template <>
struct is_involutive<logic_complement<Kleene>, Ternary> : std::true_type {};

/** @brief Witness: the @c Chain<T> reflection @c ~a is an involution
 *  (@c ~~x = x) for every integral carrier. */
template <std::integral T>
  requires(!std::same_as<T, bool>)
struct is_involutive<logic_complement<Chain<T>>, T> : std::true_type {};

/** @brief Witness: the @c Percent reflection @c 100-p is an involution. */
template <>
struct is_involutive<logic_complement<Percent>, Percentage> : std::true_type {};

/** @brief @c true iff the logic negation ¬ = @c L::RFL is a certified
 *  involution.  Both shipped De Morgan logics (@c 𝔹, @c K₃) qualify; a future
 *  intuitionistic species whose ¬¬ is only a closure would not.  Downstream
 *  double-negation elimination gates on this so it stays honest per logic. */
export template <typename L>
inline constexpr bool logic_negation_is_involutive_v =
    IsInvolution<logic_complement<L>, typename L::Ω>;

static_assert(logic_negation_is_involutive_v<Boole>,
              "𝔹: ¬ is an involution, so !!A = A is sound");
static_assert(logic_negation_is_involutive_v<Kleene>,
              "K₃: ¬ is an involution, so !!A = A is sound");
static_assert(logic_negation_is_involutive_v<Chain<int>>,
              "Chain<int>: ~ is an involution (~~x = x)");

/**
 * @section logic__De_Morgan_Hierarchy
 * @brief The committed algebraic tower for the truth objects (#901).
 *
 * @details @b Register-agnostic concepts: each gates the species' @b named ops
 *          (@c L::AND / @c L::OR / @c L::RFL) and their laws, NOT any
 * particular C++ operator, so a raw carrier like @c int qualifies via @c
 * Chain<int> without overloading (@c int's own @c && stays boolean).
 *
 *  - @c IsDeMorganAlgebra: a logical species whose reflection is a certified
 *    involution (bounded distributive lattice + De Morgan ¬; @c ¬¬=id).
 *  - @c IsBoundedDeMorganChain: adds that the truth carrier @c Ω is totally
 *    ordered.  By the chain-normality theorem (on a chain @c min(a,¬a) ≤
 *    @c max(b,¬b)), this is automatically a @b Kleene lattice.
 *  - @c IsBooleanLogic: adds that the reflection is a genuine @b complement
 *    (@c a∧¬a=⊥, @c a∨¬a=⊤), which on a chain forces the 2-element case (@c 𝔹).
 *
 *  Species-level twin of the value-level @c IsPst (bounded-chain truth object):
 *  @c IsPst gates a truth @b value type (@c bool, @c Ternary), these gate the
 *  @b species / algebra, so @c Chain<int> qualifies although @c int is not @c
 * IsΩ.
 */
/**
 * @concept IsDeMorganAlgebra
 * @brief A logical species whose reflection @c ¬ is a certified involution
 *        (@c ¬¬=id): a De Morgan algebra (bounded distributive lattice with an
 *        order-reversing involution).
 * @note These three concepts are conservative @b shape @b gates (like @c IsΩ /
 * @c IsPst): they certify the operation SIGNATURES plus the involution law
 * (@c ¬¬=id), NOT the full semantic De Morgan / distributivity / order-reversal
 * laws.  Those are pinned by the @c static_assert witnesses below (and, for the
 * reducer, by @c category:lattice).  So a pathological species could pass
 * without every law; the concept recognises the intended family (@c Boole,
 * @c Kleene, @c Chain<T>) and the witnesses hold the laws.
 */
export template <typename L>
concept IsDeMorganAlgebra =
    IsOckhamAlgebra<L> && logic_negation_is_involutive_v<L>;

/**
 * @concept IsBoundedDeMorganChain
 * @brief A De Morgan algebra whose truth carrier @c Ω is totally ordered; by
 *        the chain-normality theorem this is automatically a @b Kleene lattice.
 */
export template <typename L>
concept IsBoundedDeMorganChain =
    IsDeMorganAlgebra<L> && std::totally_ordered<typename L::Ω> &&
    // Exclude NaN-carrying (floating) Ω: @c std::totally_ordered is a SYNTAX
    // check, and a @c NaN breaks the total order chain-normality relies on. The
    // shipped Ω are integral / enum, so genuinely totally ordered.
    !std::floating_point<typename L::Ω>;

/** @brief @c true iff the species' reflection is a genuine complement
 *  (@c a∧¬a=⊥ and @c a∨¬a=⊤ for all @c a), i.e. the chain is 2-element.
 *  @c Boole qualifies; @c Kleene and @c Chain<T> (|Ω|>2) do not: their interior
 *  values have no complement.  This is the @c logic_species=@c Boole reading of
 *  decidability: the clopen core @c Σ = @c 𝔹 ↪ @c Ω. */
export template <typename L>
inline constexpr bool logic_is_complemented_v = false;
template <>
inline constexpr bool logic_is_complemented_v<Boole> = true;

/**
 * @concept IsBooleanLogic
 * @brief A bounded De Morgan chain whose reflection is a genuine @b complement
 *        (@c a∧¬a=⊥, @c a∨¬a=⊤); on a chain this forces the 2-element case @c
 * 𝔹.
 */
export template <typename L>
concept IsBooleanLogic =
    IsBoundedDeMorganChain<L> && logic_is_complemented_v<L>;

// Tower witnesses: 𝔹, K₃, Chain<int> are all De Morgan / bounded chains; only
// Boole is Boolean (complemented).  These live where the Forms are defined; the
// concrete-carrier Juliet witnesses live downstream (see #901 acceptance).
static_assert(IsDeMorganAlgebra<Boole> && IsDeMorganAlgebra<Kleene> &&
                  IsDeMorganAlgebra<Chain<int>>,
              "𝔹, K₃, Chain<int> are De Morgan algebras (involutive ¬)");
static_assert(IsBoundedDeMorganChain<Boole> && IsBoundedDeMorganChain<Kleene> &&
                  IsBoundedDeMorganChain<Chain<int>>,
              "... and bounded De Morgan chains (Ω totally ordered ⟹ Kleene)");
static_assert(
    IsBooleanLogic<Boole>,
    "𝔹 is Boolean: its reflection is a genuine complement (2-element)");
static_assert(!IsBooleanLogic<Kleene> && !IsBooleanLogic<Chain<int>>,
              "K₃ and Chain<int> are Kleene, NOT Boolean (interior values "
              "uncomplemented)");

// Chain<int> proof-of-concept (#901): the involution is bitwise NOT with
// INT_MIN / INT_MAX as the poles, and bound-absorption IS decidability
// collapse.
static_assert(Chain<int>::RFL(Chain<int>::False) == Chain<int>::True &&
                  Chain<int>::RFL(Chain<int>::True) == Chain<int>::False,
              "Chain<int>: ~ swaps ⊥=INT_MIN and ⊤=INT_MAX");
static_assert(Chain<int>::RFL(Chain<int>::RFL(42)) == 42,
              "Chain<int>: ~~x = x (involutive, fixed-point-free)");
static_assert(
    Chain<int>::AND(7, Chain<int>::False) == Chain<int>::False &&
        Chain<int>::OR(7, Chain<int>::True) == Chain<int>::True,
    "Chain<int>: bounds absorb (x∧⊥=⊥, x∨⊤=⊤): decidability collapse");
static_assert(
    Chain<int>::AND(0, Chain<int>::RFL(0)) != Chain<int>::False,
    "Chain<int>: 0 is NOT complemented (0 ∧ ~0 = min(0,-1) = -1 ≠ ⊥)");
static_assert(Chain<int>::RFL(Chain<int>::AND(3, 8)) ==
                  Chain<int>::OR(Chain<int>::RFL(3), Chain<int>::RFL(8)),
              "Chain<int>: De Morgan ~(a∧b) = ~a ∨ ~b");

// The full-range integral chain works for signed AND unsigned carriers.
static_assert(IsBoundedDeMorganChain<Chain<unsigned>> &&
                  !IsBooleanLogic<Chain<unsigned>>,
              "unsigned full-range chain [0,UMAX] is Kleene, not Boolean");

// Percent: a range-enforcing 101-grade bounded De Morgan chain, self-dual
// at 50.
static_assert(IsDeMorganAlgebra<Percent> && IsBoundedDeMorganChain<Percent> &&
                  !IsBooleanLogic<Percent>,
              "Percent [0,100] is a bounded De Morgan (Kleene) chain, NOT "
              "Boolean (interior grades uncomplemented)");
static_assert(Percent::RFL(Percentage{50}) == Percentage{50},
              "Percent is self-dual about 50 (the 50:50 midpoint)");
static_assert(Percent::RFL(Percent::True) == Percent::False &&
                  Percent::RFL(Percent::False) == Percent::True,
              "Percent: ¬ swaps the poles 0 ↔ 100");
static_assert(Percentage{200}.v == 100 && Percentage{256}.v == 100 &&
                  Percentage{-1}.v == 0,
              "Percentage enforces [0,100] (clamp in a wide type before "
              "narrowing: 256↦100 not 0, -1↦0 not 100)");

// The 𝔹 ↪ Chain<T> dominance inclusion: decided bool answers land on the poles,
// so Truth<Chain<T>> (its operator<= lifts a bool verdict) stores ⊥/⊤, never
// the interior 0/1.  Without this lift_logic branch the order relation would
// decay.
static_assert(
    lift_logic<Chain<int>>(true) == Chain<int>::True &&
        lift_logic<Chain<int>>(false) == Chain<int>::False,
    "lift_logic<Chain<int>>(bool) maps ⊤/⊥ to INT_MAX/INT_MIN, not 1/0");
static_assert((Truth<Chain<int>>{3} <= Truth<Chain<int>>{7}).value ==
                      Chain<int>::True &&
                  (Truth<Chain<int>>{7} <= Truth<Chain<int>>{3}).value ==
                      Chain<int>::False,
              "Truth<Chain<int>>::operator<= yields the poles ⊤/⊥, not 1/0");

/**
 * @brief Membership in the decided core @f$\{\top, \bot\}@f$ of an
 * answer-lattice
 *        @f$\Omega@f$ --- the Rosolini dominance @f$\Sigma@f$, which the paper
 * and
 *        @c lift_logic define as exactly the decided subobject @f$\{\top,\bot\}
 *        \hookrightarrow \Omega@f$.
 *
 * @details The @f$\top@f$ endpoint is detected by @c value @c == @c L::True;
 * the
 * @f$\bot@f$ endpoint by its Kleene reflection, @c L::RFL(value) @c == @c
 * L::True (equivalently @c value @c == @c L::False).  The reflection spelling
 * is used on purpose: it is sound @b because @c RFL is an order-reversing @b
 * involution
 * (@c logic_negation_is_involutive_v, the @c :involution witness from the
 * double-negation work), which the @c requires clause demands; a logic whose
 * @c RFL were only a closure is excluded.
 *
 * The answer is itself classical: a bound either is or is not reached, never
 * @c Unknown.  So @c is_decided co-restricts any @f$\Omega@f$ to @c bool, the
 * value-level observable behind @c HasDecidableMembership (the type-level,
 * conservative certificate).  On @c Boole (@f$\Sigma = \Omega@f$) it
 * is constantly @c true; on @c K₃ it is @c true off @c Unknown.
 *
 * @see Giuseppe Rosolini, @e Continuity @e and @e Effectiveness @e in @e Topoi
 *      (Oxford D.Phil., 1986); @c lift_logic (the inclusion @f$\Sigma
 *      \hookrightarrow \Omega@f$), #846 / #267, and the #847
 * recognised-vs-actual sub-quadrant this closes at the value level.
 */
export template <typename L>
  requires IsOckhamAlgebra<L> && logic_negation_is_involutive_v<L> &&
           std::equality_comparable<typename L::Ω>
constexpr bool is_decided(typename L::Ω value) {
  return value == L::True || L::RFL(value) == L::True;
}

static_assert(is_decided<Boole>(true) && is_decided<Boole>(false),
              "𝔹: Σ = Ω, so every answer is decided");
static_assert(is_decided<Kleene>(Ternary::True) &&
                  is_decided<Kleene>(Ternary::False),
              "K₃: the two endpoints ⊤, ⊥ are the decided core");
static_assert(!is_decided<Kleene>(Ternary::Unknown),
              "K₃: the interior Unknown is undecided (outside Σ = {⊤,⊥})");

}  // namespace dedekind::category
