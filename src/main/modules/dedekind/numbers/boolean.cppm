/**
 * @file dedekind/numbers/boolean.cppm
 * @brief Formal definition of the Boolean system 𝔹 within the numeric ontology.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :numbers
 * @dependency :algebra, :topology, :cardinalities, :scalars
 *
 * @section numbers_boolean__Booleans
 * This partition defines the @b Boolean species—the simplest non-trivial
 * numerical system. It establishes the formal mapping between logical
 * truth values and the algebraic structure of a semiring.
 *
 * @details
 * The Boolean system is modeled as an @b Idempotent @b Commutative @b Semiring
 * over the set {0, 1}. In this mapping:
 * - Addition (⊕) is represented by @b Logical @b OR (∨).
 * - Multiplication (⊗) is represented by @b Logical @b AND (∧).
 * - The Additive Identity (0) is @b False (⊥).
 * - The Multiplicative Identity (1) is @b True (⊤).
 *
 * This structure is uniquely characterized by its cardinality |S| = 2 and its
 * idempotency property (x ⊕ x = x), which distinguishes it from the
 * additive properties of ℕ or ℝ.
 *
 * Wikipedia: Boolean algebra, Semiring, Idempotency
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "It is not of the essence of mathematics to be conversant with the
 * ideas of number and quantity."
 *       -- George Boole, An Investigation of the Laws of Thought (1854)
 */
module;

#include <concepts>
#include <functional>
#include <type_traits>  // std::remove_cvref_t for the post-#559 universe-witness static_asserts

export module dedekind.numbers:boolean;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;
import :scalars;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename L = ClassicalLogic>
using FiniteBooleanSetOf = dedekind::sets::FiniteBooleanSet<L>;

/** @section numbers_boolean__Canonical_Species_Spine
 *
 * The canonical Boolean species symbol @c 𝔹 names the @b universe value
 * @c Ω<bool> (post-#559).  The carrier is @c bool, addressed directly in
 * template-type-parameter positions; @c 𝔹 is the constexpr
 * @c UniversalSet<bool, ClassicalLogic, Finite>{} value the set-builder
 * DSL takes as ambient.  The Boolean structures the carrier @c bool
 * carries are @c (bool, @c ⊕, @c ∧, @c 0, @c 1) — the Galois field
 * 𝔽₂ — and @c (bool, @c ∨, @c ∧) — the canonical Boolean rig.  The
 * universe-vs-carrier split parallels the upper tower (@c ℕ migrated
 * in #559's ℕ slice; @c ℤ / @c ℚ / @c ℝ / @c ℂ / @c 𝔻 follow under
 * the same #559 plan).
 *
 * The predicate-set role moves to @c FiniteBooleanSetOf<> (kept as a
 * non-symbol-colliding alias of @c FiniteBooleanSet for set-builder
 * DSL usage).  The canonical home of @c 𝔹 is
 * @c dedekind::algebra::𝔹 (upstream of this partition); this
 * partition re-exports it so downstream @c numbers code can reach
 * the symbol without an extra namespace qualification.
 *
 * Acts as the base of the embedding chain
 * @c 𝔹 @c ↪ @c ℕ @c ↪ @c ℤ @c ↪ @c ℚ @c ↪ @c ℝ @c ↪ @c ℂ.
 */

/** @brief Re-export of the canonical Boolean universe symbol @c 𝔹 (post-#559).
 *
 *  @details The canonical definition lives in @c dedekind::algebra::boolean
 *  (upstream of this partition).  Post-#559, @c 𝔹 names the universe value
 *  @c Ω<bool> (a constexpr @c UniversalSet<bool, ClassicalLogic, Finite>{}),
 *  not a carrier-type alias.  The underlying carrier is @c bool, used
 *  directly in template-type-parameter positions.  Predicate-set callers
 *  want @c FiniteBooleanSetOf<>{...} (explicit construction; e.g.\ the
 *  universal Boolean set is @c FiniteBooleanSetOf<>{ClassicalLogic::True,
 *  ClassicalLogic::True}, the empty Boolean set is @c FiniteBooleanSetOf<>{}).
 */
export using ::dedekind::algebra::𝔹;

/**
 * @concept Is_B
 * @brief Identifies a structure as a formal Boolean Semiring (𝔹).
 *
 * Defines a set where the algebraic rules are governed by logical
 * disjunction and conjunction, maintaining a cardinality of exactly 2.
 *
 * @tparam M The Algebraic Structure governing the Boolean set.
 * @tparam E The underlying element type (e.g., bool).
 *
 * @note FIXME(#379): this concept appears to be dead code.  The body
 * requires @c m.cardinality() to return @c std::same_as<std::size_t>,
 * but the canonical Boolean carrier @c FiniteBooleanSet (in
 * @c sets:expressions) returns @c cardinality_type @c = @c Finite ---
 * a tag type, not @c std::size_t.  The concept therefore does not
 * fire on its own canonical carrier, and there are no witnesses
 * anywhere in the codebase.  Surgery candidate: either repair the
 * concept body to accept @c Finite (or any @c IsCardinality
 * value-or-tag) and add a @c static_assert(Is_B<FiniteBooleanSetOf<>>)
 * witness, or remove the concept as deliberately-not-shipped.  Flagged
 * during the #379 paper / module sweep.
 */
export template <typename M, typename E = typename M::Domain>
concept Is_B = std::same_as<E, bool> && requires(const M& m) {
  // The structure must possess a discrete cardinality |S| = 2.
  { m.cardinality() } -> std::same_as<std::size_t>;
  requires m.cardinality() == 2;
};

/** @section numbers_boolean__Formal_Verification */

// (0) Universe witness: 𝔹 names the universe over the bool carrier (post-#559).
//     Pre-#559, 𝔹 was a carrier-type alias for bool; post-#559 it is the
//     value Ω<bool> (a constexpr UniversalSet<bool, ClassicalLogic, Finite>{}).
static_assert(std::same_as<std::remove_cvref_t<decltype(dedekind::algebra::𝔹)>,
                           UniversalSet<bool, ClassicalLogic, Finite>>,
              "𝔹 is the universe Ω<bool> (post-#559).");
static_assert(
    std::same_as<
        typename std::remove_cvref_t<decltype(dedekind::algebra::𝔹)>::Domain,
        bool>,
    "𝔹's underlying carrier IS bool — the textbook universe-over-carrier "
    "reading.");

// (0a) Relationship between 𝔹 (the carrier) and UniversalSet<bool> (the
//      predicate-set / characteristic-function wrapper).  The
//      predicate-set's @c Domain @b is the carrier, and the
//      @c FiniteBooleanSetOf<> alias from this partition is the
//      same predicate set.  IsSet<𝔹> itself does @b not fire — the
//      @c IsSet concept (in @c category:set) needs the predicate-set
//      surface (membership morphism, ambient species, etc.); a
//      bare carrier type like @c bool carries no such surface.  To
//      participate as a set, lift the carrier through
//      @c BooleanSetOf<> / @c FiniteBooleanSetOf<>; the IsSet anchor
//      in (1) below witnesses exactly that lift.
static_assert(std::same_as<typename UniversalSet<bool>::Domain, bool>,
              "UniversalSet<bool>::Domain is the carrier `bool` (and 𝔹 = "
              "Ω<bool>) — predicate-set's "
              "underlying element type IS the carrier.");
static_assert(
    std::same_as<typename FiniteBooleanSetOf<>::Domain, bool>,
    "FiniteBooleanSetOf<>::Domain is the carrier `bool` (and 𝔹 = Ω<bool>).");

// (1) IsSet anchor: the predicate-set FiniteBooleanSetOf<> is a bona-fide
//     set (membership morphism 𝔹 → Ω).  Witnesses the set-builder DSL
//     entry point that survives the carrier migration.
static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<bool>(
        FiniteBooleanSetOf<>{}))>,
    "FiniteBooleanSetOf<> is the canonical IsSet anchor for 𝔹.");

// (1b) ExtensionalSet<bool> is the canonical *listed* (vs. predicate)
//      carrier for 𝔹: instances store their elements as data (e.g.
//      `{false, true}` for the universal Boolean set) rather than
//      sampling them by a characteristic predicate.  This static_assert
//      pins the type-level claim — that the carrier lifts to IsSet via
//      the same `ambient_set<bool>(...)` gate (#598) — so the
//      type-checker carries the proof regardless of which elements any
//      particular instance happens to hold.  Element-level witnesses
//      (instances actually containing both `true` and `false`) live in
//      `extensional_test.cpp` since `std::unordered_set` is not
//      constexpr-initializable with elements in C++23.  Sister anchor
//      to (1): predicate vs. listed view of the same Ω-shaped set.
static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<bool>(
        dedekind::sets::ExtensionalSet<bool>{}))>,
    "ExtensionalSet<bool> (the canonical listed-form carrier for 𝔹) "
    "lifts to IsSet via ambient_set<bool>(...). Element-level "
    "{false, true} witnesses live in extensional_test.cpp (#598).");

// (2) Syntax (the C++ operator surface that maps to 𝔹's algebra).
//     Witnesses written against 𝔹 directly (= @c bool post-#400) so the
//     formal-verification block reads against the canonical species
//     symbol the surrounding prose names.
//   - The logical surface (∧, ∨, ¬) lives in `category:logic` as
//     `HasLogicalOperators` and is witnessed there.
//   - The lattice / bitwise surface (&, |, ^, ~) lives in
//     `order:lattice` as `HasLatticeOperators` and is witnessed
//     there; the bitwise operators promote to int but the loose
//     `convertible_to<bool>` shape lets the concept fire.
static_assert(dedekind::category::HasLogicalOperators<bool>,
              "𝔹's logical-operator surface is (&&, ||, !).");
static_assert(dedekind::order::HasLatticeOperators<bool>,
              "𝔹's lattice-operator surface is (&, |, ^, ~).");

// (3) Semantics (the algebraic structures 𝔹 actually carries).
//   - Boolean rig (𝔹, ∨, ∧): canonical commutative idempotent semiring,
//     no additive inverse (the "no negation" carrier).
//   - Boolean ring 𝔽₂ (= 𝔹 under (⊕, ∧)): a Galois field of order 2,
//     with full ring + field laws.
//   - Boolean-ring lattice (the locked `IsOrderLattice<𝔹>` from
//     PR #394): 𝔹 under (bit_xor, bit_and) certifies as a commutative
//     ring AND has the lattice operator surface.
static_assert(
    dedekind::algebra::IsRig<bool, std::logical_or<bool>,
                             std::logical_and<bool>>,
    "𝔹 under (∨, ∧) is the canonical Boolean rig (idempotent commutative "
    "semiring; no additive inverse on the carrier).");
static_assert(
    dedekind::category::IsField<bool, std::bit_xor<bool>, std::bit_and<bool>>,
    "𝔹 under (⊕, ∧) is the Galois field 𝔽₂ (the smallest non-trivial "
    "field; 𝔹's ring structure lives over the bitwise functors, "
    "not over (+, *), per the math-wins-over-C++ stance).");
static_assert(dedekind::order::IsOrderLattice<bool>,
              "𝔹 satisfies IsOrderLattice (the locked Boolean-ring lattice "
              "under (bit_xor, bit_and); both halves of the bundle fire).");
// Order witnesses (explicit, for documentation purposes).  𝔹 is a
// totally-ordered chain under @c <=, with the spaceship and the four
// partial-order operators present at the @b literal level — both the
// shape concepts @c HasPartialOrderOperators / @c HasTotalOrderOperators
// (introduced under #401) and the @b axiomatic @c IsTotallyOrdered
// fire on the carrier.  Mirrors the @b shape vs.\ @b axiom split of
// the @c HasRingOperators / @c IsRing pattern from PR #394.
static_assert(dedekind::order::HasPartialOrderOperators<bool>,
              "𝔹 carries the partial-order operator surface "
              "(<, <=, >, >=).");
static_assert(dedekind::order::HasTotalOrderOperators<bool>,
              "𝔹 carries the total-order operator surface "
              "(spaceship + the four partial-order operators).");
static_assert(dedekind::order::IsTotallyOrdered<bool>,
              "𝔹 is axiomatically totally ordered (the chain "
              "false ≤ true).");
// Order-domain witnesses: 𝔹 is a directed set (every pair has a common
// upper bound — `true` dominates) and a directed poset (directed +
// antisymmetric).  Pins 𝔹 as a valid @b net-domain.
static_assert(dedekind::order::IsDirectedSet<bool>,
              "𝔹 is a directed set — every pair has `true` as a common "
              "upper bound.");
static_assert(dedekind::order::IsDirectedPoset<bool>,
              "𝔹 is a directed poset (directed + antisymmetric).");
// Sequence witness: FinitePath<𝔹> is a finite sequence enumerating
// 𝔹-elements (the obvious 2-element path [false, true] is the
// canonical witness).  Pins 𝔹 as a valid @b sequence codomain.
static_assert(dedekind::sequences::IsFiniteSequence<
                  dedekind::sequences::FinitePath<bool>>,
              "FinitePath<𝔹> is a bona-fide finite sequence; 𝔹 is a valid "
              "sequence codomain.");

// (4) Primitive-type arrow: 𝔹 *is* @c bool (post-#400 carrier migration).
// The universal / empty Boolean predicate-sets live on @c FiniteBooleanSetOf<>
// — kept here as the predicate-set witnesses that survive the symbol-as-
// carrier reading.
static_assert(FiniteBooleanSetOf<>{ClassicalLogic::True,
                                   ClassicalLogic::True}(true) ==
                  ClassicalLogic::True,
              "Universal Boolean predicate-set contains true.");
static_assert(FiniteBooleanSetOf<>{ClassicalLogic::True,
                                   ClassicalLogic::True}(false) ==
                  ClassicalLogic::True,
              "Universal Boolean predicate-set contains false (every bool is a "
              "Boolean).");
static_assert(FiniteBooleanSetOf<>{}(true) == ClassicalLogic::False,
              "Empty Boolean predicate-set does not contain true.");
static_assert(FiniteBooleanSetOf<>{}(false) == ClassicalLogic::False,
              "Empty Boolean predicate-set does not contain false.");

/**
 * @brief Canonical embedding @c 𝔹 @c ↪ @c 𝕂3: bool → Ternary.
 * @details The two-valued-to-three-valued Kleene lift: @c false @c
 *          ↦ @c Ternary::False (@c -1), @c true @c ↦ @c
 *          Ternary::True (@c 1).  The @c Ternary::Unknown (@c 0)
 *          value is @b not in the image — it represents the third
 *          truth-value that @c 𝔹 lacks.  This is the canonical
 *          inclusion of two-valued classical logic into three-
 *          valued Kleene logic; structurally a monomorphism.
 */
export inline constexpr auto embed_𝔹_𝕂3_ =
    arrow<bool, dedekind::category::Ternary>(
        [](const bool& b) noexcept -> dedekind::category::Ternary {
          return b ? dedekind::category::Ternary::True
                   : dedekind::category::Ternary::False;
        });

// (5) Adjacent-set arrow: 𝔹 ↪ ℕ via @c embed_𝔹_uint_ in @c :natural.
// This partition is upstream of @c :natural, so the witness for the
// monic-arrow registration lives in @c :natural (registered there as
// @c is_monic_arrow_v = true on @c embed_𝔹_uint_).

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝔹_𝕂3_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_𝔹_𝕂3_)>>,
    "embed_𝔹_𝕂3_ (𝔹 ↪ 𝕂3) is registered injective.");
}  // namespace dedekind::category
