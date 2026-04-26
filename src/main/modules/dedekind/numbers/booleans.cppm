/**
 * @file ontology:numbers.cppm
 * @brief Formal definition of the Boolean system 𝔹 within the numeric ontology.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :numbers
 * @dependency :algebra, :topology, :cardinalities, :scalars
 *
 * @section Booleans: The Binary Logic Structure
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

export module dedekind.numbers:booleans;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :scalars;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename L = ClassicalLogic>
using FiniteBooleanSetOf = dedekind::sets::FiniteBooleanSet<L>;

/** @section Canonical_Species_Spine
 *
 * The canonical Boolean species symbol @c 𝔹 = @c bool (the @b carrier
 * type, after #400).  The Boolean structures @c bool carries are
 * @c (bool, @c ⊕, @c ∧, @c 0, @c 1) — the Galois field 𝔽₂ — and
 * @c (bool, @c ∨, @c ∧) — the canonical Boolean rig.  The carrier
 * reading parallels the upper tower (@c ℚ migrated in PR #397; @c ℕ
 * / @c ℤ / @c ℝ / @c ℂ / @c 𝔻 are tracked under \#399 for the same
 * migration; this partition lands the @c 𝔹 step under \#400).
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

/** @brief Re-export of the canonical Boolean carrier symbol @c 𝔹 = @c bool.
 *
 *  @details The canonical definition lives in @c dedekind::algebra::boolean
 *  (upstream of this partition).  Per #400, @c 𝔹 names the carrier @c bool
 *  itself, not a predicate-set alias.  Predicate-set callers want
 *  @c FiniteBooleanSetOf<>{...} (explicit construction; e.g.\ the universal
 *  Boolean set is @c FiniteBooleanSetOf<>{ClassicalLogic::True,
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

/** @section Formal_Verification */

// (0) Carrier-type witness: 𝔹 names the carrier itself.
static_assert(std::same_as<𝔹, bool>, "𝔹 is the bool carrier (post-#400).");

// (0a) Relationship between 𝔹 (the carrier) and Ω<bool> (the
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
static_assert(std::same_as<typename Ω<bool>::Domain, 𝔹>,
              "Ω<bool>::Domain is the bool carrier 𝔹 — predicate-set's "
              "underlying element type IS the carrier.");
static_assert(std::same_as<typename FiniteBooleanSetOf<>::Domain, 𝔹>,
              "FiniteBooleanSetOf<>::Domain is the bool carrier 𝔹.");

// (1) IsSet anchor: the predicate-set FiniteBooleanSetOf<> is a bona-fide
//     set (membership morphism 𝔹 → Ω).  Witnesses the set-builder DSL
//     entry point that survives the carrier migration.
static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<𝔹>(FiniteBooleanSetOf<>{}))>,
    "FiniteBooleanSetOf<> is the canonical IsSet anchor for 𝔹.");

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
static_assert(dedekind::category::HasLogicalOperators<𝔹>,
              "𝔹's logical-operator surface is (&&, ||, !).");
static_assert(dedekind::order::HasLatticeOperators<𝔹>,
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
    dedekind::algebra::IsRig<𝔹, std::logical_or<𝔹>, std::logical_and<𝔹>>,
    "𝔹 under (∨, ∧) is the canonical Boolean rig (idempotent commutative "
    "semiring; no additive inverse on the carrier).");
static_assert(dedekind::category::IsField<𝔹, std::bit_xor<𝔹>, std::bit_and<𝔹>>,
              "𝔹 under (⊕, ∧) is the Galois field 𝔽₂ (the smallest non-trivial "
              "field; 𝔹's ring structure lives over the bitwise functors, "
              "not over (+, *), per the math-wins-over-C++ stance).");
static_assert(dedekind::order::IsOrderLattice<𝔹>,
              "𝔹 satisfies IsOrderLattice (the locked Boolean-ring lattice "
              "under (bit_xor, bit_and); both halves of the bundle fire).");

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

// (5) Adjacent-set arrow: 𝔹 ↪ ℕ via @c embed_𝔹_ℕ in @c :naturals.
// This partition is upstream of @c :naturals, so the witness for the
// monic-arrow registration lives in @c :naturals (registered there as
// @c is_monic_arrow_v = true on @c embed_𝔹_ℕ).

}  // namespace dedekind::numbers
