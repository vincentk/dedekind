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
 * The canonical Boolean species symbol @c 𝔹.  In this partition
 * @c 𝔹 is the @b predicate-set (an alias of @c FiniteBooleanSetOf<>)
 * with @c Domain @c = @c bool; the underlying carrier is
 * @c bool itself, on which the algebraic structures live ---
 * @c (bool, @c ⊕, @c ∧, @c 0, @c 1) is the Galois field 𝔽₂, and
 * @c (bool, @c ∨, @c ∧) is the canonical Boolean rig.  The
 * carrier-vs-predicate-set distinction parallels the upper tower
 * (@c ℚ migrated to a carrier alias in this PR; @c ℕ / @c ℤ / @c ℝ /
 * @c ℂ / @c 𝔻 are tracked under \#399 for the same migration).
 *
 * Acts as the base of the embedding chain
 * @c 𝔹 @c ↪ @c ℕ @c ↪ @c ℤ @c ↪ @c ℚ @c ↪ @c ℝ @c ↪ @c ℂ.
 */

/** @brief The canonical Boolean species symbol 𝔹, alias of
 *         @c FiniteBooleanSetOf<> (the predicate-set type with
 *         @c Domain @c = @c bool).
 *
 *  @details No single-letter ASCII value-level constant is exported
 *  (the letter @c B collides too widely with matrix-algebra local-
 *  variable conventions in downstream test files); callers that want
 *  the @b universal Boolean set construct it explicitly as
 *  @c 𝔹{ClassicalLogic::True, ClassicalLogic::True}, and the @b empty
 *  Boolean set is @c 𝔹{} (default-constructed).
 */
export using 𝔹 = FiniteBooleanSetOf<>;

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

// (1) IsSet anchor: 𝔹 is a bona-fide set (membership morphism bool → Ω).
static_assert(dedekind::category::IsSet<
                  decltype(dedekind::category::ambient_set<bool>(𝔹{}))>,
              "𝔹 must be the canonical IsSet anchor for bool.");

// (2) Syntax (the C++ operator surface that maps to 𝔹's algebra).
//   - The logical surface (∧, ∨, ¬) lives in `category:logic` as
//     `HasLogicalOperators<bool>` and is witnessed there.
//   - The lattice / bitwise surface (&, |, ^, ~) lives in
//     `order:lattice` as `HasLatticeOperators<bool>` and is witnessed
//     there; bool's bitwise operators promote to int but the loose
//     `convertible_to<bool>` shape lets the concept fire.
static_assert(dedekind::category::HasLogicalOperators<bool>,
              "𝔹's logical-operator surface is bool's (&&, ||, !).");
static_assert(dedekind::order::HasLatticeOperators<bool>,
              "𝔹's lattice-operator surface is bool's (&, |, ^, ~).");

// (3) Semantics (the algebraic structures bool actually carries).
//   - Boolean rig (𝔹, ∨, ∧): canonical commutative idempotent semiring,
//     no additive inverse (the "no negation" carrier).
//   - Boolean ring 𝔽₂ (= 𝔹 under (⊕, ∧)): a Galois field of order 2,
//     with full ring + field laws.
//   - Boolean-ring lattice (the locked `IsOrderLattice<bool>` from
//     PR #394): bool under (bit_xor, bit_and) certifies as a
//     commutative ring AND has the lattice operator surface.
static_assert(
    dedekind::algebra::IsRig<bool, std::logical_or<bool>,
                             std::logical_and<bool>>,
    "𝔹 under (∨, ∧) is the canonical Boolean rig (idempotent commutative "
    "semiring; no additive inverse on the carrier).");
static_assert(
    dedekind::category::IsField<bool, std::bit_xor<bool>, std::bit_and<bool>>,
    "𝔹 under (⊕, ∧) is the Galois field 𝔽₂ (the smallest non-trivial "
    "field; bool's ring structure lives over the bitwise functors, "
    "not over (+, *), per the math-wins-over-C++ stance).");
static_assert(dedekind::order::IsOrderLattice<bool>,
              "𝔹 satisfies IsOrderLattice (the locked Boolean-ring lattice "
              "under (bit_xor, bit_and); both halves of the bundle fire).");

// (4) Primitive-type arrow:  bool ↔ 𝔹 is the trivial domain identity ---
// bool *is* 𝔹's underlying type (Domain = bool); the universal Boolean
// set 𝔹{True, True} accepts every bool, and the empty Boolean set
// 𝔹{} accepts none.
static_assert(𝔹{ClassicalLogic::True, ClassicalLogic::True}(true) ==
                  ClassicalLogic::True,
              "Universal 𝔹 contains true.");
static_assert(𝔹{ClassicalLogic::True, ClassicalLogic::True}(false) ==
                  ClassicalLogic::True,
              "Universal 𝔹 contains false (every bool is a Boolean).");
static_assert(𝔹{}(true) == ClassicalLogic::False,
              "Empty 𝔹 does not contain true.");
static_assert(𝔹{}(false) == ClassicalLogic::False,
              "Empty 𝔹 does not contain false.");

// (5) Adjacent-set arrow: 𝔹 ↪ ℕ via @c embed_𝔹_ℕ in @c :naturals.
// This partition is upstream of @c :naturals, so the witness for the
// monic-arrow registration lives in @c :naturals (registered there as
// @c is_monic_arrow_v = true on @c embed_𝔹_ℕ).

}  // namespace dedekind::numbers
