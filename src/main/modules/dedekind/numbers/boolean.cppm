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
#include <utility>  // std::forward (used in embed_𝔹_𝕂3's set-level lift)

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

/** @section numbers_boolean__Canonical_Species_Spine
 *
 * The canonical Boolean species symbol @c 𝔹 names the @b universe value
 * @c 𝔸<bool> (post-#559).  The carrier is @c bool, addressed directly in
 * template-type-parameter positions; @c 𝔹 is the constexpr
 * @c 𝔹 value the set-builder
 * DSL takes as ambient.  The Boolean structures the carrier @c bool
 * carries are @c (bool, @c ⊕, @c ∧, @c 0, @c 1) — the Galois field
 * 𝔽₂ — and @c (bool, @c ∨, @c ∧) — the canonical Boolean rig.  The
 * universe-vs-carrier split parallels the upper tower (@c ℕ migrated
 * in #559's ℕ slice; @c ℤ / @c ℚ / @c ℝ / @c ℂ / @c 𝔻 follow under
 * the same #559 plan).
 *
 * The canonical home of @c 𝔹 is @c dedekind::sets::𝔹 (upstream of this
 * partition), which also carries its witnesses; the sets over @c bool are
 * the two points @c 𝔹 @c | @c (π @c == @c v) and the reducer's nodes on them.
 *
 * Acts as the base of the embedding chain
 * @c 𝔹 @c ↪ @c ℕ @c ↪ @c ℤ @c ↪ @c ℚ @c ↪ @c ℝ @c ↪ @c ℂ.
 */

/** @section numbers_boolean__Formal_Verification */

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
//      to (1): predicate vs. listed view of the same 𝔸-shaped set.
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
    dedekind::category::IsRig<bool, std::logical_or<bool>,
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

// (5) Adjacent-set arrow: 𝔹 ↪ ℕ via @c embed_𝔹_uint_ in @c :natural.
// This partition is upstream of @c :natural, so the witness for the
// monic-arrow registration lives in @c :natural (registered there as
// @c is_monic_arrow_v = true on @c embed_𝔹_uint_).

// (5a) Set-level lift witnesses: @c embed_𝔹_𝕂3 on @c Singleton<true>
// lands at @c Ternary::True, and on @c Singleton<false> at
// @c Ternary::False.  Both pinned at the @b value level so the
// pivot equality is constant-evaluated, not just the codomain type
// (the type-only form would only check that we land in some
// @c Singleton<Ternary>, not which inhabitant).  Sister anchor
// to PR #624's @c embed_𝔹_ℕ witness in @c :natural --- same shape,
// different codomain.
// embed_𝔹_𝕂3 (set-level lift) value-witnesses live with the arrow itself in
// @c :algebra:boolean — keeping the value-level pin co-located with the
// arrow definition avoids the cross-partition lookup the bare names would
// require here (the names are not in @c dedekind::numbers post-PR-#676 move).

// embed_𝔹_𝕂3_ trait registrations (is_monic_arrow_v / is_embedding_functor_v)
// live with the arrow itself in @c :algebra:boolean (PR #676 review round).

}  // namespace dedekind::numbers
