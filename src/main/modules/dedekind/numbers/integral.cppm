/**
 * @file dedekind/numbers/integral.cppm
 * @partition :integral
 * @module dedekind.numbers:integral
 * @brief Level 4: Umbrella note over @c std::integral —
 *        the union of three structurally-distinct algebras
 *        (@c std::unsigned_integral, @c std::signed_integral, @c bool).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Honest_Stance
 * The standard-library hierarchy
 *
 *   @c std::integral @c = @c std::unsigned_integral @c ||
 *                       @c std::signed_integral @c || @c std::same_as<bool>
 *
 * unifies under a single concept three carriers that this library has
 * classified as @b structurally @b distinct algebras:
 *
 *   * @c std::unsigned_integral — finite cyclic ring @c ℤ/2^wℤ (full
 *     axiomatic ring under modular wrap; closes #417, see @c :uint).
 *   * @c std::signed_integral  — literal ring-operator surface ✓,
 *     axiomatic ring laws ✗ (UB on overflow; closes #418, see @c :sint).
 *   * @c bool                  — Boolean rig under (∨, ∧); Galois field
 *     𝔽₂ under (⊕, ∧); not a field under arithmetic (closes #400 /
 *     PR #407, see @c :boolean).
 *
 * Generic code constrained on @c std::integral @c T therefore covers
 * three different algebras whose only durable common ground is the
 * @b syntactic operator surface — nothing axiomatic survives the union.
 *
 * @section Dispatch_Pattern
 * Callers that need axiomatic guarantees must dispatch on the sibling
 * concepts rather than on the umbrella:
 *
 *   * @c std::unsigned_integral — pin @c IsRing / @c IsCyclicGroup.
 *   * @c std::signed_integral  — pin @c HasRingOperators (operator
 *     surface only; the axiomatic ring witness would be a false claim
 *     under signed-overflow UB).
 *   * @c bool                  — pin @c IsField under @c std::bit_xor /
 *     @c std::bit_and; @b not under @c std::plus / @c std::multiplies
 *     (bool's arithmetic operator surface promotes to @c int and the
 *     resulting structure is not a field).
 *
 * @section Honesty_Obligation
 * The witness blocks below pin the umbrella's @b negative claim
 * (none of the three siblings is a field under arithmetic) at the
 * two carriers where the rejection is @b not already pinned upstream:
 * @c int (the @c :sint partition pins @c HasRingOperators<int> and
 * @c !Group_ℤ<int> but not @c !IsField directly) and @c bool (the
 * @c :boolean partition pins the XOR/AND field reading; the
 * arithmetic-operator reading is filed here).  The @c unsigned @c int
 * field rejection is already in @c :uint and is @b not restated
 * here (vacuous restatements are noise — see the project's
 * cross-partition assertion-style memo).
 *
 * @note "Wer wagt es, Rittersmann oder Knapp,
 *        Zu tauchen in diesen Schlund?"
 *       ("Who dares, knight or squire, / To dive into this gulf?")
 *       — Friedrich Schiller, *Der Taucher* (1797).
 */
module;

#include <concepts>
#include <functional>

export module dedekind.numbers:integral;

import dedekind.algebra; // IsField (umbrella negative-claim witness)
import :boolean;         // sibling: bool classification (closes #400)
import :sint;  // sibling: signed_integral classification (closes #418)
import :uint;  // sibling: unsigned_integral classification (closes #417)

namespace dedekind::numbers {

/** @section Formal_Verification — umbrella negative claims under arithmetic */

// (1) @c int is not a field under @c std::plus / @c std::multiplies.  The
//     :sint partition documents that signed-overflow UB defeats the
//     axiomatic ring witness; the field rejection is the strictly weaker
//     downstream consequence and is filed here at the umbrella level so
//     the std::integral reader sees all three siblings rejected as fields
//     under arithmetic in one place.
static_assert(
    !dedekind::algebra::IsField<int, std::plus<int>, std::multiplies<int>>,
    "int is NOT a field under std::plus / std::multiplies: "
    "signed-overflow UB defeats closure under arithmetic before "
    "the field axioms are even reachable.");

// (2) @c bool is not a field under @c std::plus / @c std::multiplies.  The
//     :boolean partition pins @c IsField<𝔹, std::bit_xor, std::bit_and>
//     (𝔹 = 𝔽₂ under XOR / AND); the arithmetic-operator reading is a
//     different structure and is not a field.  Pinned here so the
//     umbrella reader does not silently inherit the XOR / AND result.
static_assert(
    !dedekind::algebra::IsField<bool, std::plus<bool>, std::multiplies<bool>>,
    "bool is NOT a field under std::plus / std::multiplies: "
    "the arithmetic operators promote to int; the field reading "
    "of bool lives at std::bit_xor / std::bit_and (see :boolean).");

}  // namespace dedekind::numbers
