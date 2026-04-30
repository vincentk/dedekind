/**
 * @file dedekind/numbers/integral.cppm
 * @partition :integral
 * @brief std::integral umbrella over uint / sint / bool.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section integral__Honest_Stance
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
 * @section integral__Dispatch_Pattern
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
 * @section integral__Honesty_Obligation
 * The umbrella's @b negative claim (no std::integral sibling is a field
 * under arithmetic) is discharged by combining three witnesses, two of
 * which are pinned upstream and not restated here (vacuous restatements
 * are noise — see the project's cross-partition assertion-style memo):
 * @c !IsField<unsigned @c int> in @c :uint, @c !IsField<int> in
 * @c algebra/field.cppm (the global signed-family witness).  The single
 * pin in this partition closes the umbrella by addressing the third
 * sibling — @c bool under arithmetic operators — whose rejection has
 * not been pinned anywhere else (the @c :boolean partition pins the
 * XOR/AND field @b acceptance, which is a structurally different
 * object).
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

namespace dedekind::numbers {

/** @section integral__Formal_Verification
 *
 * The per-sibling field rejections are pinned upstream and are NOT restated
 * here (vacuous restatements are noise — see the project's cross-partition
 * assertion-style memo):
 *   * @c !IsField<unsigned int, std::plus, std::multiplies> --- pinned in
 *     @c :uint (closes #417).
 *   * @c !IsField<int> --- pinned at @c algebra/field.cppm (the global
 *     umbrella witness for the signed family; signed-overflow UB defeats
 *     closure under arithmetic before the field axioms are reachable).
 *
 * The @c bool sibling is the only one whose arithmetic-operator field
 * rejection is genuinely new at the umbrella level: the @c :boolean
 * partition pins @c IsField<𝔹, std::bit_xor, std::bit_and> (𝔹 = 𝔽₂ under
 * XOR / AND), but the arithmetic-operator reading is a structurally
 * different object and the rejection has not been pinned before.  Filed
 * here so the umbrella reader does not silently inherit the XOR / AND
 * result.
 */
static_assert(
    !dedekind::algebra::IsField<bool, std::plus<bool>, std::multiplies<bool>>,
    "bool is NOT a field under std::plus / std::multiplies: "
    "the arithmetic operators promote to int; the field reading "
    "of bool lives at std::bit_xor / std::bit_and (see :boolean).");

}  // namespace dedekind::numbers
