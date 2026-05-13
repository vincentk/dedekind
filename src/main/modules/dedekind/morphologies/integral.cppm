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

export module dedekind.morphologies:integral;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :cyclic;

namespace dedekind::morphologies {
using namespace dedekind::algebra;
using namespace dedekind::order;
using namespace dedekind::sets;

/**
 * @concept IsInteger: integer-shape operator-surface concept
 *  order on the carrier). */
export template <typename T>
concept IsInteger =
    IsAlgebra<T, std::plus<T>, std::multiplies<T>, std::modulus<T>> &&
    // IsPreOrdered<T> may be rejected, as reflexivity is not guaranteed.
    // Instead, require the non-reflexive variants:
    std::three_way_comparable<T> && requires(T a, T b) {
      { a - b } -> std::same_as<T>;
      { a / b } -> std::same_as<T>;
    };

export template <typename T>
concept IsSaturatingInteger =
    IsInteger<T> && is_translation_invariant_ordered_v<T> && !IsCyclic<T>;

export template <typename T>
concept IsCyclicInteger =
    IsInteger<T> && !is_translation_invariant_ordered_v<T> && IsCyclic<T>;

/** @section integral__Formal_Verification Umbrella negative claims under
 * arithmetic.
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
static_assert(!IsField<bool, std::plus<bool>, std::multiplies<bool>>,
              "bool is NOT a field under std::plus / std::multiplies: "
              "the arithmetic operators promote to int; the field reading "
              "of bool lives at std::bit_xor / std::bit_and (see :boolean).");

static_assert(!dedekind::category::IsClosedUnderBinary<bool, std::plus<>>,
              "bool is NOT closed under std::plus / std::multiplies. Instead, "
              "it promotes to int.");

static_assert(dedekind::category::IsClosedUnderBinary<bool, std::plus<bool>>,
              "... but std::plus<bool> casts the result under the hood.");

static_assert(!IsAlgebra<bool, std::plus<>, std::multiplies<>>,
              "bool is NOT closed under std::plus / std::multiplies. Instead, "
              "it promotes to int.");

/**
 * @section rational__Formal_Verification_2
 *
 * @c Rational<I> satisfies @c algebra::IsField via the
 * multiplicative-inverse trait registration above; the Kleene
 * partial-function helpers (@c HonestDivRational, etc.) remain for
 * codepaths that prefer explicit @c Ternary failure over throwing.
 */
static_assert(IsInteger<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must satisfy IsInteger (Euclidean "
              "ring: +, -, *, /, % with two's-complement wrapping).");

static_assert(IsInteger<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> must satisfy IsInteger (Euclidean "
              "signed ring: sign-magnitude arithmetic, overflow-free up to "
              "2^{N*64 - 1}).");

}  // namespace dedekind::morphologies
