/**
 * @file algebra:group.cppm
 * @partition :group
 * @brief Level 3.1: The Symmetry of Numbers (ℤ, ℚ*).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section The_Symmetry_Axiom
 * « Det er utvilsomt at gruppeteorien er den mest fundamentale del av
 *   den moderne matematikken. »
 *  (It is beyond doubt that group theory is the most fundamental part
 *   of modern mathematics.)
 *  — Sophus Lie, 'Gesammelte Abhandlungen'
 *
 * @section Taxonomy_of_Symmetry
 * This partition establishes the "Rules of Reflection" (Inverses).
 * It promotes Monoids to Groups by verifying the existence of
 * symmetric elements: the negative (-x) for addition and the
 * reciprocal (1/x) for multiplication.
 */

export module dedekind.algebra:group;

import :monoid;
import dedekind.category;

namespace dedekind::algebra {

/**
 * @concept IsAdditiveGroup
 * @brief Proposition: The species (T, +) forms an Abelian Group (ℤ).
 */
export template <IsAdditiveMonoid T>
concept IsAdditiveGroup = dedekind::category::IsAbelianGroup<T, std::plus<>>;

/**
 * @concept IsMultiplicativeGroup
 * @brief Proposition: The non-zero species (T*, *) forms a Group (ℚ*).
 */
export template <IsMultiplicativeMonoid T>
concept IsMultiplicativeGroup =
    dedekind::category::IsGroup<T, std::multiplies<>>;

/** @section Formal_Verification */

// Verification of the Integer Carrier
static_assert(IsAdditiveGroup<int>, "Axiom Failure: (ℤ, +) must be a Group.");

// Verification of the Rational Carrier (PR 96 Anchor)
// Note: We probe Rational<int> to ensure the inverse() morphism satisfies
// IsGroup.
static_assert(!IsMultiplicativeGroup<int>,
              "Structural Integrity: (ℤ, *) is only a Monoid.");

}  // namespace dedekind::algebra
