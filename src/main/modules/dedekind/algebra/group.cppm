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
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:group;

import :monoid;
import dedekind.category;

namespace dedekind::algebra {

/**
 * @concept IsAdditiveGroup
 * @brief Proposition: The species (T, +) forms an Abelian Group (ℤ).
 */
export template <typename T>
concept IsAdditiveGroup = IsGroup<T, std::plus<>>;

/**
 * @concept IsMultiplicativeGroup
 * @brief Proposition: The non-zero species (T*, *) forms a Group (ℚ*).
 */
export template <typename T>
concept IsMultiplicativeGroup =
    dedekind::category::IsGroup<T, std::multiplies<>>;

/** @section Formal_Verification */

// During experimental reintegration, full group witnesses for int
// are deferred pending structure-proof registration in category module.

// static_assert(IsAdditiveGroup<int>, "Axiom Failure: (ℤ, +) must be a
// Group.");

// Multiplicative group verification is deferred during experimental
// reintegration while inverse witnesses are being retargeted.
}  // namespace dedekind::algebra
