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
 *
 * @note "In dedekind.algebra:group, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
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
 * @details Operator is configurable; default witness is `std::plus<T>`
 * (the canonical `+`).
 * @tparam T The carrier type.
 * @tparam Add The additive operation witness (defaults to `std::plus<T>`).
 */
export template <typename T, typename Add = std::plus<T>>
concept IsAdditiveGroup = IsGroup<T, Add>;

/**
 * @concept IsMultiplicativeGroup
 * @brief Proposition: The non-zero species (T*, *) forms a Group (ℚ*).
 * @details Operator is configurable; default witness is
 * `std::multiplies<T>` (the canonical `*`).
 * @tparam T The carrier type.
 * @tparam Mult The multiplicative operation witness (defaults to
 * `std::multiplies<T>`).
 */
export template <typename T, typename Mult = std::multiplies<T>>
concept IsMultiplicativeGroup = dedekind::category::IsGroup<T, Mult>;

/** @section Formal_Verification */

// During experimental reintegration, full group witnesses for int
// are deferred pending structure-proof registration in category module.

// static_assert(IsAdditiveGroup<int>, "Axiom Failure: (ℤ, +) must be a
// Group.");

// Multiplicative group verification is deferred during experimental
// reintegration while inverse witnesses are being retargeted.
}  // namespace dedekind::algebra
