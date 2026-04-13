/**
 * @file algebra:ring.cppm
 * @partition :ring
 * @brief Level 3.2: The Rules of Harmony (The Semiring Synthesis).
 *
 * @copyright 2026 The Dedekind Authors
 *
 * @section The_Structuralist_Unity
 * „Was beweisbar ist, soll in der Wissenschaft nicht ohne Beweis
 *  geglaubt werden.“ (What is provable should not be believed without proof.)
 *  — Richard Dedekind
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:ring;

import :monoid;
import :group;
import dedekind.category;

namespace dedekind::algebra {

using namespace dedekind::category;

/**
 * @concept IsSemiring
 * @brief The Unification of Algebra and Action (The Rig).
 * @details A species where (T,+) is a Commutative Monoid and (T,*) is a Monoid.
 *          In the Dedekind topos, this is a "Semimodule over itself."
 */
export template <typename T>
concept IsSemiring = IsAdditiveMonoid<T> && IsMultiplicativeMonoid<T> &&
                     dedekind::category::IsLinearAction<T, T>;

/** @concept IsRig: The "Natural" Harmony (No negatives) */
export template <typename T>
concept IsRig = dedekind::category::IsRig<T, std::plus<>, std::multiplies<>>;

/** @concept IsRng: The "Identity-less" Harmony (No unit) */
export template <typename T>
concept IsRng = dedekind::category::IsRng<T, std::plus<>, std::multiplies<>>;

/**
 * @concept IsRing
 * @brief A set that is both a Semiring AND an Abelian Group under addition.
 */
export template <typename T>
concept IsRing =
    dedekind::category::IsRing<T, std::plus<>, std::multiplies<>> &&
    IsSemiring<T> && IsAdditiveGroup<T>;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is also commutative.
 */
export template <typename T>
concept IsCommutativeRing =
    IsRing<T> && dedekind::category::IsCommutative<T, std::multiplies<>>;

/** @section Atomic_Verification
 * Deferred for experimental reintegration while ring contracts are being
 * retargeted to the active category algebra layer.
 */

}  // namespace dedekind::algebra
