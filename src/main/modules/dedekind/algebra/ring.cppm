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

export module dedekind.algebra:ring;

import :monoid;
import :group;
import dedekind.category;

namespace dedekind::algebra {

/**
 * @concept IsSemiring
 * @brief The Unification of Algebra and Action (The Rig).
 * @details A species where (T,+) is a Commutative Monoid and (T,*) is a Monoid.
 *          In the Dedekind topos, this is a "Semimodule over itself."
 */
export template <typename T>
concept IsSemiring = IsAdditiveMonoid<T> && IsMultiplicativeMonoid<T> &&
                     dedekind::category::IsLinearAction<T, T>;

/**
 * @concept IsRing
 * @brief A set that is both a Semiring AND an Abelian Group under addition.
 */
export template <typename T>
concept IsRing = IsSemiring<T> && IsAdditiveGroup<T>;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is also commutative.
 */
export template <IsRing T>
concept IsCommutativeRing =
    IsMultiplicativeMonoid<T> &&
    dedekind::category::IsCommutative<T, std::multiplies<>>;

/** @section The_Point_Free_Infix_Engine */

// If T is established as a Ring, we grant the Point-Free Engine
// permission to use the standard operator overloads.
export template <IsRing T>
constexpr T operator*(T a, T b) {
  return std::multiplies<T>{}(a, b);
}

}  // namespace dedekind::algebra
