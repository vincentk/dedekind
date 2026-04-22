/**
 * @file algebra:ring.cppm
 * @partition :ring
 * @brief Level 3.2: The Rules of Harmony (The Semiring Synthesis).
 *
 * @copyright 2026 The Dedekind Authors

 *
 * @note „Was beweisbar ist, soll in der Wissenschaft nicht ohne Beweis
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
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsSemiring =
    IsAdditiveMonoid<T, Add> && IsMultiplicativeMonoid<T, Mult> &&
    dedekind::category::IsLinearAction<T, T, Mult, Add>;

/** @concept IsRig: The "Natural" Harmony (No negatives) */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRig = dedekind::category::IsRig<T, Add, Mult>;

/** @concept IsRng: The "Identity-less" Harmony (No unit) */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRng = dedekind::category::IsRng<T, Add, Mult>;

/**
 * @concept IsRing
 * @brief A set that is both a Semiring AND an Abelian Group under addition.
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRing = dedekind::category::IsRing<T, Add, Mult> &&
                 IsSemiring<T, Add, Mult> && IsAdditiveGroup<T, Add>;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is also commutative.
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsCommutativeRing =
    IsRing<T, Add, Mult> && dedekind::category::IsCommutative<T, Mult>;

/** @section Formal_Verification */

// unsigned int with wrapping arithmetic is the canonical total commutative ring:
// IsPeriodic (wraps at 2^N) satisfies IsTotal → IsMagma → IsMonoid → IsGroup.
static_assert(IsRing<unsigned int>,
              "unsigned int must satisfy IsRing (wrapping arithmetic).");
static_assert(IsCommutativeRing<unsigned int>,
              "unsigned int must satisfy IsCommutativeRing.");

// Modular<N> is the archetypal finite commutative ring Z/NZ.
static_assert(IsRing<Modular<256>>,
              "Modular<256> must satisfy IsRing (Z/256Z).");
static_assert(IsCommutativeRing<Modular<256>>,
              "Modular<256> must satisfy IsCommutativeRing.");

// bool with OR/AND is an idempotent commutative semiring (the Boolean rig).
// It is not a Ring: there is no additive inverse for True (True + x != False).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>,
              "bool must satisfy IsRig (Boolean semiring under OR/AND).");

}  // namespace dedekind::algebra
