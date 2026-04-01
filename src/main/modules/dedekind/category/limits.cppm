/**
 * @file ontology:category.cppm
 * @partition :limits
 * @brief Level 0.6: The Boundary Objects (Initial and Terminal).
 *
 * @section Limits: The 0 and 1 of the Topos
 * In the Dedekind topos, the Initial (0) and Terminal (1) objects
 * define the logical boundaries of all characteristic morphisms.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:limits;

import :etcs;  // For IsCharacteristic

namespace dedekind::category {

/**
 * @concept IsInitialObject
 * @brief The "Zero" of the Category (0).
 * @details ∀x ∈ Domain: χ(x) == False.
 */
export template <typename S, typename Ω = ClassicalLogic>
concept IsInitialObject =
    IsCharacteristic<S, Ω> && requires(const S s, const typename S::Domain x) {
      // The Annihilator: Proof of absolute falsehood in Ω.
      { s(x) == Ω::False } -> std::same_as<typename Ω::type>;

      // Semantic: ensure it actually evaluates to the constant False
      requires(s(x) == Ω::False);
    };

/**
 * @concept IsTerminalObject
 * @brief The "One" of the Category (1).
 * @details ∀x ∈ Domain: χ(x) == True.
 */
export template <typename S, typename Ω = ClassicalLogic>
concept IsTerminalObject =
    IsCharacteristic<S, Ω> && requires(const S s, const typename S::Domain x) {
      // The Identity: Proof of absolute truth in Ω.
      { s(x) == Ω::True } -> std::same_as<typename Ω::type>;

      // Semantic: ensure it actually evaluates to the constant True
      requires(s(x) == Ω::True);
    };

}  // namespace dedekind::category
