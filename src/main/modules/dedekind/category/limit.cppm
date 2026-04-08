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

export module dedekind.category:limit;

import :cartesian;

namespace dedekind::category {

/**
 * @concept IsTerminalMorphism
 * @brief The "Truth" mapping (! : X -> 1).
 * @details Categorically, the unique morphism to the terminal object.
 *          Ontologically, the morphism where every element maps to 'True'.
 */
export template <typename S>
concept IsTerminalMorphism =
    IsPredicate<S, domain_t<S>> && requires(const S s, const domain_t<S> x) {
      // The result must be the Multiplicative Identity (True) of the Domain's
      // Logic.
      requires s(x) == identity_v<typename GetLogic<domain_t<S>>::type::type,
                                  std::logical_and<>>;
    };

/**
 * @concept IsInitialMorphism
 * @brief The "Falsehood" mapping (? : 0 -> X).
 * @details Categorically, the unique morphism from the initial object.
 *          Ontologically, the morphism where every element maps to 'False'.
 */
export template <typename S>
concept IsInitialMorphism =
    IsPredicate<S, domain_t<S>> && requires(const S s, const domain_t<S> x) {
      // The result must be the Additive Identity (False) of the Domain's Logic.
      requires s(x) == identity_v<typename GetLogic<domain_t<S>>::type::type,
                                  std::logical_or<>>;
    };

/**
 * @section Universal_Objects
 * We define the "Point" (1) and the "Empty" (0) as boundary species.
 */

/** @brief The Terminal Object (1): The species with exactly one inhabitant. */
export struct One final {
  constexpr bool operator==(const One&) const noexcept { return true; }
};

/** @brief The Initial Object (0): The species with no inhabitants. */
export struct Zero final {
  // Inhabitants are impossible; construction is forbidden.
  Zero() = delete;
};

/** @section Registration_Infrastructure */

template <>
struct identity_registry<One, std::multiplies<>> {
  static constexpr One value{};
};

/**
 * @concept IsTerminalObject
 * @brief Verification that T behaves as the Terminal Object (1).
 */
export template <typename T>
concept IsTerminalObject =
    std::same_as<T, One> || IsTerminalMorphism<decltype(unit<T, T>())>;

/**
 * @concept IsInitialObject
 * @brief Verification that T behaves as the Initial Object (0).
 */
export template <typename T>
concept IsInitialObject =
    std::same_as<T, Zero> || IsInitialMorphism<decltype(zero<T, T>())>;

}  // namespace dedekind::category
