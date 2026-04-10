/**
 * @file dedekind/category/limit.cppm
 * @partition :limit
 * @brief Level 0.6: The Boundary Objects (Initial and Terminal).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "The progress of mathematics can be viewed as progress from the
 *  infinite to the finite."
 *  — Gian-Carlo Rota, Indiscrete Thoughts
 *
 * @section Limits: The Universal Boundaries
 * In the Dedekind topos, the Initial (0) and Terminal (1) objects represent
 * the finite "anchors" of a system of otherwise infinite potential relations.
 * They are the unique sinks and sources through which the structure of
 * every other species is measured and made finite.
 */
module;

#include <concepts>
#include <exception>
#include <variant>  // Required for std::monostate

export module dedekind.category:limit;

import :discrete;
import :morphism;
import :species;

namespace dedekind::category {

/**
 * @brief The Constant Morphism f: A -> B.
 * @details the categorification of std::monostate as a constant function,
 * mapping every element of A to a fixed element c in B.
 */
export using One = std::monostate;

/**
 * @brief The zero morphism factory ?: Zero -> T.
 * @details the categorification of std::nullptr_t as a zero morphism,
 * representing an unreachable code path. It maps any input to a logically
 * unreachable state, and thus serves as the Initial Object in the category.
 */
export using Zero = std::nullptr_t;

/**
 * @concept IsMorphicTotal
 *
 * @details A morphism is "Morphic Total" if its codomain is the Terminal Object
 * (One). This concept captures the idea that such morphisms are "total" in the
 * sense that they are defined for all inputs and always yield a valid output
 * (the unique element of One).
 *
 * See also: IsTotal which captures the idea of a species being total with
 * respect to a binary operation.
 */
export template <typename F>
concept IsMorphicTotal = requires {
  typename F::Domain;
  typename F::Codomain;
} && std::same_as<typename F::Codomain, One>;

/** @concept IsTerminalMorphism */
export template <typename F>
concept IsTerminalMorphism = IsMorphicTotal<F>;

/** @brief The unit morphism factory !: T -> One */
export template <typename T>
auto unit() {
  return arrow<T, One>([](const T&) { return One{}; });
}

/** @concept IsTerminalObject */
export template <typename T>
concept IsTerminalObject =
    std::same_as<T, One> || IsTerminalMorphism<decltype(unit<T>())>;

/** @brief The zero morphism factory ?: Zero -> T */
export template <typename T>
auto zero() {
  return arrow<Zero, T>([](Zero) -> T {
    // Logically unreachable annihilator
    std::terminate();
  });
}

export template <typename T, typename U>
concept HasUniqueMorphismTo = std::same_as<U, One> && requires {
  { unit<T>() } -> IsTerminalMorphism;
};

export template <typename Z, typename T>
concept HasUniqueMorphismFrom = std::same_as<Z, Zero> && requires {
  { zero<T>() } -> IsArrow;  // zero<T> is the unique arrow 0 -> T
};

/** @concept IsInitialObject */
export template <typename T>
concept IsInitialObject = std::same_as<T, Zero>;

/** @section Realizations */
export using TerminalCategory = DiscreteCategory<One>;
export using InitialCategory = DiscreteCategory<Zero>;

}  // namespace dedekind::category
