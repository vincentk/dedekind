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
 *
 * @section Std_Namespace_Mappings
 * This partition asserts bidirectional mappings between categorical boundary
 * objects and `std` types:
 *
 * | Concept / Alias     | `std` representative  | Categorical role     |
 * |---------------------|-----------------------|----------------------|
 * | `One` (Terminal)    | `std::monostate`      | Unique sink (1)      |
 * | `Zero` (Initial)    | `std::nullptr_t`      | Unique source (0)    |
 *
 * @note "शून्यं शून्येन गुणितं शून्यम्।"
 *       ("Zero multiplied by zero is zero.")
 *       -- ब्रह्मगुप्त (Brahmagupta), ब्रह्मस्फुटसिद्धान्त
 */
module;

#include <concepts>
#include <cstddef>
#include <exception>
#include <variant>  // Required for std::monostate

export module dedekind.category:limit;

import :discrete;
import :morphism;
import :species;

namespace dedekind::category {

/**
 * @brief The Terminal Object (1): the unique sink of every morphism.
 * @details Categorification of `std::monostate` as the Terminal Object.
 * `std::monostate` is a unit type (exactly one value, `{}`), making it the
 * canonical C++ representative of the categorical "1". Every species T admits
 * exactly one morphism T → One (the constant function).
 *
 * @see IsTerminalObject, unit()
 */
export using One = std::monostate;

/**
 * @brief The Initial Object (0): the unique source of every morphism.
 * @details Categorification of `std::nullptr_t` as the Initial Object.
 * `std::nullptr_t` has exactly one value (`nullptr`) that cannot be
 * constructed from anything else, making it the canonical C++ representative
 * of the categorical "0". From Zero, there exists exactly one (logically
 * unreachable) morphism to every species T, the zero morphism.
 *
 * @see IsInitialObject, zero()
 */
export using Zero = std::nullptr_t;

/**
 * @concept IsMorphicTotal
 * @brief A Morphism whose Codomain is the Terminal Object (One).
 * @details A morphism f: A → One is "morphically total" in the categorical
 * sense: it maps every element of its domain to the unique element of One.
 * This is the categorical analogue of a constant function that always returns
 * `std::monostate{}`.
 *
 * @note This is distinct from `IsTotalArrow` (in :total), which concerns
 * whether a morphism is defined on all inputs without undefined behaviour.
 */
export template <typename F>
concept IsMorphicTotal = requires {
  typename F::Domain;
  typename F::Codomain;
} && std::same_as<typename F::Codomain, One>;

/**
 * @concept IsTerminalMorphism
 * @brief A morphism that maps into the Terminal Object (One).
 * @details Alias for `IsMorphicTotal`. A terminal morphism `!: T → One`
 * collapses all information: every element of T maps to the single inhabitant
 * of `std::monostate`.
 */
export template <typename F>
concept IsTerminalMorphism = IsMorphicTotal<F>;

/**
 * @brief The unit morphism factory: produces the unique arrow !: T → One.
 * @details For every species T there is exactly one morphism to the Terminal
 * Object. This factory constructs it as a constant function that ignores its
 * input and returns `One{}` (`std::monostate{}`).
 * @tparam T The domain species.
 */
export template <typename T>
auto unit() {
  return arrow<T, One>([](const T&) { return One{}; });
}

/**
 * @concept IsTerminalObject
 * @brief A type that is (or maps canonically to) the Terminal Object One.
 * @details `std::monostate` (aliased as `One`) is the sole terminal object.
 * Any type T that produces a valid `IsTerminalMorphism` via `unit<T>()` also
 * satisfies this concept.
 */
export template <typename T>
concept IsTerminalObject = std::same_as<T, One>;

/**
 * @brief The zero morphism factory: produces the unique (unreachable) arrow
 *        ?: Zero → T.
 * @details From the Initial Object `Zero` (`std::nullptr_t`) there is exactly
 * one morphism to any species T. Since `nullptr` can never actually appear as
 * input in a well-formed program, this arrow's body is logically unreachable
 * and terminates if ever called.
 * @tparam T The codomain species.
 */
export template <typename T>
auto zero() {
  return arrow<Zero, T>([](Zero) -> T {
    // Logically unreachable annihilator
    std::terminate();
  });
}

/**
 * @concept HasUniqueMorphismTo
 * @brief Asserts that there exists a unique morphism from T to U = One.
 * @details Encodes the universal property of the Terminal Object: for every
 * species T, the factory `unit<T>()` produces the unique arrow T → One.
 */
export template <typename T, typename U>
concept HasUniqueMorphismTo = std::same_as<U, One> && requires {
  { unit<T>() } -> IsTerminalMorphism;
};

/**
 * @concept HasUniqueMorphismFrom
 * @brief Asserts that there exists a unique (unreachable) morphism from Z =
 * Zero to T.
 * @details Encodes the universal property of the Initial Object: for every
 * species T, the factory `zero<T>()` produces the unique arrow Zero → T.
 */
export template <typename Z, typename T>
concept HasUniqueMorphismFrom = std::same_as<Z, Zero> && requires {
  { zero<T>() } -> IsArrow;  // zero<T> is the unique arrow 0 -> T
};

/**
 * @concept IsInitialObject
 * @brief A type that is the Initial Object Zero.
 * @details `std::nullptr_t` (aliased as `Zero`) is the sole initial object.
 * It is the unique type from which a morphism to every other species exists.
 */
export template <typename T>
concept IsInitialObject = std::same_as<T, Zero>;

/** @section Realizations */
export using TerminalCategory = DiscreteCategory<One>;
export using InitialCategory = DiscreteCategory<Zero>;

}  // namespace dedekind::category
