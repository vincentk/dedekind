/**
 * @file ontology:category.cppm
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
#include <functional>

export module dedekind.category:limit;

import :cartesian;

namespace dedekind::category {

/**
 * @section Universal_Aliases
 * Reifying the paper's Table 1 mapping directly.
 */
export using One = std::monostate;   // The Terminal Object (1)
export using Zero = std::nullptr_t;  // The Initial Object (0)

/** @section Terminal_Identity */
template <typename Op>
struct identity_registry<One, Op> {
  static constexpr One value{};
};

// 2. One is Associative (Trivial mapping)
template <typename Op>
inline constexpr bool is_associative_v<One, Op> = true;

// 3. One is Commutative (Optional, but useful for Lattices)
template <typename Op>
inline constexpr bool is_commutative_v<One, Op> = true;

/**
 * @section Automatic_Totality_Registration
 * Any morphism whose codomain is the Terminal Object (One)
 * is inherently total, as it represents a trivial sink.
 */
template <typename F>
  requires(std::same_as<typename SpeciesTraits<F>::Codomain, One>)
inline constexpr bool is_total_v<F> = true;

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
export template <typename T>
concept IsInitialObject = IsInitialMorphism<decltype(zero<T, T>())>;

/**
 * @concept IsTerminalObject
 * @brief Verification that T behaves as the Terminal Object (1).
 */
export template <typename T>
concept IsTerminalObject =
    IsArrow<F> && IsTotal<F> &&
    std::same_as<typename SpeciesTraits<F>::Codomain, One>;

/** @brief The Terminal Category Realization. */
using TerminalCategory = DiscreteCategory<One>;

/** @brief The Initial Category Realization. */
using InitialCategory = DiscreteCategory<Zero>;

/** @brief Verification: The realization (the hub) is a Discrete Category. */
static_assert(
    IsDiscreteCategory<TerminalCategory>,
    "Categorical Proof: The Terminal Object realization (1) must be Discrete.");

/** @brief Infrastructure check: TerminalCategory must be a valid Category. */
static_assert(
    IsCategory<TerminalCategory>,
    "Infrastructure Error: TerminalCategory failed the IsCategory contract.");

}  // namespace dedekind::category
