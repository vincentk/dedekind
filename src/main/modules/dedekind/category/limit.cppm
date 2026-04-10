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
/**
 * @file ontology:category.cppm
 * @partition :limit
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

/** @section Universal_Aliases */
export using One  = std::monostate; 
export using Zero = std::nullptr_t;

/** 
 * @section Totality_Bridge
 * We define 'IsMorphicTotal' to avoid the name collision with 'IsTotal' (binary).
 * An arrow to 'One' is axiomatically total (the Juliet "scent" of a sink).
 */
export template <typename F>
concept IsMorphicTotal = 
    IsArrow<F> && 
    std::same_as<typename SpeciesTraits<F>::Codomain, One>;

/** @concept IsTerminalMorphism */
export template <typename F>
concept IsTerminalMorphism =
    IsArrow<F> && 
    IsMorphicTotal<F> && 
    std::same_as<typename SpeciesTraits<F>::Codomain, One>;

/** @brief The unit morphism factory !: T -> One */
export template <typename T>
auto unit() {
    return arrow<T, One>([](const T&) { return One{}; });
}

/** @concept IsTerminalObject */
export template <typename T>
concept IsTerminalObject =
    std::same_as<T, One> || 
    IsTerminalMorphism<decltype(unit<T>())>;

/** @brief The zero morphism factory ?: Zero -> T */
export template <typename T>
auto zero() {
    return arrow<Zero, T>([](Zero) -> T { 
        // Logically unreachable annihilator
        std::terminate(); 
    });
}

/** @concept IsInitialObject */
export template <typename T>
concept IsInitialObject = 
    std::same_as<T, Zero> ||
    requires {
        requires IsArrow<decltype(zero<int>())>;
    };

/** @section Realizations */
using TerminalCategory = DiscreteCategory<One>;
using InitialCategory  = DiscreteCategory<Zero>;

}  // namespace dedekind::category
