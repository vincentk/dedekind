/**
 * @file ontology:category.discrete
 * @partition :discrete
 * @brief Level 0.25: The Discrete Infrastructure (Points as Arrows).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 * 
 * This partition reifies the simplest possible categorical structure: the
 * Discrete Category, where the only morphisms are identities. In our 
 * structuralist framework, this provides the "Point" (1) and the "Empty" (0)
 * objects, as well as the means to treat any species element as a 
 * constant mapping.
 *
 * @quote
 * 「道生一，一生二，二生三，三生萬物。」
 * (The Tao produced One; One produced Two; Two produced Three; 
 *  Three produced All things.)
 * — 老子 (Laozi), 道德經
 *
 * @section The_Discrete_Duality
 * While Level 0 (:morphism) provides the syntax of the Arrow, Level 0.25 
 * provides the "Discrete Points" that populate the objects:
 * - @ref One (1)   : The Terminal Object; the categorical "Atom" of Truth.
 * - @ref Zero (0)  : The Initial Object; the categorical "Atom" of Falsehood.
 * - @ref ConstantMorphism : A mapping that factors through the Terminal 
 *                          Object, collapsing the domain into a single point.
 *
 * @section Role_in_the_Fractal
 * By establishing the Discrete Floor here, we enable Level 0.5 (:limit) to 
 * define universal properties (Initiality/Terminality) and Level 1 (:total) 
 * to define algebraic identities (Zero/Unit) without circular dependencies.
 */

module;

#include <algorithm>
#include <concepts>
#include <functional>

export module dedekind.category:discrete;

import :morphism;
import :species;

namespace dedekind::category {

/** @brief The Terminal Object (1): The Discrete Point. */
export struct One final {
  constexpr bool operator==(const One&) const noexcept { return true; }
};

/** @brief The Initial Object (0): The Empty Discrete Space. */
export struct Zero final {
  Zero() = delete;
};

/**
 * @brief The Constant Morphism f: A -> B.
 * @details Maps every element of A to a fixed element c in B.
 */
export template <typename A, typename B>
struct ConstantMorphism final {
  using Domain = A;
  using Codomain = B;
  B value;

  constexpr B operator()(const A&) const noexcept { return value; }
};

/** @section Registration_Atomic_Floor */
template <typename A, typename B, typename Op>
  requires IsPointed<B, Op>
struct identity_registry<ConstantMorphism<A, B>, Op> {
  static constexpr ConstantMorphism<A, B> value{identity_v<B, Op>};
};

/**
 * @section Constant_Morphism_Axioms
 * A Constant Mapping is inherently associative under any operation
 * because it is a fixed sink: c ∘ (c ∘ c) = c.
 */
template <typename A, typename B, typename Op>
inline constexpr bool is_associative_v<ConstantMorphism<A, B>, Op> = true;

/** @section Unified_Ideal_Factories */
export template <typename A, typename B, typename Op = std::plus<B>>
  requires IsPointed<B, Op>
constexpr auto zero() {
  return ConstantMorphism<A, B>{identity_v<B, Op>};
}

export template <typename A, typename B, typename Op = std::multiplies<B>>
  requires IsPointed<B, Op>
constexpr auto unit() {
  return ConstantMorphism<A, B>{identity_v<B, Op>};
}

static_assert(IsPointed<decltype(zero<int, int>()), std::plus<int>>);
static_assert(IsPointed<decltype(unit<int, int>()), std::multiplies<int>>);

// 4. Verify functional correctness (maps to the 'Point' 0)
static_assert(
    zero<int, int, std::plus<int>>()(123) == 0,
    "Logic Error: Zero mapping failed to return the identity element.");
}  // namespace dedekind::category
