/**
 * @file dedekind/category/discrete.cppm
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
 *「三十輻，共一轂，當其無，有車之用。」
 * (Thirty spokes meet at a single hub; it is the empty space (the relation)
 * that makes the wheel useful.) — 老子 (Laozi)
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
 *
 * @note "In dedekind.category:discrete, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
 */

module;

#include <algorithm>
#include <concepts>
#include <functional>

export module dedekind.category:discrete;

import :morphism;
import :small;
import :species;

namespace dedekind::category {

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

/**
 * @section Discrete_Categories
 * A Discrete Category is a Small Category where the only morphisms are
 * identities.
 */
export template <typename Cat>
concept IsDiscreteCategory =
    IsCategory<Cat> && std::same_as<typename Cat::Arrow, typename Cat::Id>;

/**
 * @brief DiscreteCategory realization using the Identity Morphism.
 * Lifts a species T into a category where every arrow is the Identity<T>.
 */
export template <typename T>
struct DiscreteCategory final {
  // 1. The Label: Species is the underlying type
  using Species = T;

  using Arrow = Identity<T>;

  // Type alias uses Capitalized name
  using Id = Identity<T>;

  // Factory function uses lower_case name
  static constexpr Id id_c(const T&) noexcept { return category::id<T>(); }
};

// 1. Define the Subject: A discrete category over a primitive 'int'
using IntCat = DiscreteCategory<int>;

// 2. Test: Does it satisfy the general Category contract?
// This verifies: Arrow exists, id exists, and factory works.
static_assert(
    IsCategory<IntCat>,
    "Verification Failed: DiscreteCategory<int> must satisfy IsCategory.");

// 3. Test: Does it satisfy the Discrete-specific property?
// This verifies: Arrow type is identical to the Identity type.
static_assert(
    IsDiscreteCategory<IntCat>,
    "Verification Failed: DiscreteCategory<int> must be IsDiscreteCategory.");

// 4. Test: Operational check of the identity factory
// Verifies the factory's static callability and return type.
static_assert(
    requires(int x) {
      // The return type of the function id_c must match the type alias Id
      { IntCat::id_c(x) } -> std::same_as<typename IntCat::Id>;
    }, "Verification Failed: id_c factory does not return the correct type.");

}  // namespace dedekind::category
