/**
 * @file dedekind/category/discrete.cppm
 * @partition :discrete
 * @brief The Discrete Infrastructure (Points as Arrows).
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
 * @section discrete__The_Discrete_Duality
 * While Level 0 (:morphism) provides the syntax of the Arrow, Level 0.25
 * provides the "Discrete Points" that populate the objects:
 * - @ref One (1)   : The Terminal Object; the categorical "Atom" of Truth.
 * - @ref Zero (0)  : The Initial Object; the categorical "Atom" of Falsehood.
 * - @ref ConstantMorphism : A mapping that factors through the Terminal
 *                          Object, collapsing the domain into a single point.
 *
 * @section discrete__Role_in_the_Fractal
 * By establishing the Discrete Floor here, we enable Level 0.5 (:limit) to
 * define universal properties (Initiality/Terminality) and Level 1 (:total)
 * to define algebraic identities (Zero/Unit) without circular dependencies.
 *
 * @note 「三十輻，共一轂，當其無，有車之用。」
 * (Thirty spokes meet at a single hub; it is the empty space (the relation)
 * that makes the wheel useful.) — 老子 (Laozi)
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

/**
 * @section discrete__Constant_Morphism_Axioms
 * A Constant Mapping is inherently associative under any operation
 * because it is a fixed sink: c ∘ (c ∘ c) = c.
 */
template <typename A, typename B, typename Op>
inline constexpr bool is_associative_v<ConstantMorphism<A, B>, Op> = true;

/** @section discrete__Constant_Is_Constant
 *  Every argument to one value: the arrow factors through the terminal
 *  object, @f$c \circ\, !@f$, so it is stated with no order on either side
 *  (Lawvere and Rosebrugh @cite lawvere2003sets).  It is the vertical arrow
 *  of the Jlt paper's Figure 2 where the fold is @c n copies of one value
 *  (#981).  Its variances, monotone and antitone under every reflexive order
 *  on the codomain, are registered in @c :total, where that order is in
 *  reach. */
template <typename A, typename B>
inline constexpr bool is_constant_arrow_v<ConstantMorphism<A, B>> = true;
/** @brief The constant arrow by its short name, the paper's spelling.
 *  @tparam X the domain.  @tparam Y the codomain, by default the domain. */
export template <typename X, typename Y = X>
using Constant = ConstantMorphism<X, Y>;

/** @brief The constant arrow's factory, beside @c id<A>() and @c endo<A>(f):
 *  the domain named, the codomain the value's. */
export template <typename A, typename B>
constexpr auto constant(B y) {
  return Constant<A, B>{y};
}

/** @brief The equality test against one value as an arrow @f$A \to 2@f$,
 *  the characteristic map of the point @c x: the condition a guard asks of a
 *  state (the Jlt paper's Listing 3, @c until(equals(1), step)).
 *  @tparam A the object, its values comparable. */
export template <std::equality_comparable A>
struct Equals {
  using Domain = A;
  using Codomain = bool;
  A value;
  /** @param a an element.  @return whether it is the value. */
  constexpr bool operator()(const A& a) const { return a == value; }
  constexpr bool operator==(const Equals&) const = default;
};
/** @brief The equality test's factory.  @param x the value. */
export template <std::equality_comparable A>
constexpr Equals<A> equals(A x) {
  return {x};
}
static_assert(IsArrow<Equals<int>> && equals(1)(1) && !equals(1)(2),
              "equals(x) is the arrow a ↦ (a == x)");

// NOTE (#637 re-home): the algebra-flavoured @c zero<A, B, Op>() factory
// (the constant morphism pointing at @c unit<B>(Op{})) plus the @c
// unit_registry specialisation for
// @c ConstantMorphism<A, B> moved to @c :total, where they sit with @c
// IsPointed (their primary user) and the @c IsUnitalMagma / @c IsMonoid
// hierarchy.  These constructions are universal-algebra-flavoured
// (chosen-element witnesses), not discrete-category-flavoured; @c
// :discrete keeps the categorical @c ConstantMorphism construct itself
// and the @c is_associative_v specialisation (which holds unconditionally
// without an identity).  Consumers reach the algebra-flavoured factories
// through @c :total or via @c import dedekind.category.

/**
 * @section discrete__Discrete_Categories
 * A Discrete Category is a Small Category where the only morphisms are
 * identities.
 *
 * @section discrete__Set_to_Cat_Embedding
 * The discrete-category construction realises the canonical embedding
 * @c Set @c ↪ @c Cat: every set @c S lifts to the discrete category
 * @c Disc(S) whose objects are the elements of @c S and whose only
 * arrows are identities.  In categorical terms, @c Disc is the (unique
 * up to natural iso) left adjoint to the underlying-objects functor
 * @c U @c : @c Cat @c → @c Set; the adjunction @c Disc @c ⊣ @c U is
 * the textbook spelling of "every set IS a discrete category".  The
 * @c DiscreteCategory<T> realisation below witnesses the lift on
 * primitive carrier types; a generic @c IsSet-aware lift for the
 * project's set DSL is tracked as a follow-up (the @c IsSet concept
 * lives in @c category:etcs but the DSL sets live further downstream,
 * so a lift-aware partition would have to live downstream of both).
 */
export template <typename Cat>
concept IsDiscreteCategory =
    IsSmallCategory<Cat> && std::same_as<typename Cat::Arrow, typename Cat::Id>;

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
    IsSmallCategory<IntCat>,
    "Verification Failed: DiscreteCategory<int> must satisfy IsSmallCategory.");

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
