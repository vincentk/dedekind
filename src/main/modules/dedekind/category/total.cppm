/**
 * @file ontology:category.cppm
 * @partition :algebra
 * @brief Level 0.2: The Laws of Composition (The Algebraic Hierarchy).
 *
 * @section The_Categorical_Foundation
 * « Język jest aparatem wyznaczającym obraz świata. Struktura kategorii
 *   jest fundamentem wszelkiego poznania naukowego. »
 *  (Language is an apparatus that determines the image of the world.
 *   The structure of categories is the foundation of all scientific knowledge.)
 *  — Kazimierz Ajdukiewicz, 'Język i Poznanie' (Language and Cognition)
 *
 * @details
 * This partition defines the "Laws of Harmony" for a single species T under
 * a binary operation Op. Following the Polish School of Logic, we treat
 * these structures as species that "mature" as they gain axioms—moving
 * from the primal Magma to the perfect symmetry of an Abelian Group.
 *
 * @copyright 2026 The Dedekind Authors
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:algebra;

import :species;

namespace dedekind::category {

/** @concept IsMagma: T × T → T */
export template <typename T, typename Op>
concept IsMagma = requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};

/** 
 * @concept IsMagmoid
 * @brief Level 0.2: A Magma with an Identity (0 or 1).
 * @details This is the formal home for IEEE 754 (0.0).
 */
export template <typename T, typename Op>
concept IsMagmoid = IsMagma<T, Op> && HasIdentity<T, Op>;

/** 
 * @concept IsLoop
 * @brief Level 0.3: A Magmoid with Inverses (-x).
 * @details "A Group without the Associativity Axiom." 
 *          Perfect for machine integers and floats.
 */
export template <typename T, typename Op>
concept IsLoop = IsMagmoid<T, Op> && IsInvertible<T, Op>;

// Strictly True: Floats have identity and inverse, but no associativity
static_assert(IsLoop<double, std::plus<double>>);

/** @concept IsSemigroup: Associative Magma */
export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && IsAssociative<T, Op>;

/** @concept IsMonoid: Semigroup + Identity */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && IsMagmoid<T, Op>;

static_assert(!IsMonoid<double, std::plus<double>>); 

export template <typename T, typename Op>
concept IsCommutativeMonoid = IsMonoid<T, Op> && IsCommutative<T, Op>;

static_assert(IsCommutativeMonoid<bool, std::logical_or<bool>>);
static_assert(IsCommutativeMonoid<bool, std::logical_and<bool>>);

// Strictly True: Multiplication has identity (1) and is associative/commutative
static_assert(IsCommutativeMonoid<unsigned int, std::multiplies<unsigned int>>);
static_assert(IsCommutativeMonoid<int, std::multiplies<int>>);

/** @concept IsGroup: Monoid + Inverse */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsLoop<T, Op>;

/** @concept IsAbelianGroup: Commutative Group */
export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutative<T, Op>;

// Strictly True: Unsigned addition is an Abelian Group (Z/2^nZ)
static_assert(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);

}  // namespace dedekind::category
