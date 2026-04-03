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

/** @concept IsMagma: A species closed under a binary operator. */
export template <typename T, typename Op>
concept IsMagma = requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};

/** @concept IsSemigroup: An associative Magma. */
export template <IsMagma<Op> T, typename Op>
concept IsSemigroup = IsAssociative<T, Op>;

/** @concept IsMonoid: A Semigroup with an Identity. */
export template <IsSemigroup<Op> T, typename Op>
concept IsMonoid = HasIdentity<T, Op>;

/** @concept IsCommutativeMonoid: A Monoid where order is invariant. */
export template <IsMonoid<Op> T, typename Op>
concept IsCommutativeMonoid = IsCommutative<T, Op>;

/** @concept IsGroup: A Monoid where every element is invertible. */
export template <IsMonoid<Op> T, typename Op>
concept IsGroup = IsInvertible<T, Op>;

/** @concept IsAbelianGroup: A Group where the law is commutative. */
export template <IsGroup<Op> T, typename Op>
concept IsAbelianGroup = IsCommutative<T, Op>;

}  // namespace dedekind::category
