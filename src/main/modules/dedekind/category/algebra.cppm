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

/** @concept IsSemigroup: Associative Magma */
export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && IsAssociative<T, Op>;

/** @concept IsMonoid: Semigroup + Identity */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && HasIdentity<T, Op>;

/** @concept IsGroup: Monoid + Inverse */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsInvertible<T, Op>;

/** @concept IsAbelianGroup: Commutative Group */
export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutative<T, Op>;

}  // namespace dedekind::category
