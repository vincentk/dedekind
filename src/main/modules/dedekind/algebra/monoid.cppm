/**
 * @file ontology:category.cppm
 * @partition :monoid
 * @brief Level 0.2: The Rule of Identity (Semigroups and Monoids).
 *
 * @section The_Identity_Axiom
 * « Wprowadzenie pojęcia kategorii pozwala na jednolite traktowanie
 *   różnych struktur matematycznych. »
 *  (The introduction of the concept of a category allows for a uniform
 *   treatment of various mathematical structures.)
 *  — Samuel Eilenberg, 'Algebraic Topology'
 *
 * @details
 * A Monoid is the simplest "Small Category"—a species with a single object
 * where every morphism (element) can be composed associatively and
 * possesses a neutral identity.
 */

export module dedekind.category:monoid;

import :species;

namespace dedekind::category {

/**
 * @concept IsSemigroup
 * @brief An associative species closed under binary composition.
 *
 * @tparam T A species already established as a Magma.
 * @tparam Op The binary operator (e.g., std::plus).
 */
export template <IsMagma<Op> T, typename Op>
concept IsSemigroup = IsAssociative<T, Op>;

/**
 * @concept IsMonoid
 * @brief A Semigroup endowed with a Two-Sided Identity (Unit).
 * @axiom 1 * a = a = a * 1
 */
export template <IsSemigroup<Op> T, typename Op>
concept IsMonoid = HasIdentity<T, Op>;

/**
 * @concept IsCommutativeMonoid
 * @brief A Monoid where the order of composition is invariant.
 * @details This is the skeletal structure of Addition (+) and Logic (&&).
 */
export template <IsMonoid<Op> T, typename Op>
concept IsCommutativeMonoid = IsCommutative<T, Op>;

}  // namespace dedekind::category
