/**
 * @file category:actions.cppm
 * @partition :actions
 * @brief Level 0.5: The Morphisms of Influence (Actions and Representations).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:actions;

import :species;
import :morphisms;
import :algebra;

namespace dedekind::category {

/**
 * @concept IsAction
 * @brief An Action (S ⟳ M) as a Monadic Transformation.
 * @details Axioms: (s1 * s2) * m = s1 * (s2 * m) and 1 * m = m.
 * 
 * @note FIXME: Consider formal alignment with the template-template 
 *       IsMonad concept at a later stage.
 */
export template <typename S, typename M>
concept IsAction = 
    HasIdentity<S, std::multiplies<S>> &&
    IsAssociative<S, std::multiplies<S>> &&
    requires(S s, M m) {
        { s * m } -> std::same_as<M>;
        // Semantic: 1 * m == m
        // Semantic: (s1 * s2) * m == s1 * (s2 * m)
    };

/**
 * @concept IsAdditiveMorphism
 * @brief Formal verification of the first law of linearity: f(x + y) = f(x) + f(y).
 * @details This is "Internal" linearity: the mapping preserves the additive harmony.
 */
export template <typename F, typename M>
concept IsAdditiveMorphism = 
    IsArrow<M, M, F> && 
    IsCommutativeMonoid<M, std::plus<M>> &&
    requires(F f, M m1, M m2) {
        { f(m1 + m2) } -> std::same_as<M>;
    };

/**
 * @concept IsLinearAction
 * @brief The "Jewel": An Action that satisfies the Four Axioms of Harmony.
 * @details This is the categorical ground for Modules and Vector Spaces.
 * 
 * 1. Vector Additivity: s * (m1 + m2) = s*m1 + s*m2
 * 2. Scalar Additivity: (s1 + s2) * m = s1*m + s2*m
 */
export template <typename S, typename M>
concept IsLinearAction = 
    IsAction<S, M> &&
    IsAdditiveMorphism<std::multiplies<>, M> &&
    requires(S s1, S s2, M m) {
        // Scalar Additivity (The "Second Linearity")
        { (s1 + s2) * m } -> std::same_as<M>;
    };

/**
 * @concept IsLinearMorphism
 * @brief Formal verification of the second law: f(s * x) = s * f(x).
 * @details Links the mapping f to an external scalar influence S.
 */
export template <typename F, typename M, typename S>
concept IsLinearMorphism = 
    IsAdditiveMorphism<F, M> && 
    IsAction<S, M> && 
    requires(F f, M m, S s) {
        { f(s * m) } -> std::same_as<M>;
        // Semantic: f(s * m) == s * f(m)
    };

} // namespace dedekind::category
