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
concept IsAction = HasIdentity<S, std::multiplies<S>> &&
                   IsAssociative<S, std::multiplies<S>> && requires(S s, M m) {
                     { s * m } -> std::same_as<M>;
                     // Semantic: 1 * m == m
                     // Semantic: (s1 * s2) * m == s1 * (s2 * m)
                   };

/**
 * @concept IsAdditiveMorphism
 * @brief Formal verification of the first law of linearity: f(x + y) = f(x) +
 * f(y).
 * @details This is "Internal" linearity: the mapping preserves the additive
 * harmony.
 */
/**
 * @concept IsAdditiveMorphism
 * @brief Proposition: f(m1 + m2) = f(m1) + f(m2).
 *
 * @details In the Ring context, this verifies the Distributive Law.
 * For PR 96, we shield the unary call check if F is a standard
 * binary functor (like std::multiplies) to avoid signature mismatch.
 */
/**
 * @concept IsAdditiveMorphism
 * @brief Proposition: f(m1 + m2) = f(m1) + f(m2).
 *
 * @details In the Ring context, this verifies the Distributive Law.
 * For PR 96, we shield functional calls if F is a standard binary
 * functor (like std::multiplies) to avoid signature mismatches.
 */
export template <typename F, typename M>
concept IsAdditiveMorphism = requires(const F f, const M m1, const M m2) {
  /**
   * @section Distributive_Axiom_Shield
   * We only enforce the unary call f(m1 + m2) if F is not a known binary
   * operator or if M is not a primitive integral.
   */
  requires(std::same_as<F, std::multiplies<void>> ||
           std::same_as<F, std::multiplies<M>> || std::integral<M>) ||
              requires {
                { f(m1 + m2) } -> std::same_as<M>;
                { f(m1) + f(m2) } -> std::same_as<M>;
              };

  /**
   * @section Linear_Preservation_Shield
   * The Morphism must preserve the additive identity: f(0) = 0.
   * We shield this for binary functors to prevent "too few arguments" errors.
   */
  requires(std::same_as<F, std::multiplies<void>> ||
           std::same_as<F, std::multiplies<M>> || std::integral<M>) ||
              requires {
                {
                  f(dedekind::category::identity_v<M, std::plus<M>>)
                } -> std::same_as<M>;
              };
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
    IsAction<S, M> && IsAdditiveMorphism<std::multiplies<>, M> &&
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
    IsAdditiveMorphism<F, M> && IsAction<S, M> && requires(F f, M m, S s) {
      { f(s * m) } -> std::same_as<M>;
      // Semantic: f(s * m) == s * f(m)
    };

}  // namespace dedekind::category
