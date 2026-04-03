/**
 * @file algebra:monoid.cppm
 * @partition :monoid
 * @brief Level 3.0a: The Additive and Multiplicative Monoids (ℕ).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * @section The_Identity_Axiom
 * « Wprowadzenie pojęcia kategorii pozwala na jednolite traktowanie
 *   różnych struktur matematycznych. »
 *  (The introduction of the concept of a category allows for a uniform
 *   treatment of various mathematical structures.)
 *  — Samuel Eilenberg, 'Algebraic Topology'
 *
 * @section Taxonomy_of_Identity
 * This partition grounds the abstract categorical Monoid into the
 * established arithmetic notation of Algebra. It reifies the
 * "Rules of Neutrality" for Addition (0) and Multiplication (1),
 * establishing the skeletal foundation for all higher-order
 * Ring and Field structures.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:monoid;

import dedekind.category;

namespace dedekind::algebra {

/**
 * @concept IsAdditiveMonoid
 * @brief Proposition: The species (T, +) forms a Monoid.
 */
export template <typename T>
concept IsAdditiveMonoid = dedekind::category::IsMonoid<T, std::plus<>>;

/**
 * @concept IsMultiplicativeMonoid
 * @brief Proposition: The species (T, *) forms a Monoid.
 */
export template <typename T>
concept IsMultiplicativeMonoid =
    dedekind::category::IsMonoid<T, std::multiplies<>>;

/** @section Atomic_Verification */
static_assert(IsAdditiveMonoid<int>, "Axiom Failure: (Z, +) must have a Zero.");
static_assert(IsMultiplicativeMonoid<int>,
              "Axiom Failure: (Z, *) must have a Unit.");

}  // namespace dedekind::algebra
