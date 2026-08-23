/**
 * @file dedekind/algebra/monoid.cppm
 * @partition :monoid
 * @brief The Additive and Multiplicative Monoids (ℕ).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.

 *
 * @section monoid__Taxonomy_of_Identity
 * This partition grounds the abstract categorical Monoid into the
 * established arithmetic notation of Algebra. It reifies the
 * "Rules of Neutrality" for Addition (0) and Multiplication (1),
 * establishing the skeletal foundation for all higher-order
 * Ring and Field structures.
 *
 * @note « Wprowadzenie pojęcia kategorii pozwala na jednolite traktowanie
 *   różnych struktur matematycznych. »
 *  (The introduction of the concept of a category allows for a uniform
 *   treatment of various mathematical structures.)
 *  — Samuel Eilenberg, 'Algebraic Topology'
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:monoid;

import :universal;  // IsAlgebra (universal-algebra closure tier; #517)
import dedekind.category;

namespace dedekind::algebra {
using namespace dedekind::category;

/**
 * @concept IsMonoid
 * @brief Proposition: a set object @c X whose carrier @c X::Domain forms
 *        a monoid --- the set-indexed lift of @c category::IsMonoid.
 * @details @c X is an ETCS set, its carrier @c X::Domain satisfies the
 * axiomatic @c category::IsMonoid<X::Domain, Op>, and @c IsAlgebra pins
 * @c std::regular + strict closure on the carrier.  The Op-defaulted
 * @c IsAdditiveMonoid / @c IsMultiplicativeMonoid siblings name the
 * canonical additive / multiplicative witnesses.
 * @tparam X  The set object (@c category::IsSet).
 * @tparam Op The binary operation on @c X::Domain (defaults to @c std::plus).
 */
export template <typename X, typename Op = std::plus<typename X::Domain>>
concept IsMonoid = IsAlgebraOnSet<X, Op> &&
                   dedekind::category::IsMonoid<typename X::Domain, Op>;

/**
 * @concept IsAdditiveMonoid
 * @brief Proposition: a set object @c X whose carrier is a monoid under +.
 * @details The @c std::plus-defaulted additive witness of the set-indexed
 * @c IsMonoid.
 * @tparam X   The set object.
 * @tparam Add The additive operation (defaults to `std::plus<X::Domain>`).
 */
export template <typename X, typename Add = std::plus<typename X::Domain>>
concept IsAdditiveMonoid = IsMonoid<X, Add>;

/**
 * @concept IsMultiplicativeMonoid
 * @brief Proposition: a set object @c X whose carrier is a monoid under *.
 * @details Sibling of @c IsAdditiveMonoid; the @c std::multiplies-defaulted
 * multiplicative witness of the set-indexed @c IsMonoid (#517).
 * @tparam X    The set object.
 * @tparam Mult The multiplicative operation (defaults to
 *              `std::multiplies<X::Domain>`).
 */
export template <typename X, typename Mult = std::multiplies<typename X::Domain>>
concept IsMultiplicativeMonoid = IsMonoid<X, Mult>;

/** @section monoid__Formal_Verification */

// During experimental reintegration, full monoid witnesses for int
// are deferred pending structure-proof registration in category module.

// static_assert(IsAdditiveMonoid<int>, "Axiom Failure: (Z, +) must have a
// Zero."); static_assert(IsMultiplicativeMonoid<int>,
//               "Axiom Failure: (Z, *) must have a Unit.");
}  // namespace dedekind::algebra
