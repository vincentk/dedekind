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
 * @brief Proposition: the species (T, Op) forms a monoid, as an
 *        algebraic set.
 * @details The axiomatic @c category::IsMonoid<T, Op> strengthened
 * with the algebra-layer closure tier @c IsAlgebra<T, Op> --- which
 * adds @c std::regular<T> and strict closure, matching
 * Burris--Sankappanavar's value-semantics carrier convention.  The
 * upstream @c category::IsMonoid is intentionally lighter (no @c
 * std::regular requirement); this algebra-layer rung strengthens it.
 * The Op-defaulted @c IsAdditiveMonoid / @c IsMultiplicativeMonoid
 * siblings below name the canonical additive / multiplicative
 * witnesses.
 * @tparam T The carrier type (@c std::regular).
 * @tparam Op The binary operation witness.
 */
export template <typename T, typename Op>
concept IsMonoid = dedekind::category::IsMonoid<T, Op> && IsAlgebra<T, Op>;

/**
 * @concept IsAdditiveMonoid
 * @brief Proposition: The species (T, +) forms a Monoid.
 * @details The @c std::plus<T>-defaulted additive witness of the
 * algebra-layer @c IsMonoid; the closure tier @c IsAlgebra travels in
 * via @c IsMonoid.
 * @tparam T The carrier type (@c std::regular).
 * @tparam Add The additive operation witness (defaults to `std::plus<T>`).
 */
export template <typename T, typename Add = std::plus<T>>
concept IsAdditiveMonoid = IsMonoid<T, Add>;

/**
 * @concept IsMultiplicativeMonoid
 * @brief Proposition: The species (T, *) forms a Monoid.
 * @details Operator is configurable; default witness is
 * `std::multiplies<T>` (the canonical `*`) to align with
 * category:total.  Sibling of @c IsAdditiveMonoid; same
 * @c IsAlgebra<T, Mult> strengthening travelling in via @c IsMonoid
 * (#517).
 * @tparam T The carrier type (@c std::regular).
 * @tparam Mult The multiplicative operation witness (defaults to
 * `std::multiplies<T>`).
 */
export template <typename T, typename Mult = std::multiplies<T>>
concept IsMultiplicativeMonoid = IsMonoid<T, Mult>;

/** @section monoid__Formal_Verification */

// During experimental reintegration, full monoid witnesses for int
// are deferred pending structure-proof registration in category module.

// static_assert(IsAdditiveMonoid<int>, "Axiom Failure: (Z, +) must have a
// Zero."); static_assert(IsMultiplicativeMonoid<int>,
//               "Axiom Failure: (Z, *) must have a Unit.");
}  // namespace dedekind::algebra
