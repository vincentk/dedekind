/**
 * @file dedekind/algebra/magma.cppm
 * @partition :magma
 * @brief The Magma: base rung of the algebra tower (an algebraic set).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * @section magma__Where_The_Two_Roads_Meet
 * The @c category tower defines the axiomatic hierarchy
 * (@c category::IsMagma → @c IsMonoid → @c IsGroup → ... → @c IsField)
 * at the type level, without a @c std::regular carrier requirement.
 * This partition opens the parallel @b algebra tower: the same rungs,
 * strengthened by the universal-algebra closure tier @c IsAlgebra
 * (which pins @c std::regular<T> and strict closure), so that every
 * rung is a @b bona-fide algebraic set.  Magma is the base; the
 * @c :monoid, @c :group, @c :ring and @c :field partitions build the
 * remaining rungs on the same pattern.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:magma;

import :universal;  // IsAlgebra (universal-algebra closure tier; #517)
import dedekind.category;

namespace dedekind::algebra {
using namespace dedekind::category;

/**
 * @concept IsMagma
 * @brief Proposition: a set object @c X whose carrier @c X::Domain forms
 *        a magma --- the set-indexed lift of @c category::IsMagma.
 * @details The base rung of the algebra tower, as the set monad's image
 * of the type-indexed @c category::IsMagma: @c X is an ETCS set
 * (@c category::IsSet), its carrier @c X::Domain satisfies the axiomatic
 * @c category::IsMagma<X::Domain, Op>, and the algebra-layer closure tier
 * @c IsAlgebra pins @c std::regular + strict closure on the carrier.
 * (Once @c IsSet roots @c std::regular on its carrier, the @c IsAlgebra
 * regular clause here becomes redundant.)
 * @tparam X  The set object (@c category::IsSet).
 * @tparam Op The binary operation on @c X::Domain (defaults to @c std::plus).
 */
export template <typename X, typename Op = std::plus<typename X::Domain>>
concept IsMagma = IsAlgebraOnSet<X, Op> &&
                  dedekind::category::IsMagma<typename X::Domain, Op>;

/** @section magma__Formal_Verification */

// Seal the axiom substrate the lift rests on: unsigned int under + is the
// canonical magma carrier; signed int is not (overflow is UB, not total).
// The set-indexed IsMagma<X> itself is witnessed in the test suite (it needs
// a set carrier, which this upstream partition does not import).
static_assert(
    dedekind::category::IsMagma<unsigned int, std::plus<unsigned int>>,
    "unsigned int under + is the canonical magma carrier.");
static_assert(!dedekind::category::IsMagma<int, std::plus<int>>,
              "signed int under + is not a magma: overflow is UB (not total).");

}  // namespace dedekind::algebra
