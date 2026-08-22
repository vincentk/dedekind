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
 * @brief Proposition: the species (T, Op) forms a magma, as an
 *        algebraic set.
 * @details The axiomatic @c category::IsMagma<T, Op> (closed and total
 * binary operation) strengthened with the algebra-layer closure tier
 * @c IsAlgebra<T, Op> --- which pins @c std::regular<T> and strict
 * closure, matching Burris--Sankappanavar's value-semantics carrier
 * convention.  The upstream @c category::IsMagma is intentionally
 * lighter (no @c std::regular requirement); this algebra-layer rung
 * strengthens it, and the whole algebra tower builds on the same
 * pattern.
 * @tparam T The carrier type (@c std::regular).
 * @tparam Op The binary operation witness.
 */
export template <typename T, typename Op>
concept IsMagma = dedekind::category::IsMagma<T, Op> && IsAlgebra<T, Op>;

/** @section magma__Formal_Verification */

// Seal: the base rung fires on the canonical machine carrier, and
// rejects the hazard carrier.  Signed @c int under @c + is not a magma
// (signed overflow is UB, so the operation is not total); the algebra
// layer additionally demands @c std::regular closure, which @c unsigned
// int supplies via modular wrap.
static_assert(IsMagma<unsigned int, std::plus<unsigned int>>,
              "unsigned int under + is the canonical algebraic-set magma.");
static_assert(!IsMagma<int, std::plus<int>>,
              "signed int under + is not a magma: overflow is UB (not total).");

}  // namespace dedekind::algebra
