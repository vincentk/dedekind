/**
 * @file dedekind/geometry/hilbert.cppm
 * @partition :hilbert
 * @brief Module interface in the dedekind hierarchy.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Symmetry is a vast subject, significant in art and nature.
 * Mathematics lies at its root."
 *       -- Hermann Weyl, Symmetry (1952)
 */

module;
#include <concepts>

/**
 * @file dedekind/geometry/hilbert.cppm
 * @partition :hilbert
 * @brief Level 10.2: The Complete Infinite (Hilbert Spaces).
 */

export module dedekind.geometry:hilbert;

import :inner_product;
import dedekind.algebra;
import dedekind.order;
import dedekind.sequences;
import dedekind.morphologies;

namespace dedekind::geometry {

using namespace dedekind::order;
using namespace dedekind::sequences;

using namespace dedekind::morphologies;

/**
 * @concept IsFiniteDimensionalInnerProductSpace
 * @brief A finite-dimensional real inner-product space.
 */
export template <typename V, typename R>
concept IsFiniteDimensionalInnerProductSpace =
    IsInnerProductSpace<V, R> && dedekind::algebra::IsFloatingScalar<R> &&
    requires { V::dimension; };

/**
 * @concept IsHilbertSpace
 * @brief An Inner Product Space where every Cauchy sequence converges.
 * @details A Hilbert space is the "Seamless" limit of geometric intuition.
 *          Completeness of the scalar field is certified via
 *          IsDedekindCompleteField<F> (from dedekind.morphologies:archimedean),
 *          which requires the scalar to be an Archimedean ordered field.
 *          This links the metric-space completeness of H to the order-theoretic
 *          completeness (IsDedekindComplete) of its scalar domain.
 * @see IsDedekindCompleteField
 * @see IsDedekindComplete (dedekind.order)
 */
export template <typename V, typename F>
concept IsHilbertSpace =
    IsInnerProductSpace<V, F> && IsDedekindCompleteField<F>;

/** @section Formal_Verification */

// Vector<double, N> with the standard Euclidean inner product is the
// canonical finite-dimensional inner-product space ℝᴺ.  The full
// Hilbert-space concept additionally requires Dedekind-completeness
// of the scalar field, which `double` does not satisfy categorically
// (it is the IEEE proxy for ℝ, not ℝ itself); the witness for
// IsHilbertSpace lands once an exact-real carrier ships.
static_assert(IsFiniteDimensionalInnerProductSpace<Vector<double, 3>, double>,
              "Vector<double, 3> must satisfy "
              "IsFiniteDimensionalInnerProductSpace over double.");

}  // namespace dedekind::geometry
