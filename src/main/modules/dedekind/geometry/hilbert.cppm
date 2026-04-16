/**
 * @file dedekind/geometry/hilbert.cppm
 * @partition :hilbert
 * @brief Module interface in the dedekind hierarchy.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "In dedekind.geometry:hilbert, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
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

}  // namespace dedekind::geometry
