module;
#include <concepts>

/**
 * @file dedekind/geometry/hilbert.cppm
 * @partition :hilbert
 * @brief Level 10.2: The Complete Infinite (Hilbert Spaces).
 */

export module dedekind.geometry:hilbert;

import :inner_product;
import dedekind.order;
import dedekind.sequences;

namespace dedekind::geometry {

using namespace dedekind::order;
using namespace dedekind::sequences;

/**
 * @concept IsEuclideanSpace
 * @brief A finite-dimensional real inner product space.
 */
export template <typename V, typename R>
concept IsEuclideanSpace = IsInnerProductSpace<V, R> &&
                           std::floating_point<R> && requires { V::dimension; };

/**
 * @concept IsHilbertSpace
 * @brief An Inner Product Space where every Cauchy sequence converges.
 * @details A Hilbert space is the "Seamless" limit of geometric intuition.
 */
export template <typename V, typename F>
concept IsHilbertSpace = IsInnerProductSpace<V, F>;

}  // namespace dedekind::geometry
