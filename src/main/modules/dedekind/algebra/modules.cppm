/**
 * @file ontology:algebra.cppm
 * @partition :modules
 * @brief Level 3.2: The Linear Synthesis (Modules and Vector Spaces).
 *
 * @section Modules: The Geometry of Scaling
 * This partition defines the "Action" of a Ring on an Abelian Group.
 * In the Dedekind ontology, a Module is the bridge between 
 * Pure Algebra and Metric Geometry.
 */

export module dedekind.algebra:modules;

import dedekind.category; // For IsLinearAction
import :rings;            // For IsRing, IsSemiring
import :groups;           // For IsAbelianGroup

namespace dedekind::algebra {
using namespace dedekind::category;

/** 
 * @concept IsSemimodule 
 * @brief A commutative additive monoid participating in a Linear Action.
 */
export template <typename M, typename S>
concept IsSemimodule = 
    IsCommutativeMonoid<M, std::plus<M>> && 
    IsSemiring<S> && 
    IsLinearAction<S, M>;

/** 
 * @concept IsModule 
 * @brief An Abelian Group participating in a Linear Action by a Ring.
 */
export template <typename M, typename S>
concept IsModule = 
    IsAbelianGroup<M> && 
    IsRing<S> && 
    IsSemimodule<M, S>;

/** 
 * @concept IsVectorSpace 
 * @brief The Gold Standard: A Module where the Scalar is a Field.
 */
export template <typename V, typename F>
concept IsVectorSpace = 
    IsModule<V, F> && 
    IsField<F>;

} // namespace dedekind::algebra
