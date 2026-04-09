/**
 * @file algebra:modules.cppm
 * @partition :modules
 * @brief Level 3.6: The Linear Synthesis (Modules and Vector Spaces).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section The_Noetherian_Influence
 * „Die Arithmetik, Algebra und Analysis sind nur eine einzige Wissenschaft,
 *  die Wissenschaft der Zahlen.“
 *  (Arithmetic, algebra, and analysis are but a single science,
 *   the science of numbers.)
 *  — Emmy Noether
 *
 * @section Taxonomy_of_Influence
 * A Module is the reification of 'Structural Action'. It defines how
 * a Ring (the Scalar) acts upon an Additive Group (the Vector),
 * creating the 'Linear Action' that serves as the engine for all
 * Vector Spaces and Metric Geometry in the Dedekind topos.
 */
module;

#include <compare>     // for std::strong_ordering
#include <concepts>    // for std::integral, std::floating_point
#include <functional>  // for std::plus, std::multiplies

export module dedekind.algebra:modules;

import dedekind.category;
import :field;
import :group;
import :polynomial;

namespace dedekind::algebra {
using namespace dedekind::category;

/**
 * @concept IsSemimodule
 * @brief A commutative additive monoid participating in a Linear Action.
 */
export template <typename M, typename S>
concept IsSemimodule = IsCommutativeMonoid<M, std::plus<M>> && IsSemiring<S> &&
                       IsLinearAction<S, M>;

/**
 * @concept IsModule
 * @brief An Abelian Group participating in a Linear Action by a Ring.
 */
export template <typename M, typename S>
concept IsModule = IsAdditiveGroup<M> && IsRing<S> && IsSemimodule<M, S>;

/**
 * @concept IsVectorSpace
 * @brief The Gold Standard: A Module where the Scalar is a Field.
 */
export template <typename V, typename F>
concept IsVectorSpace = IsModule<V, F> && IsField<F>;

/**
 * @struct PolynomialOperator
 * @brief The "Enhanced" Polynomial: A Formal Sum reified as a Morphic Action.
 *
 * @details
 * In the Dedekind hierarchy, this transforms the static 'Formal Polynomial'
 * into a dynamic 'Operator' on a Module M.
 *
 * @tparam R The Scalar Ring (Coefficients).
 * @tparam M The Target Module (The Space).
 */
export template <typename R, typename M>
  requires IsModule<M, R>
struct PolynomialOperator {
  RigPolynomial<R> p;

  /** @section The_Action_Axiom */
  constexpr M operator()(const M& x) const {
    // Horner's Method implementation lives here,
    // where IsModule is fully visible.
    M result = dedekind::category::identity_v<M, std::plus<>>;
    for (auto it = p.coeffs().rbegin(); it != p.coeffs().rend(); ++it) {
      result = (result * x) + (*it);
    }
    return result;
  }
};

}  // namespace dedekind::algebra
