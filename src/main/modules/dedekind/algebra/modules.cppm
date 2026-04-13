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
 * @tparam M The module carrier.
 * @tparam S The scalar carrier.
 * @tparam AddM Additive law on M (defaults to `std::plus<M>`).
 * @tparam AddS Additive law on S (defaults to `std::plus<S>`).
 * @tparam MultS Multiplicative law on S (defaults to `std::multiplies<S>`).
 * @tparam Act External action witness S x M -> M
 * (defaults to `std::multiplies<>`).
 */
export template <typename M, typename S, typename AddM = std::plus<M>,
                 typename AddS = std::plus<S>,
                 typename MultS = std::multiplies<S>,
                 typename Act = std::multiplies<>>
concept IsSemimodule =
    IsCommutativeMonoid<M, AddM> && IsSemiring<S, AddS, MultS> &&
    IsLinearAction<S, M, Act, AddS>;

/**
 * @concept IsModule
 * @brief An Abelian Group participating in a Linear Action by a Ring.
 * @tparam M The module carrier.
 * @tparam S The scalar carrier.
 * @tparam AddM Additive law on M (defaults to `std::plus<M>`).
 * @tparam AddS Additive law on S (defaults to `std::plus<S>`).
 * @tparam MultS Multiplicative law on S (defaults to `std::multiplies<S>`).
 * @tparam Act External action witness S x M -> M
 * (defaults to `std::multiplies<>`).
 */
export template <typename M, typename S, typename AddM = std::plus<M>,
                 typename AddS = std::plus<S>,
                 typename MultS = std::multiplies<S>,
                 typename Act = std::multiplies<>>
concept IsModule = IsAdditiveGroup<M, AddM> && IsRing<S, AddS, MultS> &&
                   IsSemimodule<M, S, AddM, AddS, MultS, Act>;

/**
 * @concept IsVectorSpace
 * @brief The Gold Standard: A Module where the Scalar is a Field.
 * @tparam V The vector carrier.
 * @tparam F The scalar field carrier.
 * @tparam AddV Additive law on V (defaults to `std::plus<V>`).
 * @tparam AddF Additive law on F (defaults to `std::plus<F>`).
 * @tparam MultF Multiplicative law on F (defaults to `std::multiplies<F>`).
 * @tparam Act External action witness F x V -> V
 * (defaults to `std::multiplies<>`).
 */
export template <typename V, typename F, typename AddV = std::plus<V>,
                 typename AddF = std::plus<F>,
                 typename MultF = std::multiplies<F>,
                 typename Act = std::multiplies<>>
concept IsVectorSpace =
    IsModule<V, F, AddV, AddF, MultF, Act> && IsField<F, AddF, MultF>;

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
    M result = dedekind::category::identity_v<M, std::plus<M>>;
    for (auto it = p.coeffs().rbegin(); it != p.coeffs().rend(); ++it) {
      result = (result * x) + (*it);
    }
    return result;
  }
};

}  // namespace dedekind::algebra
