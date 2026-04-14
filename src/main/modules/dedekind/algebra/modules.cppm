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
#include <cstddef>     // for std::size_t
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
 * @struct OneDimensionalVector
 * @brief Reified carrier for a 1D module/vector-space coordinate.
 * @details This is the concrete baseline for issue #125. The coordinate lives
 *          in the scalar species S, with pointwise addition and scalar action.
 */
export template <typename S, typename Tag = void>
  requires std::equality_comparable<S>
struct OneDimensionalVector {
  using scalar_type = S;

  S x{};

  constexpr OneDimensionalVector() = default;
  constexpr explicit OneDimensionalVector(S value) : x(value) {}

  constexpr S coordinate() const noexcept { return x; }

  friend constexpr bool operator==(const OneDimensionalVector&,
                                   const OneDimensionalVector&) = default;

  friend constexpr OneDimensionalVector operator+(
      const OneDimensionalVector& a, const OneDimensionalVector& b) {
    return OneDimensionalVector(a.x + b.x);
  }

  friend constexpr OneDimensionalVector operator-(const OneDimensionalVector& a,
                                                  const OneDimensionalVector& b)
    requires requires(S s) { -s; }
  {
    return OneDimensionalVector(a.x - b.x);
  }

  friend constexpr OneDimensionalVector operator*(
      const S& s, const OneDimensionalVector& v) {
    return OneDimensionalVector(s * v.x);
  }

  friend constexpr OneDimensionalVector operator*(const OneDimensionalVector& v,
                                                  const S& s) {
    return OneDimensionalVector(v.x * s);
  }

  template <typename Op>
  static constexpr auto identity_v = []() {
    if constexpr (std::same_as<Op, std::plus<OneDimensionalVector>> ||
                  std::same_as<Op, std::plus<void>>) {
      return OneDimensionalVector{};
    }
  }();

  template <typename Op>
  static constexpr bool is_associative_v =
      std::same_as<Op, std::plus<OneDimensionalVector>> ||
      std::same_as<Op, std::plus<void>>;

  template <typename Op>
  static constexpr bool is_commutative_v =
      std::same_as<Op, std::plus<OneDimensionalVector>> ||
      std::same_as<Op, std::plus<void>>;
};

/** @brief Alias: reals seen as a canonical 1D vector carrier. */
export using RealLine = OneDimensionalVector<double>;

/**
 * @concept IsVectorSpaceLike
 * @brief Pragmatic vector-space check used by first reified vector carriers.
 * @details Uses field-like scalar operations directly and does not depend on
 *          the stronger IsField witness machinery.
 */
export template <typename V, typename F, typename Act = std::multiplies<>>
concept IsVectorSpaceLike = requires(F a, F b, V v) {
  { v + v } -> std::same_as<V>;
  { v - v } -> std::same_as<V>;
  { V{} } -> std::same_as<V>;
  { a + b } -> std::same_as<F>;
  { a - b } -> std::same_as<F>;
  { a * b } -> std::same_as<F>;
  { a / b } -> std::same_as<F>;
  { Act{}(a, v) } -> std::same_as<V>;
};

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

static_assert(IsVectorSpaceLike<RealLine, double>,
              "RealLine should satisfy the baseline 1D vector-space witness.");

}  // namespace dedekind::algebra

namespace dedekind::category {

template <typename S, typename Tag>
  requires requires(S s) { -s; }
inline constexpr dedekind::algebra::OneDimensionalVector<S, Tag> inverse(
    dedekind::algebra::OneDimensionalVector<S, Tag> v,
    std::plus<dedekind::algebra::OneDimensionalVector<S, Tag>>) {
  return dedekind::algebra::OneDimensionalVector<S, Tag>(-v.x);
}

}  // namespace dedekind::category
