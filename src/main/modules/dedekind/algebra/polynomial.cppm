/**
 * @file dedekind/algebra/polynomial.cppm
 * @partition :polynomial
 * @brief Level 3.1: The Algebra of Formal Sums (R[x]).

 *
 * @details
 * This implementation follows the "Honest" approach: by default, we
 * parameterize over unsigned int to anchor the species as a Semiring (Rig).
 *
 * Wikipedia: Polynomial semiring, Monoid ring, Rig (algebra)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "A Rig is a Ring without negatives. Naturally, a Polynomial Rig is a
 * construction where we forget how to subtract, but remember how to count."
 * — Fragment of the structuralist tradition.
 */
module;

#include <algorithm>   // for std::max
#include <compare>     // for std::strong_ordering
#include <concepts>    // for std::integral
#include <functional>  // for std::plus, std::multiplies
#include <vector>

export module dedekind.algebra:polynomial;

import dedekind.category;
import :ring;

namespace dedekind::algebra {

using namespace dedekind::category;

/**
 * @class RigPolynomial
 * @brief Represents the Semiring R[x] over a coefficient Semiring R.
 *
 * @tparam R The coefficient species. Defaults to unsigned int to formally
 *           enforce the "non-negative" nature of a Rig.
 */
export template <typename R = unsigned int>
  requires std::equality_comparable<R>
class RigPolynomial {
 public:
  using coefficient_type = R;
  using value_type = R;
  using machine_type = std::vector<R>;

  /**
   * @section Inversion_Axiom (The Rig Property)
   * FIXME: RigPolynomial is NOT invertible by definition.
   * In the ETCS refactor, we will introduce a 'Polynomial' elevation
   * that specializes this to true when R satisfies IsAbelianGroup.
   */
  template <typename Op>
  static constexpr bool is_invertible_v = false;

  /** @section Identity_Discovery */
  template <typename Op>
  static constexpr auto identity_v = []() {
    if constexpr (std::is_same_v<Op, std::plus<RigPolynomial>> ||
                  std::is_same_v<Op, std::plus<void>>) {
      return RigPolynomial{};
    } else if constexpr (std::is_same_v<Op, std::multiplies<RigPolynomial>> ||
                         std::is_same_v<Op, std::multiplies<void>>) {
      return RigPolynomial{R{1}};
    }
  }();

  /** @section Constants */
  static constexpr auto zero() { return RigPolynomial{}; }
  static constexpr auto one() { return RigPolynomial{R{1}}; }

  /** @section Construction */
  constexpr RigPolynomial() = default;
  constexpr explicit RigPolynomial(R scalar) : coeffs_{scalar} {
    canonicalize();
  }
  constexpr explicit RigPolynomial(std::vector<R> coeffs)
      : coeffs_(std::move(coeffs)) {
    canonicalize();
  }

  /** @section Algebraic_Morphisms */
  friend constexpr bool operator==(const RigPolynomial& a,
                                   const RigPolynomial& b) = default;

  friend constexpr RigPolynomial operator+(const RigPolynomial& a,
                                           const RigPolynomial& b) {
    if (a.is_zero()) return b;
    if (b.is_zero()) return a;
    std::vector<R> res(std::max(a.coeffs_.size(), b.coeffs_.size()),
                       dedekind::category::identity_v<R, std::plus<R>>);
    for (std::size_t i = 0; i < a.coeffs_.size(); ++i)
      res[i] = res[i] + a.coeffs_[i];
    for (std::size_t i = 0; i < b.coeffs_.size(); ++i)
      res[i] = res[i] + b.coeffs_[i];
    return RigPolynomial(std::move(res));
  }

  /**
   * @brief Coefficient-wise subtraction: (a - b)_i = a_i - b_i.
   * @details Available only when the coefficient type R satisfies
   *          dedekind::category::is_invertible_v<R, std::plus<R>>, i.e. when
   *          additive inverses are provided by the category machinery.
   *          Not available for rig coefficients such as unsigned int.
   */
  friend constexpr RigPolynomial operator-(const RigPolynomial& a,
                                           const RigPolynomial& b)
    requires dedekind::category::is_invertible_v<R, std::plus<R>>
  {
    if (b.is_zero()) return a;
    std::vector<R> res(std::max(a.coeffs_.size(), b.coeffs_.size()),
                       dedekind::category::identity_v<R, std::plus<R>>);
    for (std::size_t i = 0; i < a.coeffs_.size(); ++i)
      res[i] = res[i] + a.coeffs_[i];
    for (std::size_t i = 0; i < b.coeffs_.size(); ++i)
      res[i] =
          res[i] + dedekind::category::inverse_v<R, std::plus<R>>(b.coeffs_[i]);
    return RigPolynomial(std::move(res));
  }

  /** @brief Cauchy Product: (a * b)_n = Σ (a_i * b_{n-i}) */
  friend constexpr RigPolynomial operator*(const RigPolynomial& a,
                                           const RigPolynomial& b) {
    if (a.is_zero() || b.is_zero()) return RigPolynomial{};

    // The degree of the product is the sum of the degrees
    std::vector<R> res(a.degree() + b.degree() + 1,
                       dedekind::category::identity_v<R, std::plus<R>>);

    for (std::size_t i = 0; i < a.coeffs_.size(); ++i) {
      for (std::size_t j = 0; j < b.coeffs_.size(); ++j) {
        // We use the underlying Rig operations of R
        res[i + j] = res[i + j] + (a.coeffs_[i] * b.coeffs_[j]);
      }
    }
    return RigPolynomial(std::move(res));
  }

  /**
   * @brief Formal derivative: d/dx Σ aᵢ xⁱ = Σ_{i≥1} i·aᵢ xⁱ⁻¹.
   * @details The coefficient i·aᵢ is computed via binary repeated doubling
   *          (O(log i) additions per term, O(n log n) total), which is valid
   *          over any semiring (no subtraction or division required).
   */
  constexpr RigPolynomial derive() const {
    if (coeffs_.size() <= 1) return RigPolynomial{};
    const R zero = dedekind::category::identity_v<R, std::plus<R>>;
    std::vector<R> res(coeffs_.size() - 1, zero);
    for (std::size_t i = 1; i < coeffs_.size(); ++i) {
      // Compute i * a_i via repeated doubling: O(log i) additions.
      R coeff = zero;
      R term = coeffs_[i];
      for (std::size_t n = i; n > 0; n >>= 1) {
        if (n & 1) coeff = coeff + term;
        term = term + term;
      }
      res[i - 1] = coeff;
    }
    return RigPolynomial(std::move(res));
  }

  constexpr std::size_t degree() const {
    // FIXME #286: Laurent exponents. Currently only non-negative (Rig
    // discipline). Support negative exponents via separate Laurent<R> type or
    // exponent field.
    return coeffs_.empty() ? 0 : coeffs_.size() - 1;
  }
  constexpr bool is_zero() const { return coeffs_.empty(); }

  /** @brief Read-only access to the coefficient vector (constant-first). */
  constexpr const std::vector<R>& coeffs() const noexcept { return coeffs_; }

 private:
  std::vector<R> coeffs_;

  constexpr void canonicalize() {
    const R zero = dedekind::category::identity_v<R, std::plus<R>>;
    while (!coeffs_.empty() && coeffs_.back() == zero) coeffs_.pop_back();
  }
};

/**
 * @section The_Polynomial_Alias
 * FIXME: Temporary Alias for PR 96.
 * This bridges the existing code to the new RigPolynomial implementation.
 */
export template <typename R = unsigned int>
using Polynomial = RigPolynomial<R>;

}  // namespace dedekind::algebra

// FIXME: this is really not pretty.
namespace dedekind::category {

template <typename R>
using Poly = algebra::RigPolynomial<R>;

/** @section Symmetry_Axioms */
template <typename R>
inline constexpr bool is_associative_v<Poly<R>, std::plus<>> = true;
template <typename R>
inline constexpr bool is_associative_v<Poly<R>, std::plus<Poly<R>>> = true;

template <typename R>
inline constexpr bool is_associative_v<Poly<R>, std::multiplies<>> = true;
template <typename R>
inline constexpr bool is_associative_v<Poly<R>, std::multiplies<Poly<R>>> =
    true;

// Polynomial addition is commutative whenever its coefficient ring is.
template <typename R>
  requires is_commutative_v<R, std::plus<R>>
inline constexpr bool is_commutative_v<Poly<R>, std::plus<>> = true;
template <typename R>
  requires is_commutative_v<R, std::plus<R>>
inline constexpr bool is_commutative_v<Poly<R>, std::plus<Poly<R>>> = true;

// Polynomial multiplication is commutative whenever the coefficient ring is.
template <typename R>
  requires is_commutative_v<R, std::multiplies<R>>
inline constexpr bool is_commutative_v<Poly<R>, std::multiplies<>> = true;
template <typename R>
  requires is_commutative_v<R, std::multiplies<R>>
inline constexpr bool is_commutative_v<Poly<R>, std::multiplies<Poly<R>>> =
    true;

// Identity elements: the zero polynomial for addition, the unit polynomial
// for multiplication. These lift the class-level identity_v member into the
// global category trait, enabling IsMonoid / IsSemiring concept checks.
template <typename R>
inline constexpr Poly<R> identity_v<Poly<R>, std::plus<>> = Poly<R>{};
template <typename R>
inline constexpr Poly<R> identity_v<Poly<R>, std::plus<Poly<R>>> = Poly<R>{};
template <typename R>
inline constexpr Poly<R> identity_v<Poly<R>, std::multiplies<>> = Poly<R>{R{1}};
template <typename R>
inline constexpr Poly<R> identity_v<Poly<R>, std::multiplies<Poly<R>>> =
    Poly<R>{R{1}};

}  // namespace dedekind::category

namespace dedekind::algebra {

/**
 * @section Formal_Verification
 *
 * @note Algebraic inheritance: RigPolynomial<R> inherits the algebraic
 * structure of its coefficient type R:
 *
 *  - R is a Monoid  ⟹  Poly<R> is a Monoid  (under Cauchy product + addition)
 *  - R is a Ring    ⟹  Poly<R> is a Ring     (subtraction lifts when R has
 * additive inverses)
 *  - R is a Field   ⟹  Poly<R> is a Euclidean Domain (NOT a Field: X has no
 * inverse)
 *
 * The last point is classical: K[x] over a field K is a principal ideal domain
 * with Euclidean function deg(f), but it is not a field because the
 * indeterminate x is not invertible. Polynomial division (with remainder) gives
 * K[x] the IsEuclidean structure (deg(f) < deg(g) after remainder), enabling
 * GCD.
 *
 * @note The stronger categorical proofs IsSemiring<Poly<uint>> and
 * IsRing<Poly<int>> are architecturally blocked: those concepts require
 * IsMonoid which requires IsTotal (IsPeriodic || IsIdempotent). Polynomial
 * addition is neither periodic (unbounded vector growth) nor idempotent.
 * The identity_v and is_associative_v / is_commutative_v registrations above
 * are the correct categorical anchors at the partial-algebra stratum.
 */

}  // namespace dedekind::algebra
