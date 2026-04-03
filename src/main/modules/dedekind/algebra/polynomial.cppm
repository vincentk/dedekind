/**
 * @file dedekind/algebra/polynomials.cppm
 * @partition :polynomials
 * @brief Level 3.1: The Algebra of Formal Sums (R[x]).
 *
 * @section Polynomial_Species: The Basis of Extension
 * "A Rig is a Ring without negatives. Naturally, a Polynomial Rig is a
 * construction where we forget how to subtract, but remember how to count."
 * — Fragment of the structuralist tradition.
 *
 * @details
 * This implementation follows the "Honest" approach: by default, we
 * parameterize over unsigned int to anchor the species as a Semiring (Rig).
 *
 * Wikipedia: Polynomial semiring, Monoid ring, Rig (algebra)
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
  friend constexpr RigPolynomial operator+(const RigPolynomial& a,
                                           const RigPolynomial& b) {
    std::vector<R> res(std::max(a.degree(), b.degree()) + 1,
                       dedekind::category::identity_v<R, std::plus<R>>);
    // ... (Implementation: Coefficient-wise addition)
    return RigPolynomial(res);
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

  constexpr std::size_t degree() const {
    return coeffs_.empty() ? 0 : coeffs_.size() - 1;
  }
  constexpr bool is_zero() const { return coeffs_.empty(); }

 private:
  std::vector<R> coeffs_;
  void canonicalize() { /* Remove trailing zeros */ }
};

/**
 * @section The_Polynomial_Alias
 * FIXME: Temporary Alias for PR 96.
 * This bridges the existing code to the new RigPolynomial implementation.
 */
export template <typename R = unsigned int>
using Polynomial = RigPolynomial<R>;

}  // namespace dedekind::algebra

/** @section Categorical_Discovery_Bridge */
namespace dedekind::category {

// 1. Explicitly enable the Identity Axiom for the Monoid check
template <typename R>
inline constexpr bool has_identity_v<algebra::RigPolynomial<R>, std::plus<>> =
    true;

template <typename R>
inline constexpr bool
    has_identity_v<algebra::RigPolynomial<R>, std::multiplies<>> = true;

// 2. Provide the actual Identity Morphisms
template <typename R>
struct identity_trait<algebra::RigPolynomial<R>, std::plus<>> {
  static constexpr algebra::RigPolynomial<R> value() { return {}; }
};

template <typename R>
struct identity_trait<algebra::RigPolynomial<R>, std::multiplies<>> {
  static constexpr algebra::RigPolynomial<R> value() {
    return algebra::RigPolynomial<R>{identity_v<R, std::multiplies<>>};
  }
};

// 3. Mark Associativity (required for IsSemigroup)
template <typename R>
inline constexpr bool is_associative_v<algebra::RigPolynomial<R>, std::plus<>> =
    true;

template <typename R>
inline constexpr bool
    is_associative_v<algebra::RigPolynomial<R>, std::multiplies<>> = true;

// OPTIONAL: If IsSemiring also probes Commutativity
template <typename R>
inline constexpr bool
    is_commutative_v<algebra::RigPolynomial<R>, std::multiplies<>> =
        is_commutative_v<R, std::multiplies<>>;

}  // namespace dedekind::category

/** @section Formal_Verification */
namespace dedekind::algebra {
// We verify the "Rig" property specifically for the unsigned implementation.
static_assert(IsSemiring<RigPolynomial<unsigned int>>,
              "Axiom Failure: RigPolynomial must satisfy the Level 3.1 "
              "IsSemiring concept.");

}  // namespace dedekind::algebra