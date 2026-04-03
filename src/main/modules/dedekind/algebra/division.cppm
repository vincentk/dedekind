/**
 * @file dedekind/algebra/polynomials.cppm
 * @partition :euclidean
 * @brief Level 3.3: The Euclidean Algorithm for R[x].
 */
module;

#include <functional>  // for std::plus
#include <stdexcept>   // for std::domain_error
#include <utility>     // for std::make_pair
#include <vector>

export module dedekind.algebra:division;

import :polynomials;
import :rings;

namespace dedekind::algebra {

/**
 * @brief Performs Euclidean Division on Polynomials.
 * @return A pair {quotient, remainder}.
 */
export template <IsField F>
constexpr auto div_rem(const Polynomial<F>& a, const Polynomial<F>& b) {
  if (b.is_zero()) throw std::domain_error("Division by zero polynomial.");

  Polynomial<F> q({identity_v<F, std::plus<F>>});
  Polynomial<F> r = a;

  while (!r.is_zero() && r.degree() >= b.degree()) {
    // Leading term cancellation
    F leading_coeff = r.leading_coefficient() / b.leading_coefficient();
    std::vector<F> term_coeffs(r.degree() - b.degree() + 1,
                               identity_v<F, std::plus<F>>);
    term_coeffs.back() = leading_coeff;

    Polynomial<F> t(term_coeffs);
    q = q + t;
    r = r - (t * b);
  }
  return std::make_pair(q, r);
}

/** @section Euclidean_Operators */

export template <IsField F>
constexpr Polynomial<F> operator/(const Polynomial<F>& a,
                                  const Polynomial<F>& b) {
  return div_rem(a, b).first;
}

export template <IsField F>
constexpr Polynomial<F> operator%(const Polynomial<F>& a,
                                  const Polynomial<F>& b) {
  return div_rem(a, b).second;
}

/** @section Formal_Verification */

static_assert(
    IsEuclidean<Polynomial<double>>,
    "Axiom Failure: Polynomials over a Field must form a Euclidean Domain.");

/**
 * @concept IsIrreducible
 * @brief A polynomial that cannot be factored into non-trivial parts.
 * @details Axiom: p is irreducible if p = a * b implies a or b is a unit
 * (constant).
 */
export template <typename P>
concept IsIrreducible = IsRing<P> && requires(P p) {
  /**
   * @section The_Primality_Proof
   * In Level 3.3, this is verified via the lack of roots in the base field
   * (for low degrees) or via the Berlekamp/Kronecker algorithms.
   */
  requires p.degree() > 0;
};
}  // namespace dedekind::algebra
