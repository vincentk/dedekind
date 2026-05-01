/**
 * @file dedekind/algebra/polynomial.cppm
 * @partition :polynomial
 * @brief The Algebra of Formal Sums (R[x]).

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
#include <stdexcept>   // for std::domain_error (div_rem)
#include <utility>     // for std::make_pair (div_rem)
#include <vector>

export module dedekind.algebra:polynomial;

import dedekind.category;
import dedekind.sets;
import :field;
import :free;  // free_algebra_base trait declaration (R[x] is the free
               // associative R-algebra on one generator; #498/#499)
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
   * @section polynomial__Inversion_Axiom
   * FIXME: RigPolynomial is NOT invertible by definition.
   * In the ETCS refactor, we will introduce a 'Polynomial' elevation
   * that specializes this to true when R satisfies IsAbelianGroup.
   */
  template <typename Op>
  static constexpr bool is_invertible_v = false;

  /** @section polynomial__Identity_Discovery */
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

  /** @section polynomial__Constants */
  static constexpr auto zero() { return RigPolynomial{}; }
  static constexpr auto one() { return RigPolynomial{R{1}}; }

  /** @section polynomial__Construction */
  constexpr RigPolynomial() = default;
  constexpr explicit RigPolynomial(R scalar) : coeffs_{scalar} {
    canonicalize();
  }
  constexpr explicit RigPolynomial(std::vector<R> coeffs)
      : coeffs_(std::move(coeffs)) {
    canonicalize();
  }

  /** @section polynomial__Algebraic_Morphisms */
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

  /**
   * @brief Unary additive inverse: @c -p coefficient-wise.
   *
   * @details Available only when the coefficient ring @c R provides
   * additive inverses (@c is_invertible_v<R, std::plus<R>>).  The
   * bundled @c algebra::IsRing uses the functor-parametric
   * @c IsAlgebra (which checks the functors close on the
   * carrier, not the literal operators), so it does @b not by itself
   * require unary @c -.  This unary @c - is required by the
   * @b literal shape concept @c HasRingOperators<RigPolynomial<R>>
   * (which does require @c -a as a same-type expression) and hence
   * by the seal @c IsArithmeticRing<RigPolynomial<R>>.  Witnessing
   * @c HasRingOperators<PolyUInt> / @c IsArithmeticRing<PolyUInt> in
   * the formal-verification block below relies on this operator
   * being available.  Added under #393 alongside the shape concept
   * introduction; the prior operational alias @c IsRingLike was
   * collapsed into @c HasRingOperators under the #394 retire-Like
   * sweep, so the literal-shape concept is now the sole consumer
   * of this operator.
   */
  friend constexpr RigPolynomial operator-(const RigPolynomial& a)
    requires dedekind::category::is_invertible_v<R, std::plus<R>>
  {
    if (a.is_zero()) return a;
    std::vector<R> res;
    res.reserve(a.coeffs_.size());
    for (const R& c : a.coeffs_)
      res.push_back(dedekind::category::inverse_v<R, std::plus<R>>(c));
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
 * @section polynomial__The_Polynomial_Alias
 * FIXME: Temporary Alias for PR 96.
 * This bridges the existing code to the new RigPolynomial implementation.
 */
export template <typename R = unsigned int>
using Polynomial = RigPolynomial<R>;

/**
 * @brief Generic Polynomial Euclidean Division over a field.
 * @details Template-generic on the polynomial type so the signature can
 *          live in the `:polynomial` partition without committing to a
 *          specific concrete polynomial class.  The coefficient type
 *          @c Coeff must be a field (every non-zero element has a
 *          multiplicative inverse, as required by polynomial long
 *          division on the leading coefficients).  The @c requires
 *          clause also pins down the structural surface the algorithm
 *          uses on @c Poly (zero test, degree, leading coefficient,
 *          construction from a single @c Coeff), so misuse produces a
 *          local diagnostic rather than a cryptic template error deep
 *          inside the body.
 *
 * @throws std::logic_error until the algorithm body is filled in.
 *
 * FIXME: The algorithm body is still a stub --- it would need to
 * subtract @c (leading_coeff * b << shift) from the running remainder
 * and accumulate the term onto @c q, which requires @c Poly to expose
 * a vector-scaling / shifting surface that @c RigPolynomial does not
 * yet carry.  Until that lands (or the 'Polynomial elevation' FIXME on
 * line 56 is resolved), calling @c div_rem throws @c std::logic_error
 * rather than returning a silently-invalid @c (q, r) pair.  The
 * earlier test file @c algebra/field_test.cppm was a @c .cppm (never
 * matched by the build's @c *_test.cpp glob), so no real test
 * coverage exists yet.
 */
export template <typename Poly, typename Coeff>
  requires IsField<Coeff> && requires(Poly p, Coeff c) {
    { p.is_zero() } -> std::convertible_to<bool>;
    { p.degree() };
    { p.leading_coefficient() } -> std::same_as<Coeff>;
    Poly{c};
  }
constexpr std::pair<Poly, Poly> div_rem(const Poly& a, const Poly& b) {
  if (b.is_zero()) throw std::domain_error("Division by zero.");

  // Fail fast rather than return a silently-invalid (q, r) pair.  The
  // full polynomial long-division loop would compute the leading-
  // coefficient quotient, subtract @c (c * b << shift) from the
  // running remainder, and accumulate the term onto @c q; but @c Poly
  // does not yet expose the vector-scaling / shifting surface that
  // step requires (see FIXME).  Until it does, every non-trivial
  // input would yield an incorrect result that type-checks, so we
  // throw here to document the incompleteness at the call site.
  // Remove the throw and fill in the loop once the Poly surface lands.
  (void)a;
  throw std::logic_error(
      "div_rem: polynomial long division not yet implemented "
      "(see FIXME; requires Poly vector-scaling / shifting surface).");
}

}  // namespace dedekind::algebra

// FIXME: this is really not pretty.
namespace dedekind::category {

template <typename R>
using Poly = algebra::RigPolynomial<R>;

/** @section polynomial__Symmetry_Axioms */
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

// Totality: Poly<R> inherits periodicity coefficient-wise from R.
// All polynomial arithmetic (addition, multiplication) is applied term-by-term
// on the coefficient vector; if R's operation wraps (periodic), so does
// Poly<R>.
template <typename R>
  requires is_periodic_v<R, std::plus<R>>
struct is_periodic<Poly<R>, std::plus<Poly<R>>> : std::true_type {};

template <typename R>
  requires is_periodic_v<R, std::multiplies<R>>
struct is_periodic<Poly<R>, std::multiplies<Poly<R>>> : std::true_type {};

// Distributivity: Cauchy multiplication distributes over coefficient-wise
// addition whenever R distributes — the standard polynomial ring argument.
template <typename R>
  requires is_distributive_v<R, std::multiplies<R>, std::plus<R>>
inline constexpr bool
    is_distributive_v<Poly<R>, std::multiplies<Poly<R>>, std::plus<Poly<R>>> =
        true;

// Invertibility: coefficient-wise additive inverses lift when R has them.
// This also enables the constrained operator- in RigPolynomial.
template <typename R>
  requires is_invertible_v<R, std::plus<R>>
inline constexpr bool is_invertible_v<Poly<R>, std::plus<Poly<R>>> = true;

}  // namespace dedekind::category

namespace dedekind::algebra {

/**
 * @section polynomial__Formal_Verification
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
 * Proof: RigPolynomial<unsigned int> is a total commutative ring.
 * unsigned int satisfies IsRing (wrapping arithmetic in Z/2^N Z), and the
 * lifted is_periodic / is_distributive / is_invertible registrations above
 * lift that certification to Poly<unsigned int>.
 *
 * Proof: RigPolynomial<ExtensionalCardinal<>> is a total commutative ring.
 * ExtensionalCardinal<> (extensional ℕ, two's-complement wrapping) satisfies
 * IsRing. Its periodicity and distributivity lift to the polynomial ring.
 */

using PolyUInt = RigPolynomial<unsigned int>;
static_assert(IsCommutativeRing<PolyUInt>,
              "RigPolynomial<unsigned int> must satisfy IsCommutativeRing "
              "(coefficient wrapping lifts to the polynomial level).");

// Strict literal-operator surface (#393): RigPolynomial<R>'s friend
// operator+, binary operator-, unary operator- (all gated on R's
// additive invertibility), and operator* return `RigPolynomial<R>`
// exactly.  When R is `unsigned int`, all three gates are open and the
// literal-operator surface closes strictly on the polynomial carrier.
static_assert(HasRingOperators<PolyUInt>,
              "RigPolynomial<unsigned int> has the literal ring-operator "
              "surface (+, -, unary -, *), all returning the polynomial "
              "carrier itself.");

// IsArithmeticRing seal: the strict ring proof + the literal-operator
// surface meet on RigPolynomial<unsigned int>.  Polynomial rings over
// unsigned int are reachable through plain C++ syntax, with results
// staying in the carrier.
static_assert(IsArithmeticRing<PolyUInt>,
              "RigPolynomial<unsigned int> is the canonical arithmetic "
              "ring — strict commutative-ring proof and literal +,-,* "
              "agree on the polynomial carrier.");

using PolyEC = RigPolynomial<dedekind::sets::ExtensionalCardinal<>>;
static_assert(
    IsCommutativeRing<PolyEC>,
    "RigPolynomial<ExtensionalCardinal<>> must satisfy IsCommutativeRing.");

}  // namespace dedekind::algebra

// ---------------------------------------------------------------------------
// Free-algebra registration for RigPolynomial<R> (#498/#499).
//
// R[x] is the free associative R-algebra on one generator (Lang
// §IV.1).  A single declaration of free_algebra_base<RigPolynomial<R>>::type
// = R fires the structural-trait propagation in algebra:free —
// associativity, commutativity, distributivity, saturation all lift
// from R to R[x] uniformly.
// ---------------------------------------------------------------------------

namespace dedekind::category {

template <typename R>
struct free_algebra_base<dedekind::algebra::RigPolynomial<R>> {
  using type = R;
};

}  // namespace dedekind::category
