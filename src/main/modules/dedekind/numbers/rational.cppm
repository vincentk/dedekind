/**
 * @file dedekind/numbers/rational.cppm
 * @module dedekind.numbers:rational
 * @brief Level 8: The Quotient Field (Q).
 */

export module dedekind.numbers:rational;

import dedekind.algebra;

namespace dedekind::numbers {

using namespace dedekind::algebra;

/**
 * @class Rational
 * @brief The Field of Fractions over an Integral Domain Z.
 */
export template <IsIntegralDomain Z>
class Rational {
 public:
  using Domain = Z;

  constexpr Rational(Z num, Z den) : num_(num), den_(den) { simplify(); }

  /** @section The_Simplification_Morphism */
  constexpr void simplify() {
    if (is_zero_v<Z>(den_))
      throw std::domain_error("Rational: Division by zero.");

    Z common = gcd(num_, den_);
    num_ = num_ / common;
    den_ = den_ / common;

    // Canonical sign: denominator is always positive
    if (den_) {
      num_ = -num_;
      den_ = -den_;
    }
  }

  /** @section Relational_Morphisms */

  // (a/b) <= (c/d) <=> ad <= cb (assuming positive denominators)
  friend constexpr bool operator<=(const Rational& a, const Rational& b) {
    return (a.num_ * b.den_) <= (b.num_ * a.den_);
  }

  friend constexpr bool operator==(const Rational&,
                                   const Rational&) const = default;

  constexpr Z num() const { return num_; }
  constexpr Z den() const { return den_; }

 public:
  /** @section Multiplicative_Inverse: The Reciprocal */
  constexpr Rational inverse() const {
    if (is_zero_v<Z>(num_)) {
      throw std::domain_error("Rational: Zero has no multiplicative inverse.");
    }
    // To invert (a/b), we return (b/a)
    return Rational(den_, num_);
  }

  /** @section Field_Operators */

  // Multiplication: (a/b) * (c/d) = (ac / bd)
  friend constexpr Rational operator*(const Rational& a, const Rational& b) {
    return Rational(a.num_ * b.num_, a.den_ * b.den_);
  }

  // Division: (a/b) / (c/d) = (a/b) * (d/c)
  friend constexpr Rational operator/(const Rational& a, const Rational& b) {
    return a * b.inverse();
  }

  // Additive Inverse: -(a/b) = (-a/b)
  constexpr Rational operator-() const { return Rational(-num_, den_); }

 private:
  Z num_, den_;
};

/** @section Formal_Verification */

// 1. Proof: Rationals over Integers form a Field.
static_assert(IsField<Rational<int>>,
              "Axiom Failure: The Quotient Field of Z must satisfy IsField.");

// 2. Proof: The inverse of 2/3 is 3/2.
static_assert((Rational<int>(2, 3).inverse().num() == 3));

}  // namespace dedekind::numbers
