module;
#include <compare>
#include <concepts>
#include <numeric>
#include <stdexcept>

/**
 * @file dedekind/numbers/rational.cppm
 * @module dedekind.numbers:rational
 * @brief Level 8: The Quotient Field (Q).
 */

export module dedekind.numbers:rational;

import dedekind.category;
import dedekind.sets;
import :integer;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @class Rational
 * @brief The Field of Fractions over an Integral Domain Z.
 */
export template <std::signed_integral Z>
class Rational {
 public:
  using Domain = Z;

  constexpr Rational(Z num, Z den) : num_(num), den_(den) { simplify(); }

  /** @section The_Simplification_Morphism */
  constexpr void simplify() {
    if (den_ == Z{0}) throw std::domain_error("Rational: Division by zero.");

    Z common = std::gcd(num_, den_);
    num_ = num_ / common;
    den_ = den_ / common;

    // Canonical sign: denominator is always positive
    if (den_ < Z{0}) {
      num_ = -num_;
      den_ = -den_;
    }
  }

  /** @section Relational_Morphisms */

  // (a/b) <= (c/d) <=> ad <= cb (assuming positive denominators)
  friend constexpr bool operator<=(const Rational& a, const Rational& b) {
    return (a.num_ * b.den_) <= (b.num_ * a.den_);
  }

  friend constexpr bool operator==(const Rational&, const Rational&) = default;

  constexpr Z num() const { return num_; }
  constexpr Z den() const { return den_; }

 public:
  /** @section Multiplicative_Inverse: The Reciprocal */
  constexpr Rational inverse() const {
    if (num_ == Z{0}) {
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

  friend constexpr Rational operator+(const Rational& a, const Rational& b) {
    return Rational((a.num_ * b.den_) + (b.num_ * a.den_), a.den_ * b.den_);
  }

  friend constexpr Rational operator-(const Rational& a, const Rational& b) {
    return a + (-b);
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

// Proof: The inverse of 2/3 is 3/2.
static_assert((Rational<int>(2, 3).inverse().num() == 3));

/**
 * @brief Characteristic morphism for ℚ: the rationals.
 * Accepts native Rational<int> and delegates predecessor checks through ℤ.
 */
export template <std::signed_integral I = int, typename L = ClassicalLogic,
                 typename C = ℵ_0>
struct RationalsOf {
  using Domain = Rational<I>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native Rational<int>: always a member of ℚ
  constexpr typename L::Ω operator()(const Rational<I>&) const {
    return L::True;
  }

  // Direct parent: embed int into ℚ.
  constexpr typename L::Ω operator()(int z) const {
    return operator()(Rational<I>{static_cast<I>(z), static_cast<I>(1)});
  }

  // Delegate non-parent ancestors to ambient ℤ.
  template <typename T>
    requires(!std::same_as<T, Rational<I>> && !std::same_as<T, int>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::Z(x);
  }
};

export using RationalSet = RationalsOf<>;
export using ℚ = RationalSet;

export inline constexpr ℚ Q{};

/**
 * @brief Canonical embedding ℤ ↪ ℚ: int → Rational<int>.
 * @details Every integer n embeds as the fraction n/1.
 *          This is the standard ring homomorphism Z → Q.
 */
export inline constexpr auto embed_ℤ_ℚ = arrow<int, Rational<int>>(
    [](const int& n) noexcept { return Rational<int>{n, 1}; });

}  // namespace dedekind::numbers

namespace dedekind::category {
template <std::signed_integral Z>
struct SpeciesTraits<dedekind::numbers::Rational<Z>> {
  using Domain = dedekind::numbers::Rational<Z>;
  using machine_type = dedekind::numbers::Rational<Z>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ)>> =
        true;
}  // namespace dedekind::category
