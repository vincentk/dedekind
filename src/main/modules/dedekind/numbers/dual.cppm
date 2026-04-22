/**
 * @file dedekind/numbers/dual.cppm
 * @partition :dual
 * @brief Module interface in the dedekind hierarchy.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Musica est exercitium arithmeticae occultum nescientis se numerare
 * animi."
 *       ("Music is the pleasure the human mind experiences from counting
 * without being aware that it is counting.")
 *       -- Gottfried Wilhelm Leibniz, letter to Christian Goldbach (1712)
 */

module;
#include <concepts>

/**
 * @file dedekind/numbers/dual.cppm
 * @partition :dual
 * @brief Level 9.5: The Infinitesimal Extension (D).
 *
 * @section The_Nilpotent_Basis
 * Reifies Dual Numbers a + bε where ε² = 0.
 * This provides the algebraic foundation for Forward-Mode
 * Automatic Differentiation (AD).
 */

export module dedekind.numbers:dual;

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

namespace dedekind::numbers {

using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @class Dual
 * @brief Represents f(x) + f'(x)ε.
 */
export template <typename F>
  requires std::regular<F>
class Dual {
 public:
  using value_type = F;

  constexpr Dual(F val, F der = F{}) : val_(val), der_(der) {}

  constexpr F value() const { return val_; }
  constexpr F derivative() const { return der_; }

  /** @section Dual_Arithmetic: ε² = 0 */

  friend constexpr bool operator==(const Dual&, const Dual&) = default;

  friend constexpr Dual operator+(const Dual& a, const Dual& b) {
    return {a.val_ + b.val_, a.der_ + b.der_};
  }

  friend constexpr Dual operator-(const Dual& a, const Dual& b) {
    return {a.val_ - b.val_, a.der_ - b.der_};
  }

  constexpr Dual operator-() const { return {-val_, -der_}; }

  friend constexpr Dual operator*(const Dual& a, const Dual& b) {
    // (a + be)(c + de) = ac + (ad + bc)e + bde²(->0)
    return {a.val_ * b.val_, (a.val_ * b.der_) + (a.der_ * b.val_)};
  }

  /**
   * @brief Multiplicative inverse: (a + bε)⁻¹ = (1/a) - (b/a²)ε.
   * Valid when a ≠ 0.
   */
  constexpr Dual inverse() const { return {F{1} / val_, -der_ / (val_ * val_)}; }

  friend constexpr Dual operator/(const Dual& a, const Dual& b) {
    return a * b.inverse();
  }

 private:
  F val_, der_;
};

/** @section Formal_Verification */

// Basis element ε = Dual(0, 1); the nilpotent axiom ε² = 0.
inline constexpr Dual<double> eps{0.0, 1.0};
static_assert(eps * eps == Dual<double>{0.0, 0.0},
              "Nilpotent axiom: ε² = 0.");

// Forward-mode AD correctness: d/dx(x²)|_{x=3} = 6.
// Dual(3, 1) seeds x with derivative 1; squaring gives value 9, derivative 6.
inline constexpr Dual<double> x_seed{3.0, 1.0};
static_assert(x_seed * x_seed == Dual<double>{9.0, 6.0},
              "AD rule: d/dx(x²)|_{x=3} = 6.");

// Dual<double> is field-like: +, -, unary -, *, / are all defined and closed.
static_assert(dedekind::algebra::IsFieldLikeScalar<Dual<double>>,
              "Dual<double> must satisfy the operational field-like witness.");


export template <typename F = double, typename L = ClassicalLogic,
                 typename C = ℶ_1>
using DualSetOf = Ω<Dual<F>, L, C>;

export using DualSet = DualSetOf<>;
export using 𝔻 = DualSet;

export inline constexpr 𝔻 D{};

}  // namespace dedekind::numbers
