/**
 * @file dedekind/numbers/dual.cppm
 * @partition :dual
 * @brief Level 9.5: The Infinitesimal Extension 𝔻 — dual numbers
 *        a + bε with ε² = 0 (algebraic basis for forward-mode AD).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section The_Nilpotent_Basis
 * Reifies Dual Numbers a + bε where ε² = 0.
 * This provides the algebraic foundation for Forward-Mode
 * Automatic Differentiation (AD).
 *
 * @note "Musica est exercitium arithmeticae occultum nescientis se numerare
 * animi."
 *       ("Music is the pleasure the human mind experiences from counting
 * without being aware that it is counting.")
 *       -- Gottfried Wilhelm Leibniz, letter to Christian Goldbach (1712)
 */

module;
#include <concepts>

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
 *
 * @details Structural C++20 NTTP-compatible layout: primal / tangent fields
 *          are public so the type can be used as a non-type template
 *          parameter (e.g. `Halfspace2D<Dual<Rat>, …>` for parametric LP).
 *          Backward-compatible accessors `value()` / `derivative()` are
 *          kept for existing call sites.
 */
export template <typename F>
  requires std::regular<F>
struct Dual {
  using value_type = F;

  F val{};  ///< Primal   f(x). Public so the type is NTTP-structural.
  F der{};  ///< Tangent  f'(x). Public so the type is NTTP-structural.

  constexpr Dual() = default;
  constexpr Dual(F v, F d = F{}) : val(v), der(d) {}

  constexpr F value() const { return val; }
  constexpr F derivative() const { return der; }

  /** @section Dual_Arithmetic: ε² = 0 */

  friend constexpr bool operator==(const Dual&, const Dual&) = default;

  /**
   * @brief Primal-lex ordering: `a < b` iff `a.val < b.val`.
   *
   *  This is a partial order on `Dual<F>` (ties under primal equality
   *  are incomparable — we return `false`), projecting to the natural
   *  total order on the primal component `F`. Used by downstream
   *  reductions (e.g. argmax in `dedekind.optimization:lp`) whose
   *  ordering semantics are defined on the primal part only. Tangents
   *  are not part of the ordering — they ride along via the chain rule
   *  on arithmetic.
   */
  friend constexpr bool operator<(const Dual& a, const Dual& b) {
    return a.val < b.val;
  }

  friend constexpr Dual operator+(const Dual& a, const Dual& b) {
    return {a.val + b.val, a.der + b.der};
  }

  friend constexpr Dual operator-(const Dual& a, const Dual& b) {
    return {a.val - b.val, a.der - b.der};
  }

  constexpr Dual operator-() const { return {-val, -der}; }

  friend constexpr Dual operator*(const Dual& a, const Dual& b) {
    // (a + bε)(c + dε) = ac + (ad + bc)ε + bdε²(→0)
    return {a.val * b.val, (a.val * b.der) + (a.der * b.val)};
  }

  /**
   * @brief Multiplicative inverse: (a + bε)⁻¹ = (1/a) - (b/a²)ε.
   * Valid when a ≠ 0.
   */
  constexpr Dual inverse() const { return {F{1} / val, -der / (val * val)}; }

  friend constexpr Dual operator/(const Dual& a, const Dual& b) {
    return a * b.inverse();
  }
};

/** @section Formal_Verification */

// Basis element ε = Dual(0, 1); the nilpotent axiom ε² = 0.
inline constexpr Dual<double> eps{0.0, 1.0};
static_assert(eps * eps == Dual<double>{0.0, 0.0}, "Nilpotent axiom: ε² = 0.");

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
