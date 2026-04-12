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

namespace dedekind::numbers {

using namespace dedekind::algebra;

/**
 * @class Dual
 * @brief Represents f(x) + f'(x)ε.
 */
export template <typename F>
  requires std::regular<F>
class Dual {
 public:
  using value_type = F;

  constexpr Dual(F val, F der = F{})
      : val_(val), der_(der) {}

  constexpr F value() const { return val_; }
  constexpr F derivative() const { return der_; }

  /** @section Dual_Arithmetic: ε² = 0 */

  friend constexpr Dual operator+(const Dual& a, const Dual& b) {
    return {a.val_ + b.val_, a.der_ + b.der_};
  }

  friend constexpr Dual operator*(const Dual& a, const Dual& b) {
    // (a + be)(c + de) = ac + (ad + bc)e + bde²(->0)
    return {a.val_ * b.val_, (a.val_ * b.der_) + (a.der_ * b.val_)};
  }

 private:
  F val_, der_;
};

/** @section Formal_Verification
 * Deferred while Dual witnesses are being retargeted to the active ring
 * contracts.
 */

}  // namespace dedekind::numbers
