/**
 * @file dedekind/numbers/ieee.cppm
 * @module dedekind.numbers.ieee
 * @brief Explicit IEEE fast lane: opt-in effect context for machine floats.
 */
module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.numbers.ieee;

import dedekind.category;
import dedekind.numbers;

namespace dedekind::numbers {
using namespace dedekind::category;

/**
 * @brief IEEE effect wrapper: values interpreted under machine IEEE semantics.
 *
 * This wrapper is intentionally explicit. Importing this module is a
 * compile-time opt-in: algebraic properties (associativity, commutativity)
 * are certified by policy (fiat) rather than derived. The module boundary
 * is the semantic boundary — outside this import, IEEE<F> is unknown.
 *
 * The design follows the "effect context" or monad pattern: @c assume_ieee
 * and @c discharge_ieee are the unit and counit of the IEEE effect, and
 * @c ieee_map / @c ieee_bind are the functor/monad combinators.
 *
 * @ref IEEE Std 754-2019. "IEEE Standard for Floating-Point Arithmetic".
 *      IEEE, New York. doi:10.1109/IEEESTD.2019.8766229
 * @ref Higham, N.J. (2002). "Accuracy and Stability of Numerical Algorithms"
 *      (2nd ed.). SIAM. ISBN 978-0-89871-521-7. §2.2 (model of floating-point
 *      arithmetic and rounding).
 */
export template <std::floating_point F = machine_real_scalar>
class IEEE {
 public:
  using Domain = F;

  constexpr IEEE() = default;
  constexpr explicit IEEE(F value) : value_(value) {}

  constexpr F resolve() const { return value_; }

  friend constexpr bool operator==(const IEEE&, const IEEE&) = default;

  friend constexpr IEEE operator+(const IEEE& a, const IEEE& b) {
    return IEEE{a.value_ + b.value_};
  }

  friend constexpr IEEE operator*(const IEEE& a, const IEEE& b) {
    return IEEE{a.value_ * b.value_};
  }

  friend constexpr IEEE operator-(const IEEE& a, const IEEE& b) {
    return IEEE{a.value_ - b.value_};
  }

  friend constexpr IEEE operator/(const IEEE& a, const IEEE& b) {
    return IEEE{a.value_ / b.value_};
  }

 private:
  F value_{};
};

/** @brief Monadic unit for the IEEE effect context. */
export template <std::floating_point F = machine_real_scalar>
constexpr IEEE<F> ieee_unit(F value) noexcept {
  return IEEE<F>{value};
}

/**
 * @brief Functor map in the IEEE effect context.
 *
 * We keep map shape-preserving for a single scalar carrier F.
 */
export template <std::floating_point F, typename Func>
constexpr IEEE<F> ieee_map(const IEEE<F>& x, Func&& f) {
  return IEEE<F>{static_cast<F>(f(x.resolve()))};
}

/**
 * @brief Monadic bind in the IEEE effect context.
 *
 * f must be a Kleisli arrow of shape F -> IEEE<F>.
 */
export template <std::floating_point F, typename Func>
constexpr IEEE<F> ieee_bind(const IEEE<F>& x, Func&& f) {
  return f(x.resolve());
}

/** @brief Explicit entry from the honest lane into IEEE fast lane. */
export template <std::floating_point F = machine_real_scalar>
constexpr IEEE<F> assume_ieee(const Real<F>& r) noexcept {
  return IEEE<F>{r.resolve()};
}

/** @brief Explicit exit from IEEE fast lane into the honest lane carrier. */
export template <std::floating_point F = machine_real_scalar>
constexpr Real<F> discharge_ieee(const IEEE<F>& x) noexcept {
  return Real<F>{x.resolve()};
}

/**
 * @brief Certified operation token for addition under IEEE-by-fiat semantics.
 *
 * Algebraic properties (associativity, commutativity) are declared by policy
 * (fiat), not derived — the same opt-in certification used for IEEE<F> itself.
 *
 * The @c sensitivity() method returns the exact partial derivatives
 * ∂(a+b)/∂a = 1, ∂(a+b)/∂b = 1. These are compile-time constants and
 * require no floating-point arithmetic, making them safe to use as the
 * ground level of a Gaussian error propagation chain without circularity.
 *
 * @ref Ku, H.H. (1966). "Notes on the use of propagation of error formulas".
 *      J. Research NBS 70C(4):262–263. doi:10.6028/jres.070C.025
 * @ref Ogita, T.; Rump, S.M.; Oishi, S. (2005). "Accurate Sum and Dot
 *      Product". SIAM J. Sci. Comput. 26(6):1955–1988.
 *      doi:10.1137/030601818
 */
export template <std::floating_point F = machine_real_scalar>
struct IEEEAdd {
  using value_type = IEEE<F>;
  using logic_species = ClassicalLogic;

  constexpr IEEE<F> operator()(
      std::pair<const IEEE<F>&, const IEEE<F>&> p) const noexcept {
    auto [a, b] = p;
    return a + b;
  }

  /**
   * @brief Exact local sensitivities: ∂(a+b)/∂a = 1, ∂(a+b)/∂b = 1.
   *
   * Both partial derivatives are identically 1 regardless of operand values.
   * Declared as a compile-time constant pair — no IEEE arithmetic involved.
   */
  static constexpr std::pair<F, F> sensitivity(const IEEE<F>&,
                                               const IEEE<F>&) noexcept {
    return {F{1}, F{1}};
  }
};

/**
 * @brief Certified operation token for multiplication under IEEE-by-fiat
 * semantics.
 *
 * @c sensitivity() returns ∂(a·b)/∂a = b, ∂(a·b)/∂b = a by reading the
 * already-computed operand values. This is a direct lookup, not a new
 * arithmetic chain, so there is no bootstrapping circularity.
 *
 * @ref Ku, H.H. (1966). "Notes on the use of propagation of error formulas".
 *      J. Research NBS 70C(4):262–263. doi:10.6028/jres.070C.025
 * @ref Goodman, L.A. (1960). "On the Exact Variance of Products".
 *      J. Amer. Statist. Assoc. 55(292):708–713. doi:10.2307/2281592
 */
export template <std::floating_point F = machine_real_scalar>
struct IEEEMul {
  using value_type = IEEE<F>;
  using logic_species = ClassicalLogic;

  constexpr IEEE<F> operator()(
      std::pair<const IEEE<F>&, const IEEE<F>&> p) const noexcept {
    auto [a, b] = p;
    return a * b;
  }

  /**
   * @brief Exact local sensitivities: ∂(a·b)/∂a = b, ∂(a·b)/∂b = a.
   *
   * Returns the operand values, swapped. No new arithmetic — just a read
   * of already-resolved floats. Safe to call at the base of an error chain.
   */
  static constexpr std::pair<F, F> sensitivity(const IEEE<F>& a,
                                               const IEEE<F>& b) noexcept {
    return {b.resolve(), a.resolve()};
  }
};

}  // namespace dedekind::numbers

namespace dedekind::category {

/**
 * @brief Atlas registration for the opt-in IEEE fast-lane species.
 *
 * This declares algebraic laws by policy (fiat) for imported contexts.
 */
template <std::floating_point F>
struct SpeciesTraits<dedekind::numbers::IEEE<F>> {
  using Domain = dedekind::numbers::IEEE<F>;
  using machine_type = F;

  template <typename Op>
  static constexpr bool is_associative_v = true;

  template <typename Op>
  static constexpr bool is_commutative_v = true;
};

template <std::floating_point F>
struct is_associative<dedekind::numbers::IEEE<F>, dedekind::numbers::IEEEAdd<F>>
    : std::true_type {};

template <std::floating_point F>
struct is_commutative<dedekind::numbers::IEEE<F>, dedekind::numbers::IEEEAdd<F>>
    : std::true_type {};

template <std::floating_point F>
struct is_associative<dedekind::numbers::IEEE<F>, dedekind::numbers::IEEEMul<F>>
    : std::true_type {};

template <std::floating_point F>
struct is_commutative<dedekind::numbers::IEEE<F>, dedekind::numbers::IEEEMul<F>>
    : std::true_type {};

template <std::floating_point F>
inline constexpr bool is_kleene_associative_v<dedekind::numbers::IEEE<F>,
                                              dedekind::numbers::IEEEAdd<F>> =
    true;

template <std::floating_point F>
inline constexpr bool is_kleene_commutative_v<dedekind::numbers::IEEE<F>,
                                              dedekind::numbers::IEEEAdd<F>> =
    true;

template <std::floating_point F>
inline constexpr dedekind::numbers::IEEE<F> partial_identity_v<
    dedekind::numbers::IEEE<F>, dedekind::numbers::IEEEAdd<F>> =
    dedekind::numbers::IEEE<F>{F{0}};

template <std::floating_point F>
inline constexpr bool is_kleene_associative_v<dedekind::numbers::IEEE<F>,
                                              dedekind::numbers::IEEEMul<F>> =
    true;

template <std::floating_point F>
inline constexpr bool is_kleene_commutative_v<dedekind::numbers::IEEE<F>,
                                              dedekind::numbers::IEEEMul<F>> =
    true;

template <std::floating_point F>
inline constexpr dedekind::numbers::IEEE<F> partial_identity_v<
    dedekind::numbers::IEEE<F>, dedekind::numbers::IEEEMul<F>> =
    dedekind::numbers::IEEE<F>{F{1}};

}  // namespace dedekind::category
