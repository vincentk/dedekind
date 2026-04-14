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
 * This wrapper is intentionally explicit. Importing this partition is a local,
 * compile-time opt-in for fast-lane algebraic certifications.
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

/** @brief Certified operation token for addition under IEEE-by-fiat semantics.
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
};

/** @brief Certified operation token for multiplication under IEEE-by-fiat
 * semantics. */
export template <std::floating_point F = machine_real_scalar>
struct IEEEMul {
  using value_type = IEEE<F>;
  using logic_species = ClassicalLogic;

  constexpr IEEE<F> operator()(
      std::pair<const IEEE<F>&, const IEEE<F>&> p) const noexcept {
    auto [a, b] = p;
    return a * b;
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
