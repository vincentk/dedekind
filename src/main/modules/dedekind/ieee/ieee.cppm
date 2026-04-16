/**
 * @file dedekind/ieee/ieee.cppm
 * @module dedekind.ieee
 * @brief Core IEEE fast lane as explicit physical plumbing.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "In dedekind.ieee, structure is clarified by explicit composition and
 * typed interfaces." (Module-specific documentation note for maintainers.)
 *       -- dedekind maintainers
 */
module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.ieee;

import dedekind.category;

namespace dedekind::ieee {
using namespace dedekind::category;

/**
 * @brief IEEE effect wrapper: values interpreted under machine IEEE semantics.
 */
export template <std::floating_point F>
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

  friend constexpr IEEE operator-(const IEEE& a) { return IEEE{-a.value_}; }

  friend constexpr IEEE operator/(const IEEE& a, const IEEE& b) {
    return IEEE{a.value_ / b.value_};
  }

 private:
  F value_{};
};

/** @brief Monadic unit for the IEEE effect context. */
export template <std::floating_point F>
constexpr IEEE<F> ieee_unit(F value) noexcept {
  return IEEE<F>{value};
}

/** @brief Functor map in the IEEE effect context. */
export template <std::floating_point F, typename Func>
constexpr IEEE<F> ieee_map(const IEEE<F>& x, Func&& f) {
  return IEEE<F>{static_cast<F>(f(x.resolve()))};
}

/** @brief Monadic bind in the IEEE effect context. */
export template <std::floating_point F, typename Func>
constexpr IEEE<F> ieee_bind(const IEEE<F>& x, Func&& f) {
  return f(x.resolve());
}

/** @brief Certified operation token for addition under IEEE-by-fiat semantics.
 */
export template <std::floating_point F>
struct IEEEAdd {
  using value_type = IEEE<F>;
  using logic_species = ClassicalLogic;

  constexpr IEEE<F> operator()(
      std::pair<const IEEE<F>&, const IEEE<F>&> p) const noexcept {
    auto [a, b] = p;
    return a + b;
  }

  static constexpr std::pair<F, F> sensitivity(const IEEE<F>&,
                                               const IEEE<F>&) noexcept {
    return {F{1}, F{1}};
  }
};

/** @brief Certified operation token for multiplication under IEEE-by-fiat
 * semantics. */
export template <std::floating_point F>
struct IEEEMul {
  using value_type = IEEE<F>;
  using logic_species = ClassicalLogic;

  constexpr IEEE<F> operator()(
      std::pair<const IEEE<F>&, const IEEE<F>&> p) const noexcept {
    auto [a, b] = p;
    return a * b;
  }

  static constexpr std::pair<F, F> sensitivity(const IEEE<F>& a,
                                               const IEEE<F>& b) noexcept {
    return {b.resolve(), a.resolve()};
  }
};

}  // namespace dedekind::ieee

namespace dedekind::category {

template <std::floating_point F>
struct SpeciesTraits<dedekind::ieee::IEEE<F>> {
  using Domain = dedekind::ieee::IEEE<F>;
  using machine_type = F;
};

template <std::floating_point F>
struct is_associative<dedekind::ieee::IEEE<F>, dedekind::ieee::IEEEAdd<F>>
    : std::true_type {};

template <std::floating_point F>
struct is_commutative<dedekind::ieee::IEEE<F>, dedekind::ieee::IEEEAdd<F>>
    : std::true_type {};

template <std::floating_point F>
struct is_associative<dedekind::ieee::IEEE<F>, dedekind::ieee::IEEEMul<F>>
    : std::true_type {};

template <std::floating_point F>
struct is_commutative<dedekind::ieee::IEEE<F>, dedekind::ieee::IEEEMul<F>>
    : std::true_type {};

template <std::floating_point F>
inline constexpr bool is_kleene_associative_v<dedekind::ieee::IEEE<F>,
                                              dedekind::ieee::IEEEAdd<F>> =
    true;

template <std::floating_point F>
inline constexpr bool is_kleene_commutative_v<dedekind::ieee::IEEE<F>,
                                              dedekind::ieee::IEEEAdd<F>> =
    true;

template <std::floating_point F>
inline constexpr dedekind::ieee::IEEE<F>
    partial_identity_v<dedekind::ieee::IEEE<F>, dedekind::ieee::IEEEAdd<F>> =
        dedekind::ieee::IEEE<F>{F{0}};

template <std::floating_point F>
inline constexpr bool is_kleene_associative_v<dedekind::ieee::IEEE<F>,
                                              dedekind::ieee::IEEEMul<F>> =
    true;

template <std::floating_point F>
inline constexpr bool is_kleene_commutative_v<dedekind::ieee::IEEE<F>,
                                              dedekind::ieee::IEEEMul<F>> =
    true;

template <std::floating_point F>
inline constexpr dedekind::ieee::IEEE<F>
    partial_identity_v<dedekind::ieee::IEEE<F>, dedekind::ieee::IEEEMul<F>> =
        dedekind::ieee::IEEE<F>{F{1}};

}  // namespace dedekind::category
