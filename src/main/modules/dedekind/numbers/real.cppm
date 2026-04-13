/**
 * @file dedekind/numbers/real.cppm
 * @module dedekind.numbers:real
 * @brief Level 9: Minimal real wrapper for experimental reintegration.
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.numbers:real;

import dedekind.category;
import dedekind.sets;
import :rational;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

// Canonical machine realization for the real scalar carrier.
export using machine_real_scalar = double;

export template <typename S>
concept IsRealCarrier = requires(S a, S b) {
  S{};
  { a + b } -> std::same_as<S>;
  { a - b } -> std::same_as<S>;
  { a * b } -> std::same_as<S>;
  { a / b } -> std::same_as<S>;
  { a < b } -> std::convertible_to<bool>;
};

export template <IsRealCarrier Q>
class Real {
 public:
  using Domain = Q;
  using path_type = Q;

  constexpr Real() = default;
  constexpr explicit Real(Q value) : value_(value) {}

  constexpr Q resolve() const { return value_; }
  constexpr Q path() const { return value_; }

  // Compatibility with prior symbolic-cut style usage.
  constexpr bool operator()(const Q& x) const { return x < value_; }

  friend constexpr bool operator==(const Real&, const Real&) = default;

  friend constexpr Real operator+(const Real& a, const Real& b) {
    return Real{a.value_ + b.value_};
  }

  friend constexpr Real operator*(const Real& a, const Real& b) {
    return Real{a.value_ * b.value_};
  }

  friend constexpr Real operator-(const Real& a, const Real& b) {
    return Real{a.value_ - b.value_};
  }

  friend constexpr Real operator/(const Real& a, const Real& b) {
    return Real{a.value_ / b.value_};
  }

 private:
  Q value_{};
};

/**
 * @brief Machine realization arrow ℚ ↪ ℝ: Rational<I> → Real<S>.
 * @details Converts a rational p/q to the closest IEEE 754 value of type S.
 */
export template <IsInteger I = machine_integer,
                 IsRealCarrier S = machine_real_scalar>
inline constexpr auto embed_ℚ_ℝ =
    arrow<Rational<I>, Real<S>>([](const Rational<I>& q) noexcept {
      return Real<S>{static_cast<S>(q.num()) / static_cast<S>(q.den())};
    });

/**
 * @brief Characteristic morphism for ℝ: the real numbers.
 * Accepts native Real<S> and delegates predecessor checks through ℚ.
 */
export template <IsRealCarrier S = machine_real_scalar,
                 IsInteger I = machine_integer, typename L = ClassicalLogic,
                 typename C = ℶ_1>
struct RealsOf {
  using Domain = Real<S>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native Real<S>: always a member of ℝ
  constexpr typename L::Ω operator()(const Real<S>&) const { return L::True; }

  // Direct parent: embed Rational<I> into ℝ via the canonical arrow.
  constexpr typename L::Ω operator()(const Rational<I>& q) const {
    return operator()(embed_ℚ_ℝ<I, S>(q));
  }

  // Delegate non-parent ancestors to ambient ℚ.
  template <typename T>
    requires(!std::same_as<T, Real<S>> && !std::same_as<T, Rational<I>>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::RationalsOf<I>{}(x);
  }
};

export using RealSet = RealsOf<>;
export using ℝ = RealSet;

export inline constexpr ℝ R{};

}  // namespace dedekind::numbers

namespace dedekind::category {
template <typename Q>
struct SpeciesTraits<dedekind::numbers::Real<Q>> {
  using Domain = dedekind::numbers::Real<Q>;
  using machine_type = dedekind::numbers::Real<Q>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℚ_ℝ<>)>> =
        true;
}  // namespace dedekind::category
