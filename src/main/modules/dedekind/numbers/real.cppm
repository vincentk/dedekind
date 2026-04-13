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

export template <typename Q>
  requires std::regular<Q>
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

  friend constexpr Real operator/(const Real& a, const Real& b) {
    return Real{a.value_ / b.value_};
  }

 private:
  Q value_{};
};

export template <typename Q = double, typename L = ClassicalLogic,
                 typename C = ℶ_1>
using RealSetOf = Ω<Real<Q>, L, C>;

export using RealSet = RealSetOf<>;
export using ℝ = RealSet;

export inline constexpr ℝ R{};

/**
 * @brief Canonical embedding ℚ ↪ ℝ: Rational<int> → Real<double>.
 * @details Converts a rational p/q to the closest IEEE 754 double.
 *          The embedding is exact for rationals representable in double;
 *          rounding is acknowledged by the ℶ_1 cardinality of ℝ.
 */
export inline constexpr auto embed_Q_R =
    arrow<Rational<int>, Real<double>>([](const Rational<int>& q) noexcept {
      return Real<double>{static_cast<double>(q.num()) /
                          static_cast<double>(q.den())};
    });

}  // namespace dedekind::numbers

namespace dedekind::category {
template <typename Q>
struct SpeciesTraits<dedekind::numbers::Real<Q>> {
  using Domain = dedekind::numbers::Real<Q>;
  using machine_type = dedekind::numbers::Real<Q>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_Q_R)>> =
        true;
}  // namespace dedekind::category
