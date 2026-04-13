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

}  // namespace dedekind::numbers
