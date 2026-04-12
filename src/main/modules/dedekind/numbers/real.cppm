/**
 * @file dedekind/numbers/real.cppm
 * @module dedekind.numbers:real
 * @brief Level 9: Minimal real wrapper for experimental reintegration.
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.numbers:real;

namespace dedekind::numbers {

export template <typename Q>
  requires std::regular<Q>
class Real {
 public:
  using Domain = Q;
  using path_type = Q;

  constexpr Real() = default;
  constexpr explicit Real(Q value) : value_(value) {}

  template <typename PathLike>
    requires requires(PathLike p, std::size_t n) {
      { p(n) } -> std::convertible_to<Q>;
    }
  constexpr explicit Real(PathLike p) : value_(static_cast<Q>(p(0))) {}

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

}  // namespace dedekind::numbers
