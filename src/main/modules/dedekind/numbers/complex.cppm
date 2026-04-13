/**
 * @file dedekind/numbers/complex.cppm
 * @partition :complex
 * @brief Minimal complex wrapper for experimental reintegration.
 */
module;

#include <concepts>

export module dedekind.numbers:complex;

import dedekind.category;
import dedekind.sets;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename C, typename R>
concept IsComplex = requires(C z) {
  { z.real() } -> std::same_as<R>;
  { z.imag() } -> std::same_as<R>;
};

export template <typename R>
  requires std::regular<R>
class Complex {
 public:
  using scalar_type = R;

  constexpr Complex(R re = R{}, R im = R{}) : re_(re), im_(im) {}

  constexpr R real() const { return re_; }
  constexpr R imag() const { return im_; }

  friend constexpr Complex operator+(const Complex& a, const Complex& b) {
    return {a.re_ + b.re_, a.im_ + b.im_};
  }

  friend constexpr Complex operator*(const Complex& a, const Complex& b) {
    return {(a.re_ * b.re_) - (a.im_ * b.im_),
            (a.re_ * b.im_) + (a.im_ * b.re_)};
  }

 private:
  R re_{};
  R im_{};
};

export template <typename R = double, typename L = ClassicalLogic,
                 typename C = ℶ_1>
using ComplexSetOf = Ω<Complex<R>, L, C>;

export using ComplexSet = ComplexSetOf<>;
export using ℂ = ComplexSet;

export inline constexpr ℂ C{};

}  // namespace dedekind::numbers
