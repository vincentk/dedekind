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
import :real;

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

/**
 * @brief Canonical embedding ℝ ↪ ℂ: Real<double> → Complex<double>.
 * @details Every real x embeds as the complex number (x + 0i).
 *          This is the canonical ring monomorphism R → C.
 */
export inline constexpr auto embed_R_C =
    arrow<Real<double>, Complex<double>>([](const Real<double>& r) noexcept {
      return Complex<double>{r.resolve(), 0.0};
    });

}  // namespace dedekind::numbers

namespace dedekind::category {
template <typename R>
struct SpeciesTraits<dedekind::numbers::Complex<R>> {
  using Domain = dedekind::numbers::Complex<R>;
  using machine_type = dedekind::numbers::Complex<R>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_R_C)>> =
        true;
}  // namespace dedekind::category
