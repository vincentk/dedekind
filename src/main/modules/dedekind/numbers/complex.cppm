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

/**
 * @brief Characteristic morphism for ℂ: the complex numbers.
 * Accepts native Complex<R> and all embedded predecessors
 * (Real<R>, Rational<int>, int, unsigned, Ternary).
 */
export template <std::floating_point R = double, typename L = ClassicalLogic,
                 typename C = ℶ_1>
struct ComplexesOf {
  using Domain = Complex<R>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native Complex<R>: always a member of ℂ
  constexpr typename L::Ω operator()(const Complex<R>&) const {
    return L::True;
  }

  // Direct parent: embed Real<R> into ℂ (canonical x -> x + 0i).
  constexpr typename L::Ω operator()(const Real<R>& r) const {
    return operator()(
        Complex<R>{static_cast<R>(r.resolve()), static_cast<R>(0)});
  }

  // Delegate non-parent ancestors to ambient ℝ.
  template <typename T>
    requires(!std::same_as<T, Complex<R>> && !std::same_as<T, Real<R>>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::R(x);
  }
};

export using ComplexSet = ComplexesOf<>;
export using ℂ = ComplexSet;

export inline constexpr ℂ C{};

/**
 * @brief Canonical embedding ℝ ↪ ℂ: Real<double> → Complex<double>.
 * @details Every real x embeds as the complex number (x + 0i).
 *          This is the canonical ring monomorphism R → C.
 */
export inline constexpr auto embed_ℝ_ℂ =
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
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℝ_ℂ)>> =
        true;
}  // namespace dedekind::category
