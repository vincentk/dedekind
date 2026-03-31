/**
 * @file dedekind/numbers/complex.cppm
 * @partition :complex
 * @brief Level 9.1: The Algebraic Closure (C).
 *
 * @section The_Imaginary_Basis
 * This partition reifies the Complex species as a 2D Vector Space over R.
 * Following Hamilton’s approach, we define a complex number as an
 * ordered pair (a, b) where i² = -1.
 *
 * Wikipedia: Complex number, Algebraic closure
 */

export module dedekind.numbers:complex;

import :real;
import dedekind.algebra;

namespace dedekind::numbers {

using namespace dedekind::algebra;

/**
 * @concept IsComplex
 * @brief A species synthesized from the product of two Reals (Basis: 1, i).
 */
export template <typename C, typename R>
concept IsComplex = IsVectorSpace<C, R> && requires(C z) {
  { z.real() } -> std::same_as<R>;
  { z.imag() } -> std::same_as<R>;
};

/**
 * @class Complex
 * @brief The formal implementation of C over a Real field R.
 */
export template <typename R>
  requires IsField<R>
class Complex {
 public:
  using scalar_type = R;

  constexpr Complex(R re, R im) : re_(re), im_(im) {}

  constexpr R real() const { return re_; }
  constexpr R imag() const { return im_; }

  /** @section Complex_Arithmetic: The Rotation Rule */

  friend constexpr Complex operator+(const Complex& a, const Complex& b) {
    return {a.re_ + b.re_, a.im_ + b.im_};
  }

  friend constexpr Complex operator*(const Complex& a, const Complex& b) {
    // (a + bi)(c + di) = (ac - bd) + (ad + bc)i
    return {(a.re_ * b.re_) - (a.im_ * b.im_),
            (a.re_ * b.im_) + (a.im_ * b.re_)};
  }

 private:
  R re_, im_;
};

/** @section Formal_Verification */

static_assert(
    IsVectorSpace<Complex<Real<double>>, Real<double>>,
    "Axiom Failure: Complex numbers must form a Vector Space over R.");

static_assert(
    IsComplex<Complex<Real<double>>, Real<double>>,
    "Axiom Failure: Complex species must satisfy the IsComplex concept.");

}  // namespace dedekind::numbers
