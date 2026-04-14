/**
 * @file dedekind/numbers/complex.cppm
 * @partition :complex
 * @brief Minimal complex wrapper for experimental reintegration.
 */
module;

#include <concepts>
#include <utility>

export module dedekind.numbers:complex;

import dedekind.category;
import dedekind.sets;
import :real;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename S>
concept IsComplexScalar = requires(S a, S b) {
  S{};
  { a + b } -> std::same_as<S>;
  { a - b } -> std::same_as<S>;
  { a * b } -> std::same_as<S>;
};

export template <typename R>
  requires IsComplexScalar<R>
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

/** @section Partial_Arithmetic_with_Ternary_Logic */

/**
 * @brief Partial addition transform for Complex<R>.
 *
 * Complex addition is component-wise on the scalar type R.
 * The operation always completes (returns Ternary::True); any
 * non-finite results from the carrier are observable in the value.
 */
export template <typename R>
  requires IsComplexScalar<R>
struct PartialAddComplex {
  using value_type = Complex<R>;
  using logic_species = TernaryLogic;

  TernaryResult<Complex<R>> operator()(
      std::pair<const Complex<R>&, const Complex<R>&> p) const noexcept {
    auto [a, b] = p;
    return {Ternary::True, a + b};
  }
};

/**
 * @brief Partial multiplication transform for Complex<R>.
 *
 * Complex multiplication (a+bi)(c+di) = (ac-bd) + (ad+bc)i
 * inherits the partiality of R's arithmetic.
 */
export template <typename R>
  requires IsComplexScalar<R>
struct PartialMulComplex {
  using value_type = Complex<R>;
  using logic_species = TernaryLogic;

  TernaryResult<Complex<R>> operator()(
      std::pair<const Complex<R>&, const Complex<R>&> p) const noexcept {
    auto [a, b] = p;
    return {Ternary::True, a * b};
  }
};

/**
 * @brief Identity and Associativity traits for Complex arithmetic.
 *
 * Complex<R> arithmetic is commutative (same component-wise FP operations
 * regardless of argument order) but NOT associative when R is a floating-point
 * type: rounding means (a+b)+c ≠ a+(b+c) in general.
 * Commutativity holds; associativity-by-fiat belongs to the IEEE<F> opt-in
 * wrapper only.
 *
 * Partial identities: 0+0i for addition, 1+0i for multiplication.
 *
 * Specializations are declared in the dedekind::category namespace (see below).
 */

/**
 * @brief Embedding transform: ℝ ↪ ℂ with Ternary acknowledgment.
 *
 * The embedding of a real R into the complex numbers is **exact**:
 * every real x corresponds uniquely to (x + 0i).
 * This transform returns Ternary::True to signal no information loss.
 */
export template <IsRealCarrier R = machine_real_scalar>
struct PartialEmbedRealToComplex {
  using value_type = Complex<R>;
  using logic_species = TernaryLogic;

  TernaryResult<Complex<R>> operator()(const Real<R>& r) const noexcept {
    return {Ternary::True, Complex<R>{r.resolve(), R{}}};
  }
};

}  // namespace dedekind::numbers

namespace dedekind::category {

/** @brief Kleene traits for complex arithmetic.
 *
 * Commutativity holds for IEEE 754 component-wise operations.
 * Associativity does NOT hold for floating-point carriers — that opt-in
 * belongs exclusively to dedekind::ieee::IEEE<F>.
 */
template <typename R>
  requires dedekind::numbers::IsComplexScalar<R>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Complex<R>, dedekind::numbers::PartialAddComplex<R>> =
    true;

template <typename R>
  requires dedekind::numbers::IsComplexScalar<R>
inline constexpr dedekind::numbers::Complex<R> partial_identity_v<
    dedekind::numbers::Complex<R>, dedekind::numbers::PartialAddComplex<R>> =
    dedekind::numbers::Complex<R>{R{}, R{}};

template <typename R>
  requires dedekind::numbers::IsComplexScalar<R>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Complex<R>, dedekind::numbers::PartialMulComplex<R>> =
    true;

template <typename R>
  requires dedekind::numbers::IsComplexScalar<R>
inline constexpr dedekind::numbers::Complex<R> partial_identity_v<
    dedekind::numbers::Complex<R>, dedekind::numbers::PartialMulComplex<R>> =
    dedekind::numbers::Complex<R>{R{1}, R{}};

/** @brief Kleene traits for real→complex embedding (exact). */
template <dedekind::numbers::IsRealCarrier R>
inline constexpr bool
    is_kleene_associative_v<dedekind::numbers::Complex<R>,
                            dedekind::numbers::PartialEmbedRealToComplex<R>> =
        true;

}  // namespace dedekind::category

namespace dedekind::numbers {

/**
 * @brief Machine realization arrow ℝ ↪ ℂ: Real<R> → Complex<R>.
 * @details Every real x embeds as the complex number (x + 0i).
 *          This is the current machine model lift of R → C.
 */
export template <IsRealCarrier R = machine_real_scalar>
inline constexpr auto embed_ℝ_ℂ = arrow<Real<R>, Complex<R>>(
    [](const Real<R>& r) noexcept { return Complex<R>{r.resolve(), R{}}; });

/**
 * @brief Characteristic morphism for ℂ: the complex numbers.
 * Accepts native Complex<R> and all embedded predecessors
 * (Real<R>, Rational<I>, int, unsigned, Ternary).
 */
export template <typename R = machine_real_scalar,
                 IsInteger I = machine_integer, typename L = ClassicalLogic,
                 typename C = ℶ_1>
  requires IsComplexScalar<R>
struct ComplexesOf {
  using Domain = Complex<R>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native Complex<R>: always a member of ℂ
  constexpr typename L::Ω operator()(const Complex<R>&) const {
    return L::True;
  }

  // Direct parent: embed Real<R> into ℂ via the canonical arrow.
  constexpr typename L::Ω operator()(const Real<R>& r) const {
    return operator()(embed_ℝ_ℂ<R>(r));
  }

  // Delegate non-parent ancestors to ambient ℝ.
  template <typename T>
    requires(!std::same_as<T, Complex<R>> && !std::same_as<T, Real<R>>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::RealsOf<machine_real_scalar, I>{}(x);
  }
};

export using ComplexSet = ComplexesOf<>;
export using ℂ = ComplexSet;

export inline constexpr ℂ C{};

}  // namespace dedekind::numbers

namespace dedekind::category {
template <typename R>
struct SpeciesTraits<dedekind::numbers::Complex<R>> {
  using Domain = dedekind::numbers::Complex<R>;
  using machine_type = dedekind::numbers::Complex<R>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℝ_ℂ<>)>> =
        true;
}  // namespace dedekind::category
