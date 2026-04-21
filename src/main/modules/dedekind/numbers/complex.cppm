/**
 * @file dedekind/numbers/complex.cppm
 * @partition :complex
 * @brief Minimal complex wrapper for experimental reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Les mathematiciens n'etudient pas des objets, mais des relations
 * entre des objets."
 *       ("Mathematicians do not study objects, but the relations between
 * objects.")
 *       -- Henri Poincare, La Science et l'hypothese (1902)
 */
module;

#include <cmath>
#include <concepts>
#include <limits>
#include <utility>

export module dedekind.numbers:complex;

import dedekind.category;
import dedekind.geometry;
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

/**
 * @brief Squared Euclidean norm on Complex<R>: |z|^2 = re^2 + im^2.
 * @details Kept squared to avoid introducing a square root in hot paths.
 */
export template <IsComplexScalar R>
constexpr R euclidean_norm_squared(const Complex<R>& z) {
  const R re = z.real();
  const R im = z.imag();
  return (re * re) + (im * im);
}

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

namespace detail {
constexpr bool to_lattice_coordinate(
    double x, dedekind::geometry::IntegerLatticeScalar& out) {
  using Scalar = dedekind::geometry::IntegerLatticeScalar;
  constexpr double lo = static_cast<double>(std::numeric_limits<Scalar>::min());
  constexpr double hi = static_cast<double>(std::numeric_limits<Scalar>::max());
  if ((x < lo) || (x > hi)) return false;
  if (std::trunc(x) != x) return false;
  out = static_cast<Scalar>(x);
  return true;
}
}  // namespace detail

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
                 IsInteger I = default_integer, typename L = ClassicalLogic,
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

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<
                                       Complex<machine_real_scalar>>(C))>,
    "ComplexesOf must be the canonical IsSet anchor for "
    "dedekind.numbers:complex.");

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

namespace dedekind::numbers {

/**
 * @brief Canonical embedding ℤ² ↪ ℂ: (x, y) ↦ x + iy.
 *
 * @details The Gaussian integers ℤ[i] embed into ℂ via (a, b) ↦ a + bi.
 *          This is the standard lattice injection identifying the square
 *          integer grid ℤ² with ℤ[i] ⊂ ℂ.
 *          Declared monic below: distinct integer pairs yield distinct
 *          complex numbers since real() and imag() recover a and b exactly.
 */
export inline constexpr auto embed_z2_c =
    arrow<dedekind::geometry::IntegerLatticePoint2D, Complex<double>>(
        [](const dedekind::geometry::IntegerLatticePoint2D& p) noexcept {
          return Complex<double>{static_cast<double>(p.first),
                                 static_cast<double>(p.second)};
        });

/**
 * @brief Lift a Set<IntegerLatticePoint2D> (a lattice grid) to
 *        Set<Complex<double>>
 *        via the embedding embed_z2_c.
 *
 * @details A complex number z belongs to the image if and only if:
 *          (1) z has integral real and imaginary parts, and
 *          (2) the corresponding lattice point is in grid.
 *
 *          This is the canonical preimage characterisation of the image of a
 *          monic (injective) map: z ∈ embed_z2_c(grid) ↔ embed_z2_c⁻¹(z) ∈
 * grid.
 *
 * @param grid  A Set<dedekind::geometry::IntegerLatticePoint2D,
 *              ClassicalLogic, P> (e.g. from
 *              dedekind::geometry::square_integer_grid).
 * @return A Set<Complex<double>, ClassicalLogic, ...>.
 */
export template <typename L, typename P>
constexpr auto embed_grid_ℂ(
    const dedekind::sets::Set<dedekind::geometry::IntegerLatticePoint2D, L, P>&
        grid) {
  using namespace dedekind::sets;
  auto c = var<ℂ>;
  return Set{c % C | [grid](const Complex<double>& z) {
    const double re = z.real();
    const double im = z.imag();
    dedekind::geometry::IntegerLatticeScalar x =
        dedekind::geometry::IntegerLatticeScalar{0};
    dedekind::geometry::IntegerLatticeScalar y =
        dedekind::geometry::IntegerLatticeScalar{0};
    if (!detail::to_lattice_coordinate(re, x) ||
        !detail::to_lattice_coordinate(im, y))
      return false;
    using GridLogic = typename std::decay_t<decltype(grid)>::logic_species;
    return grid(dedekind::geometry::IntegerLatticePoint2D{x, y}) ==
           GridLogic::True;
  }};
}

/**
 * @brief The canonical N×N square Gaussian-integer grid as a Set<ℂ>.
 *
 * @details Combines dedekind::geometry::square_integer_grid with
 *          embed_grid_ℂ to produce the set:
 *            Λ_N = { x + iy ∈ ℂ | 0 ≤ x < n, 0 ≤ y < n, x,y ∈ ℤ }.
 *          This is the default discretization of ℂ used in numerical
 *          algorithms such as the Mandelbrot set approximation.
 *
 * @param n  Side length of the grid (number of lattice points per axis).
 * @return A Set<Complex<double>, ClassicalLogic, ...>.
 */
export constexpr auto complex_lattice(int n) {
  return embed_grid_ℂ(dedekind::geometry::square_integer_grid(n));
}

/**
 * @brief Embed a complex number into a 2-dimensional real vector.
 *
 * Formalises the identification ℂ ≅ ℝ² via z = a + bi ↦ (a, b).
 * The dimension matches HasDimension<Vector<R,2>, 2>.
 *
 * @tparam R  A floating scalar type satisfying both IsComplexScalar and
 *            IsFloatingScalar (e.g. double).
 */
export template <typename R>
  requires IsComplexScalar<R> && std::floating_point<R>
constexpr dedekind::geometry::Vector<R, 2> as_vector(const Complex<R>& z) {
  return {z.real(), z.imag()};
}

/**
 * @brief Embed a complex number into its 2×2 rotation matrix representation.
 *
 * The standard regular representation of ℂ inside M₂(ℝ) sends
 *   a + bi  ↦  [[a, -b], [b, a]]
 *
 * This matrix is orthogonal (when |z|=1) and satisfies:
 *   as_matrix(z) * as_vector(w) == as_vector(z * w)
 *
 * The map is an injective ring homomorphism ℂ → M₂(ℝ).
 *
 * @tparam R  A floating scalar type satisfying both IsComplexScalar and
 *            IsFloatingScalar (e.g. double).
 */
export template <typename R>
  requires IsComplexScalar<R> && std::floating_point<R>
constexpr dedekind::geometry::LinearMap<R, 2, 2> as_matrix(
    const Complex<R>& z) {
  return {{{z.real(), -z.imag()}, {z.imag(), z.real()}}};
}

}  // namespace dedekind::numbers

namespace dedekind::category {

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_z2_c)>> =
        true;

static_assert(
    IsMonicArrow<std::decay_t<decltype(dedekind::numbers::embed_z2_c)>>,
    "embed_z2_c must be recognised as a monic arrow.");

}  // namespace dedekind::category
