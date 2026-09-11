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
#include <functional>  // std::plus / std::multiplies in trait specialisations
#include <limits>
#include <type_traits>  // std::bool_constant for the composite exactness traits
#include <utility>

export module dedekind.numbers:complex;

import dedekind.algebra; // HasRingOperators (canonical-spine witnesses)
import dedekind.category;
import dedekind.geometry;
import dedekind.morphologies; // Modular<N> / Congruence<N,R> — the finite
                              // quotient the de Moivre node set factors through
import dedekind.sets;
import :real;
import :quadratic;  // QuadraticReal<2> — the coat-hanger ℝ carrier that the
                    // genuine ℂ = Complex<QuadraticReal<2>> is built over

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
  // Self-Domain: var<ℂ> ranges over complex numbers (per the
  // symbolic-scout factory's Variable<S>::T = S::Domain rule).
  using Domain = Complex;
  using scalar_type = R;
  using ScalarCarrier = R;

  // Real and imaginary parts as a public pair.
  // Public to satisfy IsProduct<Complex<R>, R, R> (ℂ ≅ ℝ × ℝ).
  R first;   // real part
  R second;  // imaginary part

  constexpr Complex(R re = R{}, R im = R{}) : first(re), second(im) {}

  constexpr R real() const { return first; }
  constexpr R imag() const { return second; }

  friend constexpr bool operator==(const Complex&, const Complex&) = default;

  friend constexpr Complex operator+(const Complex& a, const Complex& b) {
    return {a.first + b.first, a.second + b.second};
  }

  // Binary and unary negation — complex numbers form an additive group
  // so both ops must exist. Relied on by `dedekind.algebra::HasRingOperators`
  // witness in `:linear_algebra::embeddings`, which asserts the canonical
  // regular representation ℂ → M₂(R) is a ring homomorphism.
  friend constexpr Complex operator-(const Complex& a, const Complex& b) {
    return {a.first - b.first, a.second - b.second};
  }

  // Unary negation expressed via `R{} - x` rather than `-x`, so the op's
  // requirement set matches `IsComplexScalar<R>` exactly (which has `R{}`
  // and binary `-` but does not mandate unary `-` on R).
  friend constexpr Complex operator-(const Complex& a) {
    return {R{} - a.first, R{} - a.second};
  }

  friend constexpr Complex operator*(const Complex& a, const Complex& b) {
    return {(a.first * b.first) - (a.second * b.second),
            (a.first * b.second) + (a.second * b.first)};
  }

  // Explicit scalar action R × Complex<R> → Complex<R> (and the
  // symmetric form), matching Vec2V<T>'s scalar-action pattern.  The
  // implicit-conversion path R → Complex<R>{r, 0} → Complex<R>×Complex<R>
  // exists, but nested requires-expressions in the IsAction concept
  // don't discover the friend operator* through ADL on the dependent
  // friend, so we expose the action explicitly.
  friend constexpr Complex operator*(const R& s, const Complex& a) {
    return {s * a.first, s * a.second};
  }
  friend constexpr Complex operator*(const Complex& a, const R& s) {
    return {a.first * s, a.second * s};
  }

  /**
   * @brief Division via the conjugate identity: `z / w = z · w̄ / |w|²`.
   *
   *   (a + bi) / (c + di) = [(ac + bd) + (bc − ad)i] / (c² + d²).
   *
   * Requires `operator/` on the scalar `R`. Defined when `|w|² ≠ 0`; for
   * the zero-division case the behaviour is inherited from `R / R` (e.g.
   * `Rational<Z>` throws / asserts).
   *
   * Lifts `Complex<R>` from `HasRingOperators` into `HasFieldOperators`
   * (the `a/b → S` clause), which in turn lets `Dual<Complex<R>>` admit
   * `operator/` for holomorphic forward-mode automatic differentiation
   * at compile time.
   */
  friend constexpr Complex operator/(const Complex& a, const Complex& b) {
    const R denom = b.first * b.first + b.second * b.second;
    return {(a.first * b.first + a.second * b.second) / denom,
            (a.second * b.first - a.first * b.second) / denom};
  }
};

/**
 * @brief Complex conjugate: conj(a + bi) = a − bi.
 *
 * @details The canonical ring involution of ℂ = ℝ[i]/(i²+1) fixing the real
 * subfield {im = 0}.  Previously lived only inline inside `operator/`
 * (z / w = z · conj(w) / |w|²); exported here as a first-class library
 * primitive because two downstream uses ride it: the norm identity
 * |z|² = Re(z · conj z) (witnessed below) and the even-spectrum symmetry
 * c_k = conj(c_{−k}) that the Figure-5 analytic symmetry check rests on.
 *
 * FIXME(#808): register as an `arrow<Complex<R>, Complex<R>>` carrying
 * `is_involution_v` (@c category:involution) once the Spectrum<(ℤ/N)², ·>
 * even-check (Tier 2) needs the arrow form; kept a plain function for now.
 */
export template <IsComplexScalar R>
constexpr Complex<R> conj(const Complex<R>& z) {
  return {z.real(), R{} - z.imag()};
}

/**
 * @brief The real inner product on ℂ ≅ ℝ²: dot(z, w) = Re(z · conj w)
 *        = re_z·re_w + im_z·im_w.
 *
 * @details The Hermitian form read through the ℝ²-realification of ℂ (scalar
 * field R): the inner-product form of ℂ.  With @c abs2 it holds for @b every
 * @c IsComplexScalar R (exact ℚ(√2) included); adding @c norm (on floating R)
 * completes the full @c HasInnerProduct / @c IsInnerProductSpace surface for
 * @c Complex<double>, while the exact ℂ(ℚ(√2)) keeps the exact @c abs2 without
 * a
 * @c norm.  @c abs2 and the Euclidean escape-test norm both derive from it,
 * replacing the former bespoke |z|² = re²+im² formula with the
 * inner-product-induced one.  The codomain is R, not
 * @c Complex<R>: the sesquilinear ℂ-valued form z·conj w is a richer layer that
 * would break the R-typed norm consumers (cf. the Mandelbrot escape radius),
 * so the realification is the default that slots into the existing metric.
 */
export template <IsComplexScalar R>
constexpr R dot(const Complex<R>& z, const Complex<R>& w) {
  return (z.real() * w.real()) + (z.imag() * w.imag());
}

/**
 * @brief Squared norm abs2(z) = <z, z> = |z|² = re² + im²: exact, √-free.
 * @details The @c HasInnerProduct primitive for ℂ, available for every
 * @c IsComplexScalar R --- including the exact, non-√-closed ℝ = ℚ(√2), where
 * @c norm does not exist but @c abs2 does.  Equals Re(z · conj z) (pinned
 * below).
 */
export template <IsComplexScalar R>
constexpr R abs2(const Complex<R>& z) {
  return dot(z, z);
}

/**
 * @brief Squared Euclidean norm on Complex<R>: a synonym for @c abs2, kept so
 * it stays reachable by ADL in @c dedekind::numbers for callers that do not
 * import @c dedekind.geometry (e.g. the Mandelbrot escape test and the
 * set-algebra predicates).  Kept squared to avoid a square root in hot paths.
 */
export template <IsComplexScalar R>
constexpr R euclidean_norm_squared(const Complex<R>& z) {
  return abs2(z);
}

/**
 * @brief Induced norm ||z|| = sqrt(<z, z>), for @b floating carriers only.
 * @details Completes @c HasInnerProduct<Complex<F>, F> on the materialisable
 * floating ℂ.  The exact coat-hanger ℂ(ℚ(√2)) deliberately has no @c norm ---
 * its field is not closed under √ --- only the exact @c abs2.
 */
export template <IsComplexScalar R>
  requires std::floating_point<R>
constexpr R norm(const Complex<R>& z) {
  return std::sqrt(abs2(z));
}

/** @section complex__Partial_Arithmetic_with_Ternary_Logic */

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
 * @brief Embedding transform: ℝ_d ↪ ℂ (Real<R> → Complex<R>) with Ternary
 *        acknowledgment.
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

// ---------------------------------------------------------------------------
// Complex<R> = R[i]/(i²+1) is a rig / commutative ring (a FIELD when x²+1 is
// irreducible over R), its rig traits lifted by PROPAGATION from R.  So the
// exact coat-hanger ℂ = Complex<ℚ(√2)> is a certified semiring, while
// Complex<double> is correctly NOT associative (IEEE; that opt-in stays with
// ieee::IEEE<F>).  This lets ℂ satisfy category::IsSemiring, so the semiring
// bra-ket inner_product ⟨·|·⟩ (linear_algebra:transfer) works over exact ℂ.
// (The traits are lifted per-trait rather than via quotient_algebra_base ---
// ℂ already carries its own IsQuotientAlgebra registration in the HSP-legs
// block below, and a second quotient_algebra_base base would conflict.)
template <typename R>
struct is_exact_total<dedekind::numbers::Complex<R>,
                      std::plus<dedekind::numbers::Complex<R>>>
    : is_exact_total<R, std::plus<R>> {};
// ℂ's × = (ac−bd, ad+bc) uses R's × AND R's +/−, so exact-total × needs BOTH
// of R's additive and multiplicative exactness (mult exactness alone is not
// enough — a carrier with exact × but inexact + must not be certified).
template <typename R>
struct is_exact_total<dedekind::numbers::Complex<R>,
                      std::multiplies<dedekind::numbers::Complex<R>>>
    : std::bool_constant<is_exact_total<R, std::multiplies<R>>::value &&
                         is_exact_total<R, std::plus<R>>::value> {};

template <typename R>
struct is_associative<dedekind::numbers::Complex<R>,
                      std::plus<dedekind::numbers::Complex<R>>>
    : is_associative<R, std::plus<R>> {};
template <typename R>
struct is_associative<dedekind::numbers::Complex<R>,
                      std::multiplies<dedekind::numbers::Complex<R>>>
    : is_associative<R, std::multiplies<R>> {};
template <typename R>
struct is_commutative<dedekind::numbers::Complex<R>,
                      std::plus<dedekind::numbers::Complex<R>>>
    : is_commutative<R, std::plus<R>> {};
template <typename R>
struct is_commutative<dedekind::numbers::Complex<R>,
                      std::multiplies<dedekind::numbers::Complex<R>>>
    : is_commutative<R, std::multiplies<R>> {};

template <typename R>
inline constexpr bool
    is_distributive_v<dedekind::numbers::Complex<R>,
                      std::multiplies<dedekind::numbers::Complex<R>>,
                      std::plus<dedekind::numbers::Complex<R>>> =
        is_distributive_v<R, std::multiplies<R>, std::plus<R>>;

template <typename R>
struct identity_trait<dedekind::numbers::Complex<R>,
                      std::plus<dedekind::numbers::Complex<R>>> {
  using value_type = dedekind::numbers::Complex<R>;
  static constexpr value_type value{};  // 0 = 0 + 0i
};
template <typename R>
struct identity_trait<dedekind::numbers::Complex<R>,
                      std::multiplies<dedekind::numbers::Complex<R>>> {
  using value_type = dedekind::numbers::Complex<R>;
  static constexpr value_type value{R{1}, R{}};  // 1 = 1 + 0i
};

template <typename R>
inline constexpr bool
    is_invertible_v<dedekind::numbers::Complex<R>,
                    std::plus<dedekind::numbers::Complex<R>>> =
        true;  // additive inverse −z always exists (Complex<R> is a ring)

template <typename R>
struct inverse_trait<dedekind::numbers::Complex<R>,
                     std::plus<dedekind::numbers::Complex<R>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::numbers::Complex<R>;
  static constexpr value_type compute(const value_type& z) { return -z; }
};

// FIELD-ness does NOT lift uniformly --- the discriminant classification.
// Complex<R> = R[i]/(i²+1) is a field iff x²+1 is IRREDUCIBLE over R, i.e. R is
// FORMALLY REAL (−1 is not a square).  This is NOT implied by
// std::totally_ordered<R> alone: a carrier can be totally ordered by its
// representatives yet have −1 a square (e.g. 𝔽₅: −1 = 4 = 2²), which splits
// Complex<𝔽₅> into zero divisors.  So the multiplicative inverse is registered
// only for the SUPPORTED formally-real bases --- the real quadratic fields
// ℚ(√D) (D>1) --- rather than by a blanket order gate.  z⁻¹ = conj(z)/|z|² (the
// is_invertible_v convention excludes z=0).  This provides the actual inverse,
// so category::IsField<Complex<ℚ(√D)>> holds through the full chain, not just
// is_invertible_v.  Other bases (Complex<double> — excluded by associativity;
// Complex<Complex<·>>; 𝔽₅-like carriers) get NO field certificate.  Parabolic
// sibling: Dual<F> = F[ε]/(ε²) registers no multiplicative inverse (ε
// nilpotent) --- a ring, never a field.
template <long D, typename Q>
struct inverse_trait<
    dedekind::numbers::Complex<dedekind::numbers::QuadraticReal<D, Q>>,
    std::multiplies<
        dedekind::numbers::Complex<dedekind::numbers::QuadraticReal<D, Q>>>> {
  static constexpr bool exists = true;
  using value_type =
      dedekind::numbers::Complex<dedekind::numbers::QuadraticReal<D, Q>>;
  static constexpr value_type compute(const value_type& z) {
    using Rq = dedekind::numbers::QuadraticReal<D, Q>;
    return dedekind::numbers::conj(z) /
           value_type{dedekind::numbers::abs2(z), Rq{}};  // conj(z)/|z|²
  }
};

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
 * @brief Machine realization arrow ℝ_d ↪ ℂ: Real<R> → Complex<R>.
 * @details Every real x embeds as the complex number (x + 0i).
 *          This is the current machine model lift of R → C.
 */
export template <IsRealCarrier R = machine_real_scalar>
inline constexpr auto embed_ℝ_d_ℂ = arrow<Real<R>, Complex<R>>(
    [](const Real<R>& r) noexcept { return Complex<R>{r.resolve(), R{}}; });

/**
 * @brief The Birkhoff @b S leg @f$\mathbb{R}\hookrightarrow\mathbb{C}@f$ over
 * the
 *        @b coat-hanger: @c QuadraticReal<2> → @c Complex<QuadraticReal<2>>,
 *        @f$r\mapsto (r,0)@f$.
 *
 * @details A genuine monic @b ring embedding (an @c EmbedsAsSubalgebra S-leg,
 * the ℂ sibling of @c embed_ℚ_ℝ): ℝ is the real subfield
 * @f$\{\,\mathrm{im}=0\,\}
 * \subset\mathbb{C}@f$.  Distinct from the machine @c embed_ℝ_d_ℂ
 * (@c Real<double> → @c Complex<double>); this is the coat-hanger arrow
 * @f$r\mapsto r+0i@f$, witnessed by computation below.  Not registered
 * @c is_monotone_v --- ℂ carries no total order (that is exactly what the
 * quotient by @f$(i^2+1)@f$ forfeits vs. ℝ). */
export inline constexpr auto embed_ℝ_ℂ =
    arrow<QuadraticReal<2>, Complex<QuadraticReal<2>>>(
        [](const QuadraticReal<2>& r) noexcept {
          return Complex<QuadraticReal<2>>{r, QuadraticReal<2>{}};
        });

/** @section complex__Roots_of_Unity_de_Moivre
 *
 * @brief The exact de Moivre exponential @f$\zeta_8 : \mathbb{Z}/8 \to
 * \mathbb{C}^\times@f$ over the coat-hanger ℝ = ℚ(√2):
 * @f$k \mapsto \zeta_8^{\,k} = (\cos\tfrac{2\pi k}{8},\ \sin\tfrac{2\pi
 * k}{8})@f$.
 *
 * @details The arithmetic heart of an 8-point DFT, made @b exact by the
 * cyclotomic coincidence @f$\sqrt 2 = \zeta_8 + \zeta_8^{-1}@f$: hence
 * ℚ(√2) = ℝ ∩ ℚ(ζ₈) already contains every 8th root of unity, and the sole
 * irrational the table needs is ½√2 = cos(π/4).  The map is a group
 * homomorphism @f$(\mathbb{Z}/8, +) \to (\mathbb{C}^\times, \cdot)@f$ --- angle
 * addition @b is exponent addition, de Moivre as a ring identity (witnessed
 * below).  Implemented via the half-turn fold @f$\zeta_8^4 = -1@f$ (@c k =
 * 4q+r) so the whole μ₈ table --- and its real-zero set --- are shallow @b
 * compile-time facts, not the deep iterated-multiplication chain that would
 * exhaust the constant-evaluation budget.
 *
 * @b Library @b fact (the Figure-5 anchor): the cosine-node set
 * @f$\{k : \operatorname{Re}\zeta_8^k = 0\}@f$ is @b exactly the residue class
 * @f$2 \pmod 4@f$ --- a finite-quotient predicate
 * @c morphologies::Congruence<4,2>.  So the ℂ(ℚ(√2)) node test factors through
 * the finite quotient @c Modular<4>; the strength reduction that keeps
 * materialisation on a cheap integer surrogate is this residue-class
 * identity, not insight poured into the exhibit. */
export inline constexpr auto root8 =
    arrow<dedekind::morphologies::Modular<8u>, Complex<QuadraticReal<2>>>(
        [](const dedekind::morphologies::Modular<8u>& k) noexcept {
          using Rz = QuadraticReal<2>;
          const Rz s = Rz::of(Rational<default_integer>{},
                              Rational<default_integer>{1, 2});  // ½√2
          const Complex<Rz> table[4] = {
              {Rz{1}, Rz{}},  // ζ⁰ = 1
              {s, s},         // ζ¹ = ½√2 + ½√2·i
              {Rz{}, Rz{1}},  // ζ² = i
              {Rz{} - s, s},  // ζ³ = −½√2 + ½√2·i
          };
          const unsigned r = k.value & 3u;               // k mod 4
          const bool neg = ((k.value >> 2) & 1u) != 0u;  // ζ⁴ = −1 half-turn
          const Complex<Rz> z = table[r];
          return neg ? -z : z;
        });

/** @brief The primitive 8th root of unity ζ8 = e^{2πi/8} = ½√2 + ½√2·i, exact
 * in ℂ(ℚ(√2)) --- the generator of the de Moivre map @c root8. */
export inline constexpr Complex<QuadraticReal<2>> ζ8 =
    root8(dedekind::morphologies::Modular<8u>{1});

/** @section complex__Roots_of_Unity_Witnesses
 *  @c root8 is the exact de Moivre homomorphism, @b computed.  Witnesses are
 *  kept shallow: each @c root8 call folds a QuadReal table, so the loops below
 *  stay small (primitivity, node set) and the homomorphism is pinned on
 *  representative pairs; the exhaustive 64-pair sweep is a runtime check in the
 *  @c roots_of_unity exhibit. */
namespace {
using M8_ru = dedekind::morphologies::Modular<8u>;
using R2_ru = QuadraticReal<2>;
using Cx_ru = Complex<QuadraticReal<2>>;

static_assert(root8(M8_ru{2}) == Cx_ru{R2_ru{}, R2_ru{1}}, "ζ₈² = i.");
static_assert(root8(M8_ru{4}) == -Cx_ru{R2_ru{1}, R2_ru{}}, "ζ₈⁴ = −1.");
static_assert(root8(M8_ru{0}) == Cx_ru{R2_ru{1}, R2_ru{}}, "ζ₈⁰ = 1.");
static_assert(ζ8 == Cx_ru{R2_ru::of(Rational<default_integer>{},
                                    Rational<default_integer>{1, 2}),
                          R2_ru::of(Rational<default_integer>{},
                                    Rational<default_integer>{1, 2})},
              "ζ8 = ½√2 + ½√2·i.");

// Primitivity: no proper power (1..7) of ζ₈ is 1 (ζ₈⁸ = ζ₈⁰ = 1 by ℤ/8 wrap).
static_assert(
    [] {
      for (unsigned k = 1; k < 8u; ++k)
        if (root8(M8_ru{k}) == Cx_ru{R2_ru{1}, R2_ru{}}) return false;
      return true;
    }(),
    "ζ₈ is a primitive 8th root: no proper power equals 1.");

// De Moivre homomorphism (ℤ/8, +) → (ℂˣ, ·): ζ^a · ζ^b = ζ^{a+b}.  Pinned on
// representative pairs at COMPILE TIME (kept shallow --- each root8 call is a
// QuadReal-heavy fold, so the exhaustive 64-pair sweep is a runtime check in
// the roots_of_unity exhibit, not a module-compile static_assert): a
// non-wrapping pair, and two that wrap around ℤ/8.
static_assert(root8(M8_ru{1}) * root8(M8_ru{2}) == root8(M8_ru{3}),
              "ζ¹·ζ² = ζ³ (no wrap).");
static_assert(root8(M8_ru{5}) * root8(M8_ru{5}) == root8(M8_ru{2}),
              "ζ⁵·ζ⁵ = ζ^{10 mod 8} = ζ² (wrap).");
static_assert(root8(M8_ru{3}) * root8(M8_ru{5}) == root8(M8_ru{0}),
              "ζ³·ζ⁵ = ζ^{8 mod 8} = ζ⁰ = 1 (wrap to identity).");

// The library fact: the cosine-node set {k : Re ζ₈^k = 0} IS the residue class
// 2 (mod 4) = morphologies::Congruence<4,2>.  This is the strength-reduction
// anchor, certified at compile time (the node set factors through Modular<4>).
static_assert(
    [] {
      for (unsigned k = 0; k < 8u; ++k)
        if ((root8(M8_ru{k}).real() == R2_ru{}) !=
            dedekind::morphologies::Congruence<4, 2>{}(k))
          return false;
      return true;
    }(),
    "cosine-node set of the 8-point kernel is Congruence<4,2> (k ≡ 2 mod 4).");
}  // namespace

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
    return operator()(embed_ℝ_d_ℂ<R>(r));
  }

  // Delegate non-parent ancestors to ambient ℝ.
  template <typename T>
    requires(!std::same_as<T, Complex<R>> && !std::same_as<T, Real<R>>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::RealsOf<machine_real_scalar, I>{}(x);
  }
};

export using ComplexSet = ComplexesOf<>;

/** @brief The canonical complex-number universe ℂ =
 * Ω<Complex<QuadraticReal<2>>, ClassicalLogic, ℶ_1> — the coat-hanger
 * ℂ = Cplx(ℝ) over the genuine ℝ = ℚ(√2) (mirroring ℝ and 𝔻).
 *
 *  @details Per #559's chosen direction (option A): the named species
 *  symbols denote @b universe values (constexpr instances of
 *  @c UniversalSet over the carrier), not classifier-alias types.  All
 *  seven species symbols (@c 𝔹, @c ℕ, @c ℤ, @c ℚ, @c ℝ, @c ℂ, @c 𝔻)
 *  carry the canonical @c element<ℂ> scout spelling.
 *
 *  Post-HSP retarget: the carrier of @c ℂ is @c Complex<QuadraticReal<2>> ---
 *  the 2nd-order quotient ℝ[i]/(i²+1) over the coat-hanger ℝ, NOT
 *  @c Complex<double>.  Machine-double complex lives on the materialisable
 *  ambient @c ℂ_d = Ω<Complex<machine_real_scalar>> below (mirroring
 *  ℝ_d / 𝔻_d).  The classifier (multi-overload cross-carrier @c operator()
 *  that delegates ℝ_d-side arguments through @c embed_ℝ_d_ℂ and lands non-
 *  parent ancestors via @c RealsOf<>) is reachable via @c ComplexSet
 *  @c = @c ComplexesOf<>.
 *
 *  Cardinality is set explicitly to @c ℶ_1 (continuum) — the textbook
 *  cardinality of ℂ, matching ℝ — overriding the @c Ω<...> variable
 *  template's @c ℵ_0 default.  ℂ is in bijection with ℝ × ℝ and
 *  therefore shares ℝ's continuum cardinality.
 *
 *  Textbook construction: ℂ = ℝ[i]/(i²+1) --- the H-leg witnessed below via
 *  @c quotient_algebra_base<Complex<R>> = R (the sibling of 𝔻 = ℝ[ε]/(ε²)).
 */
export inline constexpr auto ℂ =
    dedekind::sets::Ω<Complex<QuadraticReal<2>>, ClassicalLogic, ℶ_1>;

static_assert(
    std::same_as<std::remove_cvref_t<decltype(ℂ)>,
                 dedekind::sets::UniversalSet<Complex<QuadraticReal<2>>,
                                              ClassicalLogic, ℶ_1>>,
    "ℂ is the universe Ω<Complex<QuadraticReal<2>>, ClassicalLogic, ℶ_1> — the "
    "coat-hanger ℂ = Cplx(ℝ) over the genuine ℝ = ℚ(√2), mirroring "
    "ℝ = Ω<QuadraticReal<2>> (#806).  Not Complex<double>.");
static_assert(
    std::same_as<typename std::remove_cvref_t<decltype(ℂ)>::Domain,
                 Complex<QuadraticReal<2>>>,
    "ℂ's carrier IS Complex<QuadraticReal<2>> — the 2nd-order quotient "
    "ℝ[i]/(i²+1) over the coat-hanger ℝ.");

/** @brief The materialisable machine ambient @c ℂ_d = @c Ω<Complex<double>>,
 *  mirroring @c ℝ_d.  Machine-double complex work (showcases, Mandelbrot,
 *  benchmarks, the Python facade) lives here, exactly as @c double reals moved
 *  from @c ℝ to @c ℝ_d in #806.  The abstract @c ℂ is the coat-hanger. */
export inline constexpr auto ℂ_d =
    dedekind::sets::Ω<Complex<machine_real_scalar>, ClassicalLogic, ℶ_1>;
static_assert(
    std::same_as<typename std::remove_cvref_t<decltype(ℂ_d)>::Domain,
                 Complex<machine_real_scalar>>,
    "ℂ_d's carrier is Complex<machine_real_scalar> (machine ambient).");

export inline constexpr ComplexSet C{};

}  // namespace dedekind::numbers

namespace dedekind::category {
template <typename R>
struct SpeciesTraits<dedekind::numbers::Complex<R>> {
  using Domain = dedekind::numbers::Complex<R>;
  using machine_type = dedekind::numbers::Complex<R>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℝ_d_ℂ<>)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_ℝ_d_ℂ<>)>>,
    "embed_ℝ_d_ℂ (ℝ_d ↪ ℂ) is registered injective.");

// The coat-hanger S-leg ℝ ↪ ℂ is monic (r ↦ (r,0) is injective).
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℝ_ℂ)>> =
        true;
}  // namespace dedekind::category

namespace dedekind::algebra {
// The coat-hanger S-leg ℝ ↪ ℂ is a ring homomorphism (r ↦ (r,0) preserves
// +,×,0,1) — backed by the computed witnesses below.
template <>
inline constexpr bool
    is_homomorphism_v<std::decay_t<decltype(dedekind::numbers::embed_ℝ_ℂ)>> =
        true;
}  // namespace dedekind::algebra

namespace dedekind::numbers {

// Functor identification: Complex<R> = Cplx(R).  The Cplx functor
// (quotient ring R[i]/(i² + 1); cf. Lang §III.1) takes a commutative
// ring R to the ring extension by a square root of -1; for any
// IsComplexScalar R, Complex<R> IS that quotient ring.  The structural
// type of the result depends on how i² + 1 sits in R[x]:
//   - irreducible over R → R[i]/(i² + 1) is an integral-domain
//     extension (a field when R itself is a field);
//   - reducible into distinct linear factors over R → semisimple
//     decomposition R[i]/(i² + 1) ≅ R × R (e.g., over R = ℂ);
//   - reducible into a repeated factor (i.e., (x - α)² in R[x]) →
//     nilpotent quotient with non-trivial radical (e.g., R = 𝔽_2,
//     where x² + 1 = (x + 1)²).
// The "nilpotent" branch is the @em not-square-free case specifically,
// not "any reducible case" — semisimple-decomposition reductions
// don't produce nilpotents (they produce idempotents).  The
// ScalarCarrier alias is the source-side projection of the Cplx
// functor, mechanically aligning the §2 paper paragraph
// ("Named functors that build the library's carriers") with source.
//
// FIXME(#498/NEW-A): same naming-convention question as
// Rational<I>::IntegerCarrier and Real<Q>::ScalarCarrier — see the
// FIXME there.  Aligning IntegerCarrier / ScalarCarrier / value_type
// with :functor's Σ_cat / Τ_cat / Shape<U> convention is NEW-A
// trait-registry work.
static_assert(std::same_as<typename Complex<double>::ScalarCarrier, double>,
              "Complex<R> is the Cplx-functor image of R; ScalarCarrier "
              "names R mechanically.");

}  // namespace dedekind::numbers

// ---------------------------------------------------------------------------
// Quotient-algebra registration for Complex<R> (#498/#499 NEW-A).
//
// Complex<R> = R[i]/(i² + 1) is a polynomial-quotient construction.
// The single declaration below — `quotient_algebra_base<Complex<R>>::type
// = R` — fires the structural-trait propagation through
// `dedekind.algebra:quotient`: associativity, commutativity,
// distributivity, and the full IsTotal disjunction (periodic /
// idempotent / saturating) all lift from R to Complex<R> uniformly.
// The carrier-specific bits (additive identity, additive inverse) live
// next to it as identity_trait / inverse_trait specialisations.
// ---------------------------------------------------------------------------

namespace dedekind::category {

template <dedekind::numbers::IsComplexScalar R>
struct quotient_algebra_base<dedekind::numbers::Complex<R>> {
  using type = R;
};

template <dedekind::numbers::IsComplexScalar R>
struct identity_trait<dedekind::numbers::Complex<R>,
                      std::plus<dedekind::numbers::Complex<R>>> {
  using value_type = dedekind::numbers::Complex<R>;
  static constexpr value_type value = value_type{R{}, R{}};
};

template <dedekind::numbers::IsComplexScalar R>
struct identity_trait<dedekind::numbers::Complex<R>,
                      std::multiplies<dedekind::numbers::Complex<R>>> {
  using value_type = dedekind::numbers::Complex<R>;
  static constexpr value_type value = value_type{R{1}, R{}};
};

template <dedekind::numbers::IsComplexScalar R>
inline constexpr bool is_invertible_v<
    dedekind::numbers::Complex<R>, std::plus<dedekind::numbers::Complex<R>>> =
    true;

template <dedekind::numbers::IsComplexScalar R>
struct inverse_trait<dedekind::numbers::Complex<R>,
                     std::plus<dedekind::numbers::Complex<R>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::numbers::Complex<R>;
  static constexpr value_type compute(
      const dedekind::numbers::Complex<R>& z) noexcept {
    return -z;
  }
};

}  // namespace dedekind::category

namespace dedekind::numbers {

// NEW-A trait registry witness (#498/#499): @c Complex<R> is a module
// over its @c ScalarCarrier @c R (textbook reading: the quotient ring
// @c R[i]/(i² + 1) carries the canonical @c R-action by component-
// wise multiplication on the @c (real, imag) pair).  The witness
// fires through the quotient-algebra propagation in
// @c dedekind.algebra:quotient.
static_assert(dedekind::algebra::is_module_v<Complex<Rational<default_integer>>,
                                             Rational<default_integer>>,
              "Complex<ℚ> is a module over ℚ.");

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
  // FIXME(#399 slice 4-6): once ℂ becomes a carrier alias for
  // Complex<...>, switch to @c element<Ω<ℂ>>; for now ℂ is still the
  // predicate-set type.
  auto c = element<Ω<Complex<double>>>;
  return Set{c | [grid](const Complex<double>& z) {
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
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_z2_c)>>,
    "embed_z2_c (ℤ² ↪ ℂ) is registered injective.");

// Structural product proof: ℂ ≅ ℝ × ℝ (or more generally S × S for any
// carrier).
static_assert(
    dedekind::category::IsProduct<dedekind::numbers::Complex<double>, double,
                                  double>,
    "Complex<double> must satisfy IsProduct<Complex<R>, R, R> (ℂ ≅ ℝ × ℝ).");

// Proof over the exact real: ℂ over ExactReal is also a product.
static_assert(
    dedekind::category::IsProduct<
        dedekind::numbers::Complex<dedekind::numbers::ExactReal<>>,
        dedekind::numbers::ExactReal<>, dedekind::numbers::ExactReal<>>,
    "Complex<ExactReal<>> must satisfy IsProduct (ℂ ≅ ℝ × ℝ over ℚ-based ℝ).");

// ── The HSP legs of the coat-hanger ℂ = Cplx(ℝ) = ℝ[i]/(i²+1), ℝ = ℚ(√2)
// ────── P (product): ℂ ≅ ℝ × ℝ as a set/module.
static_assert(
    dedekind::category::IsProduct<
        dedekind::numbers::Complex<dedekind::numbers::QuadraticReal<2>>,
        dedekind::numbers::QuadraticReal<2>,
        dedekind::numbers::QuadraticReal<2>>,
    "P-leg: the coat-hanger ℂ ≅ ℝ × ℝ (Complex<QuadReal<2>> IsProduct).");
// S (subalgebra): ℝ ↪ ℂ is a monic ring embedding, r ↦ (r,0).
static_assert(
    dedekind::algebra::EmbedsAsSubalgebra<
        std::decay_t<decltype(dedekind::numbers::embed_ℝ_ℂ)>>,
    "S-leg: ℝ ↪ ℂ (embed_ℝ_ℂ) is a Birkhoff S-leg — a monic ring embedding.");
// H (quotient): ℂ = ℝ[i]/(i²+1) is a quotient algebra over ℝ, via the
// type-pointer quotient path (quotient_algebra_base<Complex<R>> = R; per #803
// Complex uses IsQuotientAlgebra law-propagation, not the machine-carrier
// IsCongruenceQuotient).
static_assert(
    dedekind::category::IsQuotientAlgebra<
        dedekind::numbers::Complex<dedekind::numbers::QuadraticReal<2>>>,
    "H-leg: the coat-hanger ℂ = ℝ[i]/(i²+1) is a quotient algebra over ℝ.");

}  // namespace dedekind::category

namespace dedekind::numbers {

/** @section complex__ℝ_ℂ_S_Leg_Witnesses
 *  The S-leg ℝ ↪ ℂ (r ↦ (r,0)) is a genuine ring homomorphism, @b computed. */
namespace {
using R2_cx = QuadraticReal<2>;
using C2_cx = Complex<QuadraticReal<2>>;
constexpr R2_cx a_cx = R2_cx{2};
constexpr R2_cx b_cx = R2_cx{3};
static_assert(embed_ℝ_ℂ(a_cx + b_cx) == embed_ℝ_ℂ(a_cx) + embed_ℝ_ℂ(b_cx),
              "ℝ ↪ ℂ preserves +.");
static_assert(embed_ℝ_ℂ(a_cx* b_cx) == embed_ℝ_ℂ(a_cx) * embed_ℝ_ℂ(b_cx),
              "ℝ ↪ ℂ preserves ×.");
static_assert(embed_ℝ_ℂ(R2_cx{}) == C2_cx{}, "ℝ ↪ ℂ preserves 0.");
static_assert(embed_ℝ_ℂ(R2_cx{1}) == C2_cx{R2_cx{1}, R2_cx{}},
              "ℝ ↪ ℂ preserves 1.");
static_assert(a_cx != b_cx && embed_ℝ_ℂ(a_cx) != embed_ℝ_ℂ(b_cx),
              "ℝ ↪ ℂ is injective (monic).");
static_assert(embed_ℝ_ℂ(a_cx).imag() == R2_cx{},
              "image of ℝ ↪ ℂ lies in the real subfield {im = 0} ⊂ ℂ.");

// conj is the ring involution of ℂ, and the norm is the conjugate-product:
// computed over the exact ℂ = ℂ(ℚ(√2)).
constexpr C2_cx z_cx{a_cx, b_cx};
static_assert(conj(conj(z_cx)) == z_cx,
              "conj is an involution: conj∘conj = id.");
static_assert(conj(z_cx) == C2_cx{a_cx, R2_cx{} - b_cx},
              "conj(a + bi) = a − bi.");
static_assert(
    euclidean_norm_squared(z_cx) == (z_cx * conj(z_cx)).real(),
    "|z|² = Re(z · conj z): the squared norm is the conjugate-product.");
static_assert((z_cx * conj(z_cx)).imag() == R2_cx{},
              "z · conj z is real (imaginary part vanishes).");

// abs2 = ⟨z,z⟩ = euclidean_norm_squared, and it IS Re(z · conj z): the bespoke
// |z|² formula is now the inner-product-induced one.
static_assert(dot(z_cx, z_cx) == abs2(z_cx), "abs2(z) = ⟨z, z⟩.");
static_assert(abs2(z_cx) == euclidean_norm_squared(z_cx),
              "euclidean_norm_squared is the abs2 synonym.");
static_assert(abs2(z_cx) == (z_cx * conj(z_cx)).real(),
              "abs2(z) = Re(z · conj z).");

// ℂ over a floating carrier is a genuine inner-product space (dot + abs2 +
// norm); the EXACT coat-hanger ℂ(ℚ(√2)) has abs2 but no norm (no √-closure), so
// it is not HasInnerProduct --- the exact/materialisable split, pinned.
static_assert(
    dedekind::geometry::HasInnerProduct<Complex<double>, double>,
    "ℂ over double has the full inner-product surface (dot, abs2, norm).");
static_assert(dedekind::geometry::IsInnerProductSpace<Complex<double>, double>,
              "ℂ over double IS an inner-product space (Hilbert's example).");
static_assert(
    !dedekind::geometry::HasInnerProduct<Complex<QuadraticReal<2>>,
                                         QuadraticReal<2>>,
    "exact ℂ(ℚ(√2)) exposes abs2 but not norm (ℚ(√2) is not √-closed).");
}  // namespace

/** @section complex__Canonical_Species_Spine (ℂ)
 *
 * The canonical complex-number species ℂ is defined above as
 * @c ComplexSet @c = @c ComplexesOf<> with value-level constant
 * @c C; @c Complex<S> implements ℂ via the Cayley--Dickson
 * construction over any scalar ring @c S.  The spine witnesses pin
 * ℂ's syntax / semantics / arrow fabric:
 *
 * (1) IsSet anchor on @c C (above).
 * (2) Syntax: @c HasRingOperators<Complex<R>> for any @c
 *     HasRingOperators @c R --- ℂ's ring operator surface lifts
 *     elementwise (binary +, binary -, unary -, *).  The unary
 *     negation surface is @c R{} @c - @c x at the callsite;
 *     @c Complex<R> does not mandate unary @c - on its scalar
 *     carrier @c R.
 * (3) Semantics: @c IsProduct<Complex<R>, R, R> witnesses (above);
 *     the regular representation @c ℂ @c ↪ @c M_2(R) is in
 *     @c dedekind.linear_algebra:embeddings as
 *     @c IsRingHomomorphism (renamed under PR #394's retire-Like
 *     sweep).
 * (4) Primitive-type arrow: @c std::complex<double> ↔ ℂ is not
 *     yet shipped --- a future @c embed_std_complex would close
 *     the loop, but the current path is to construct
 *     @c Complex<double>{re, im} directly.
 * (5) Adjacent-set arrow: ℝ_d ↪ ℂ via @c embed_ℝ_d_ℂ above
 *     (registered monic); reverse projections @c .real() / @c
 *     .imag() live on the carrier as accessors.  Higher: ℂ ↪ ℍ
 *     (quaternions) via @c Quaternion<R>'s zero-imaginary lift
 *     (see @c :quaternion).
 */
static_assert(dedekind::algebra::HasRingOperators<Complex<double>>,
              "Complex<double> closes the literal ring operator surface.");
static_assert(dedekind::algebra::HasRingOperators<Complex<ExactReal<>>>,
              "Complex<ExactReal<>> --- the exact ℂ carrier --- closes the "
              "literal ring operator surface.");

}  // namespace dedekind::numbers
