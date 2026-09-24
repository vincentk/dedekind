#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

namespace {

// Post-HSP retarget: ℂ is the COAT-HANGER universe value
// 𝔸<Complex<QuadraticReal<2>>, Boole, ℶ_1>, so these ℂ showcases run
// on EXACT ℚ(√2) arithmetic.  The comprehensions spell the node explicitly as
// @c Comprehension{ℂ, pred} (and @c Comprehension{ℝ_d, pred}); the ambient is
// the base and no scout is needed to disambiguate from set-union.  ℝ itself is
// the ℚ(√2) coat-hanger, so Real<double> live on the materialisable ambient
// ℝ_d = 𝔸<Real<double>, Boole, ℶ_1>.  (A plain 𝔸<Real<double>> with the
// default ℵ_0 would route the carrier-axis resolver to Boole, contradicting the
// ℶ_1 cardinality and the Kleene semantics this test relies on.)
using R2 = QuadraticReal<2>;  // the exact real carrier ℝ = ℚ(√2)
using Q = Rational<>;         // for the exact rational thresholds below

constexpr auto real_gt_zero = [](const Real<double>& x) {
  return x.resolve() > 0.0;
};

constexpr auto real_lt_three = [](const Real<double>& x) {
  return x.resolve() < 3.0;
};

constexpr auto complex_re_positive = [](const Complex<R2>& z) {
  return z.real() > R2{};
};

constexpr auto complex_im_nonnegative = [](const Complex<R2>& z) {
  return z.imag() >= R2{};
};

constexpr auto real_le_zero = [](const Real<double>& x) {
  return x.resolve() <= 0.0;
};
constexpr auto real_ge_three = [](const Real<double>& x) {
  return x.resolve() >= 3.0;
};

constexpr auto real_between = real_gt_zero && real_lt_three;
constexpr auto real_outside_band = real_le_zero || real_ge_three;
constexpr auto complex_first_quadrant =
    complex_re_positive && complex_im_nonnegative;
constexpr auto complex_not_third_quadrant =
    complex_re_positive || complex_im_nonnegative;

constexpr auto real_nonzero = [](const Real<double>& x) {
  return x.resolve() != 0.0;
};
constexpr auto complex_outside_unit_ball = [](const Complex<R2>& z) {
  return euclidean_norm_squared(z) > R2{1};
};

// Component-extraction predicates (a real's @c .resolve(), a complex's
// @c .real()/.imag()) are not π-expressible, so they stay NAMED-predicate
// comprehensions --- reusing the named predicates above.
constexpr auto ℝ_plus = Set{Comprehension{ℝ_d, real_gt_zero}};

constexpr auto ℝ_small = Set{Comprehension{ℝ_d, real_lt_three}};

constexpr auto ℝ_nonzero = Set{Comprehension{ℝ_d, real_nonzero}};

constexpr auto ℂ_right_half = Set{Comprehension{ℂ, complex_re_positive}};

constexpr auto ℂ_upper_half = Set{Comprehension{ℂ, complex_im_nonnegative}};

constexpr auto ℂ_outside_unit_ball =
    Set{Comprehension{ℂ, complex_outside_unit_ball}};

constexpr auto between_reals = Set{Comprehension{ℝ_d, real_between}};
constexpr auto outside_real_band = Set{Comprehension{ℝ_d, real_outside_band}};
constexpr auto first_quadrant = Set{Comprehension{ℂ, complex_first_quadrant}};
constexpr auto not_third_quadrant =
    Set{Comprehension{ℂ, complex_not_third_quadrant}};

constexpr auto real_mix = !((ℝ_plus & ℝ_nonzero) | (ℝ_small | !ℝ_plus));
constexpr auto complex_mix =
    (ℂ_right_half & ℂ_outside_unit_ball) | !ℂ_upper_half;

// #892 gates the complement-pair collapse (a & !a to Ø, a | !a to 𝔸) on
// Boole, so under Kleene these pairs stay residual nodes rather
// than collapsing; the meaning is still decided pointwise (the predicates are
// decidable).  The residual TYPE is pinned for the classical forms in
// halfspace_test / the exhibit; the K3 gate nuance is #860 / #894.
static_assert((ℝ_plus & !ℝ_plus)(Real<double>{4.0}) == Ternary::False);
static_assert((ℝ_plus | !ℝ_plus)(Real<double>{4.0}) == Ternary::True);
static_assert((ℂ_outside_unit_ball & !ℂ_outside_unit_ball)(Complex<R2>{
                  R2{2}, R2{}}) == Ternary::False);
static_assert((ℂ_outside_unit_ball | !ℂ_outside_unit_ball)(Complex<R2>{
                  R2{2}, R2{}}) == Ternary::True);

static_assert(real_mix(Real<double>{4.0}) == Ternary::False);
static_assert(real_mix(Real<double>{2.0}) == Ternary::False);
static_assert(real_mix(Real<double>{-1.0}) == Ternary::False);

static_assert(between_reals(Real<double>{2.0}) == Ternary::True);
static_assert(between_reals(Real<double>{4.0}) == Ternary::False);
static_assert(outside_real_band(Real<double>{-1.0}) == Ternary::True);

static_assert(complex_mix(Complex<R2>{R2{2}, R2{}}) == Ternary::True);
static_assert(complex_mix(Complex<R2>{R2{Q{1, 5}}, R2{Q{-2, 5}}}) ==
              Ternary::True);
static_assert(complex_mix(Complex<R2>{R2{Q{1, 5}}, R2{Q{2, 5}}}) ==
              Ternary::False);

static_assert(first_quadrant(Complex<R2>{R2{1}, R2{1}}) == Ternary::True);
static_assert(first_quadrant(Complex<R2>{R2{1}, R2{-1}}) == Ternary::False);
static_assert(not_third_quadrant(Complex<R2>{R2{-1}, R2{-1}}) ==
              Ternary::False);

static_assert(((between_reals | !between_reals)(Real<double>{5.0})) ==
              Ternary::True);
static_assert(((first_quadrant & !first_quadrant)(Complex<R2>{R2{1}, R2{1}})) ==
              Ternary::False);

}  // namespace

TEST_CASE("Numbers: static set algebra over R and C",
          "[numbers][sets][static]") {
  SUCCEED();
}
