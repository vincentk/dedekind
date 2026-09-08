#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

namespace {

// Post-HSP retarget: ℂ is the COAT-HANGER universe value
// Ω<Complex<QuadraticReal<2>>, ClassicalLogic, ℶ_1>, so these ℂ showcases run
// on EXACT ℚ(√2) arithmetic.  (The scout stays element<ℂ>: `ℂ | pred` directly
// is blocked because the free operator|(Set, Set) already means set-union, so
// the scout disambiguates comprehension from union.)  The machine-real scout is
// element<ℝ_d> — ℝ itself is the ℚ(√2) coat-hanger, so Real<double> live on the
// materialisable ambient ℝ_d = Ω<Real<double>, ClassicalLogic, ℶ_1>.  (A plain
// Ω<Real<double>> with the default ℵ_0 would route the carrier-axis resolver to
// ClassicalLogic, contradicting the ℶ_1 cardinality and the Ternary semantics
// this test relies on.)
using R2 = QuadraticReal<2>;  // the exact real carrier ℝ = ℚ(√2)
using Q = Rational<>;         // for the exact rational thresholds below
constexpr auto r = element<ℝ_d>;
constexpr auto c = element<ℂ>;  // now a Complex<QuadraticReal<2>> scout

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

constexpr auto ℝ_plus =
    Set{r | [](const Real<double>& x) { return x.resolve() > 0.0; }};

constexpr auto ℝ_small =
    Set{r | [](const Real<double>& x) { return x.resolve() < 3.0; }};

constexpr auto ℝ_nonzero =
    Set{r | [](const Real<double>& x) { return x.resolve() != 0.0; }};

constexpr auto ℂ_right_half =
    Set{c | [](const Complex<R2>& z) { return z.real() > R2{}; }};

constexpr auto ℂ_upper_half =
    Set{c | [](const Complex<R2>& z) { return z.imag() >= R2{}; }};

constexpr auto ℂ_outside_unit_ball = Set{
    c | [](const Complex<R2>& z) { return euclidean_norm_squared(z) > R2{1}; }};

constexpr auto between_reals = Set{r | real_between};
constexpr auto outside_real_band = Set{r | real_outside_band};
constexpr auto first_quadrant = Set{c | complex_first_quadrant};
constexpr auto not_third_quadrant = Set{c | complex_not_third_quadrant};

using RDomain = typename decltype(ℝ_plus)::Domain;
using CDomain = typename decltype(ℂ_right_half)::Domain;

constexpr Ø<RDomain, TernaryLogic> R_empty{};
constexpr UniversalSet<RDomain, TernaryLogic> R_ambient{};
constexpr Ø<CDomain, TernaryLogic> C_empty{};
constexpr UniversalSet<CDomain, TernaryLogic> C_ambient{};

constexpr auto real_mix = !((ℝ_plus & ℝ_nonzero) | (ℝ_small | !ℝ_plus));
constexpr auto complex_mix =
    (ℂ_right_half & ℂ_outside_unit_ball) | !ℂ_upper_half;

static_assert(R_empty == (ℝ_plus & !ℝ_plus));
static_assert(R_ambient == (ℝ_plus | !ℝ_plus));

static_assert(C_empty == (ℂ_outside_unit_ball & !ℂ_outside_unit_ball));
static_assert(C_ambient == (ℂ_outside_unit_ball | !ℂ_outside_unit_ball));

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
