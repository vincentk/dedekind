#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

namespace {

// ℂ is 𝔸<Complex<QuadraticReal<2>>, Kleene, ℶ_1>, so the ℂ cases run on EXACT
// ℚ(√2) arithmetic.  Machine reals live on ℝ_d, the finite doubles 𝕃<double>:
// NaN is excluded by the carrier, so ℝ_d is Boole.  The comprehensions spell
// the node explicitly as @c Comprehension{universe, pred}.
using Rd = typename decltype(ℝ_d)::Domain;  // 𝕃<double>, the finite doubles
using R2 = QuadraticReal<2>;                // the exact real carrier ℝ = ℚ(√2)
using Q = Rational<>;  // for the exact rational thresholds below

constexpr auto real_gt_zero = [](const Rd& x) { return x.value() > 0.0; };

constexpr auto real_lt_three = [](const Rd& x) { return x.value() < 3.0; };

constexpr auto complex_re_positive = [](const Complex<R2>& z) {
  return z.real() > R2{};
};

constexpr auto complex_im_nonnegative = [](const Complex<R2>& z) {
  return z.imag() >= R2{};
};

constexpr auto real_le_zero = [](const Rd& x) { return x.value() <= 0.0; };
constexpr auto real_ge_three = [](const Rd& x) { return x.value() >= 3.0; };

constexpr auto real_between = real_gt_zero && real_lt_three;
constexpr auto real_outside_band = real_le_zero || real_ge_three;
constexpr auto complex_first_quadrant =
    complex_re_positive && complex_im_nonnegative;
constexpr auto complex_not_third_quadrant =
    complex_re_positive || complex_im_nonnegative;

constexpr auto real_nonzero = [](const Rd& x) { return x.value() != 0.0; };
constexpr auto complex_outside_unit_ball = [](const Complex<R2>& z) {
  return euclidean_norm_squared(z) > R2{1};
};

// Component-extraction predicates (a real's @c .value(), a complex's
// @c .real()/.imag()) are not π-expressible, so they stay NAMED-predicate
// comprehensions --- reusing the named predicates above.
constexpr auto ℝ_plus = Comprehension{ℝ_d, real_gt_zero};

constexpr auto ℝ_small = Comprehension{ℝ_d, real_lt_three};

constexpr auto ℝ_nonzero = Comprehension{ℝ_d, real_nonzero};

constexpr auto ℂ_right_half = Comprehension{ℂ, complex_re_positive};

constexpr auto ℂ_upper_half = Comprehension{ℂ, complex_im_nonnegative};

constexpr auto ℂ_outside_unit_ball =
    Comprehension{ℂ, complex_outside_unit_ball};

constexpr auto between_reals = Comprehension{ℝ_d, real_between};
constexpr auto outside_real_band = Comprehension{ℝ_d, real_outside_band};
constexpr auto first_quadrant = Comprehension{ℂ, complex_first_quadrant};
constexpr auto not_third_quadrant =
    Comprehension{ℂ, complex_not_third_quadrant};

constexpr auto real_mix = ~((ℝ_plus & ℝ_nonzero) | (ℝ_small | ~ℝ_plus));
constexpr auto complex_mix =
    (ℂ_right_half & ℂ_outside_unit_ball) | ~ℂ_upper_half;

// ℝ_d is Boole, so excluded middle and non-contradiction are laws there.
static_assert(std::same_as<typename decltype(ℝ_plus)::logic_species, Boole>);
static_assert((ℝ_plus & ~ℝ_plus)(Rd{4.0}) == Boole::False);
static_assert((ℝ_plus | ~ℝ_plus)(Rd{4.0}) == Boole::True);

static_assert(real_mix(Rd{4.0}) == Boole::False);
static_assert(real_mix(Rd{2.0}) == Boole::False);
static_assert(real_mix(Rd{-1.0}) == Boole::False);

static_assert(between_reals(Rd{2.0}) == Boole::True);
static_assert(between_reals(Rd{4.0}) == Boole::False);
static_assert(outside_real_band(Rd{-1.0}) == Boole::True);

static_assert(complex_mix(Complex<R2>{R2{2}, R2{}}) == Ternary::True);
static_assert(complex_mix(Complex<R2>{R2{Q{1, 5}}, R2{Q{-2, 5}}}) ==
              Ternary::True);
static_assert(complex_mix(Complex<R2>{R2{Q{1, 5}}, R2{Q{2, 5}}}) ==
              Ternary::False);

static_assert(first_quadrant(Complex<R2>{R2{1}, R2{1}}) == Ternary::True);
static_assert(first_quadrant(Complex<R2>{R2{1}, R2{-1}}) == Ternary::False);
static_assert(not_third_quadrant(Complex<R2>{R2{-1}, R2{-1}}) ==
              Ternary::False);

static_assert(((between_reals | ~between_reals)(Rd{5.0})) == Boole::True);

}  // namespace

TEST_CASE("Numbers: static set algebra over R and C",
          "[numbers][sets][static]") {
  SUCCEED();
}
