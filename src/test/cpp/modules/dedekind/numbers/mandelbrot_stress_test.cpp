/**
 * @file mandelbrot_stress_test.cpp
 * @brief Unit tests of @c :numbers:mandelbrot over exact Gaussian rationals.
 *
 * Every parameter is chosen so its orbit stays exact and small: c = 0 and
 * c = -1 are bounded (fixed point, 2-cycle), c = 2, 10 and -5/2 escape within
 * a few steps.  Parameters whose bounded orbit is dense in ℚ (c = -1/2) would
 * double their denominators' digits each step and are deliberately absent.
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.sequences;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::sequences;
using namespace dedekind::numbers;

namespace {
using Q = Rational<>;
using C = Complex<Q>;
constexpr C c_of(Q re) { return C{re, Q{}}; }
}  // namespace

TEST_CASE("Mandelbrot: orbit and escape over exact Gaussian rationals",
          "[numbers][mandelbrot]") {
  const auto criterion = euclidean_escape_radius_squared<Q>();

  SECTION("Orbit starts at zero") {
    const auto orbit = mandelbrot_orbit(c_of(Q{-1}));
    static_assert(IsSequence<decltype(orbit)>);
    REQUIRE(orbit.at(0) == C{});
    REQUIRE(orbit.at(1) == c_of(Q{-1}));
    REQUIRE(orbit.at(2) == C{});  // the 2-cycle 0, -1, 0, ...
  }

  SECTION("Truncated orbit prefix is a finite sequence") {
    const auto orbit = prefix(mandelbrot_orbit(c_of(Q{-1})), 9u);
    static_assert(IsFiniteSequence<decltype(orbit)>);
    REQUIRE(orbit.size() == 9u);
  }

  SECTION("orbit_escape_time: bounded and escaping points") {
    REQUIRE(!orbit_escape_time(mandelbrot_orbit(c_of(Q{0})), 50u, criterion)
                 .has_value());
    REQUIRE(!orbit_escape_time(mandelbrot_orbit(c_of(Q{-1})), 50u, criterion)
                 .has_value());

    // c = 2: z_1 = 2 (|2|² = 4, not > 4), z_2 = 6 → escapes at 2.
    const auto et_2 =
        orbit_escape_time(mandelbrot_orbit(c_of(Q{2})), 50u, criterion);
    REQUIRE(et_2.has_value());
    REQUIRE(*et_2 == 2u);

    const auto et_10 =
        orbit_escape_time(mandelbrot_orbit(c_of(Q{10})), 50u, criterion);
    REQUIRE(et_10.has_value());
    REQUIRE(*et_10 < *et_2);
  }

  SECTION("orbit_divergence_path: Kleene running state, True absorbing") {
    const auto divergence =
        orbit_divergence_path(mandelbrot_orbit(c_of(Q{2})), criterion);
    static_assert(std::same_as<std::remove_cvref_t<decltype(divergence)>,
                               DivergencePath<Q>>);
    REQUIRE(divergence.at(0) == Ternary::Unknown);
    REQUIRE(divergence.at(1) == Ternary::Unknown);
    REQUIRE(divergence.at(2) == Ternary::True);
    REQUIRE(divergence.at(3) == Ternary::True);

    const auto bounded =
        orbit_divergence_path(mandelbrot_orbit(c_of(Q{-1})), criterion);
    REQUIRE(bounded.at(0) == Ternary::Unknown);
    REQUIRE(bounded.at(50) == Ternary::Unknown);
  }

  SECTION("euclidean_escape_radius_squared: parametric threshold") {
    const auto large_criterion = euclidean_escape_radius_squared<Q>(Q{9});
    REQUIRE(criterion(c_of(Q{5, 2})) == true);         // |5/2|² > 4
    REQUIRE(large_criterion(c_of(Q{5, 2})) == false);  // not > 9
  }

  SECTION("M_kleene_N and M_N: the three layers") {
    const auto kleene = M_kleene_N<Q>(50u, criterion);
    REQUIRE(kleene(c_of(Q{-1})) == Ternary::Unknown);
    REQUIRE(kleene(c_of(Q{-5, 2})) == Ternary::True);

    const auto inclusive = M_N<Q>(50u, criterion);
    using LI = typename decltype(inclusive)::logic_species;
    REQUIRE(inclusive(c_of(Q{-1})) == LI::True);
    REQUIRE(inclusive(c_of(Q{-5, 2})) == LI::False);

    const auto exclusive = M_N<Q>(50u, criterion, KleenePolicy::Exclusive);
    using LE = typename decltype(exclusive)::logic_species;
    REQUIRE(exclusive(c_of(Q{-1})) == LE::False);
    REQUIRE(exclusive(c_of(Q{-5, 2})) == LE::False);
  }
}
