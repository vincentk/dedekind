/** @file dedekind/morphologies/safe_float_test.cpp
 *
 * Runtime coverage for @c safe_float<F> / @c 𝕃<F> --- the finite-float
 * lattice (#496 first slice).  The compile-time certificates
 * (@c IsTotallyOrdered, meet / join semilattice) are pinned in the module,
 * but @c static_assert is invisible to Codecov, so this file exercises the
 * carrier at @b runtime:
 *
 *  - @c try_safe_float gates the boundary: @c nullopt on NaN / +/-inf,
 *    @c Just on a finite value (the enforced invariant).
 *  - @c (min, max) obey the lattice laws (idempotence, commutativity,
 *    associativity, absorption) on @c 𝕃<double> --- associativity being the
 *    exact bit that @c (+, x) lack, since min / max are selections.
 *  - signed zeros collapse to one lattice element (antisymmetry up to @c ==).
 *  - the lattice concepts fire (@c STATIC_CHECK, for a local witness).
 */

#include <algorithm>  // std::ranges::min / max
#include <catch2/catch_test_macros.hpp>
#include <limits>
#include <optional>

import dedekind.morphologies;
import dedekind.category;
import dedekind.order;

namespace mo = dedekind::morphologies;
using SF = mo::safe_float<double>;

namespace {
/** @brief Lift a value known to be finite (test inputs only). */
SF sf(double x) { return *mo::try_safe_float(x); }
}  // namespace

TEST_CASE("morphologies:safe_float — try_safe_float gates the boundary",
          "[morphologies][safe_float][lattice][496]") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double inf = std::numeric_limits<double>::infinity();
  CHECK_FALSE(mo::try_safe_float(nan).has_value());
  CHECK_FALSE(mo::try_safe_float(inf).has_value());
  CHECK_FALSE(mo::try_safe_float(-inf).has_value());
  REQUIRE(mo::try_safe_float(1.5).has_value());
  CHECK(mo::try_safe_float(1.5)->value() == 1.5);
}

TEST_CASE(
    "morphologies:safe_float — (min,max) obey the lattice laws on 𝕃<double>",
    "[morphologies][safe_float][lattice]") {
  const SF a = sf(-2.0), b = sf(3.0), c = sf(7.5);

  // idempotence
  CHECK(std::ranges::min(a, a) == a);
  CHECK(std::ranges::max(a, a) == a);

  // commutativity
  CHECK(std::ranges::min(a, b) == std::ranges::min(b, a));
  CHECK(std::ranges::max(a, b) == std::ranges::max(b, a));

  // associativity --- the exact bit: a selection, no rounding
  CHECK(std::ranges::min(a, std::ranges::min(b, c)) ==
        std::ranges::min(std::ranges::min(a, b), c));
  CHECK(std::ranges::max(a, std::ranges::max(b, c)) ==
        std::ranges::max(std::ranges::max(a, b), c));

  // absorption: min(a, max(a, b)) == a == max(a, min(a, b))
  CHECK(std::ranges::min(a, std::ranges::max(a, b)) == a);
  CHECK(std::ranges::max(a, std::ranges::min(a, b)) == a);

  // the selected values
  CHECK(std::ranges::min(a, b).value() == -2.0);
  CHECK(std::ranges::max(b, c).value() == 7.5);
}

TEST_CASE(
    "morphologies:safe_float — signed zeros collapse to one lattice element",
    "[morphologies][safe_float][lattice][signed-zero]") {
  // -0.0 and +0.0 are distinct bit patterns that compare equal; the lattice
  // works up to ==, which the defaulted operator== absorbs.
  CHECK(sf(-0.0) == sf(+0.0));
  CHECK(std::ranges::min(sf(-0.0), sf(+0.0)) == sf(0.0));
}

TEST_CASE("morphologies:safe_float — the lattice concepts fire",
          "[morphologies][safe_float][lattice][concepts]") {
  STATIC_CHECK(dedekind::order::IsTotallyOrdered<SF>);
  STATIC_CHECK(
      dedekind::category::IsCertifiedOrderMeetSemilattice<mo::𝕃<double>>);
  STATIC_CHECK(
      dedekind::category::IsCertifiedOrderJoinSemilattice<mo::𝕃<double>>);
}
