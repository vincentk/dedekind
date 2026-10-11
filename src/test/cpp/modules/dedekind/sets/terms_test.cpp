/** @file dedekind/sets/terms_test.cpp
 *
 * Runtime companions for sets:terms: terms compose at any nesting, a constant
 * meets the carrier of the term beside it, and the checks on a constant's
 * value raise where they cannot be compile errors.
 */

#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <utility>

import dedekind.sets;

using namespace dedekind::sets;
using P = std::pair<int, int>;

TEST_CASE("sets:terms: terms compose and evaluate at a point",
          "[sets][terms]") {
  const auto step = cond(π1 % 2 == 0, π1 / 2, π1 * 3 + 1) == π2;
  CHECK(step(P{6, 3}));
  CHECK(step(P{7, 22}));
  CHECK_FALSE(step(P{7, 21}));
  CHECK(((π1 + 1) * 3 == π2)(P{1, 6}));
  CHECK((π % 3 == 0)(9));
  CHECK_FALSE((π % 3 == 0)(10));
  CHECK(slots_v<decltype(π1 * 3 + 1)> == 0b10u);
}

TEST_CASE("sets:terms: a constant meets the carrier beside it",
          "[sets][terms][constants]") {
  const auto succ = π1 + 1 == π2;
  CHECK(succ(std::pair{finite_cardinality(4), finite_cardinality(5)}));
  CHECK(carrier_cast<Cardinality>(3) == finite_cardinality(3));
  CHECK_THROWS_AS(carrier_cast<Cardinality>(-1), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<unsigned char>(300), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<bool>(2), std::domain_error);
}

TEST_CASE("sets:terms: a division by the constant 0 is refused when built",
          "[sets][terms][constants]") {
  CHECK_THROWS_AS(π1 / 0, std::domain_error);
  CHECK_THROWS_AS(π1 % 0, std::domain_error);
  CHECK_NOTHROW(π1 / 2);
}

TEST_CASE("sets:terms: a constant is checked when the term is bound",
          "[sets][terms][constants]") {
  CHECK_THROWS_AS(𝔸<Cardinality>{} | (π + -1 == 3), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<int>(1.5), std::domain_error);
  CHECK_THROWS_AS(carrier_cast<int>(1e12), std::domain_error);
  CHECK(carrier_cast<int>(2.0) == 2);
  const auto both = !(π1 < 3 && π2 > 5);
  CHECK(both(P{3, 0}));
  CHECK_FALSE(both(P{2, 6}));
}
