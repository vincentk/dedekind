#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.numbers.ieee;

using namespace dedekind::category;
using namespace dedekind::numbers;

TEST_CASE("IEEE fast lane requires explicit import",
          "[numbers][ieee][unsafe]") {
  const auto x = ieee_unit(0.1);
  const auto y = ieee_unit(0.2);
  const auto s = x + y;

  CHECK(s.resolve() != 0.3);
}

TEST_CASE("IEEE map/bind and lane conversions", "[numbers][ieee][unsafe]") {
  const auto x = ieee_unit(2.0);

  const auto mapped = ieee_map(x, [](double v) { return v * 3.0; });
  CHECK(mapped.resolve() == 6.0);

  const auto bound = ieee_bind(x, [](double v) { return ieee_unit(v + 5.0); });
  CHECK(bound.resolve() == 7.0);

  const auto safe = Real<machine_real_scalar>{4.0};
  const auto fast = assume_ieee(safe);
  CHECK(fast.resolve() == 4.0);

  const auto back = discharge_ieee(fast);
  CHECK(back.resolve() == 4.0);
}

static_assert(is_associative_v<IEEE<double>, IEEEAdd<double>>,
              "IEEE fast lane should certify associativity by explicit policy");

static_assert(
    is_kleene_associative_v<IEEE<double>, IEEEAdd<double>>,
    "IEEE fast lane should certify Kleene associativity by explicit policy");

static_assert(partial_identity_v<IEEE<double>, IEEEAdd<double>>.resolve() ==
                  0.0,
              "IEEE addition identity should be 0");

static_assert(partial_identity_v<IEEE<double>, IEEEMul<double>>.resolve() ==
                  1.0,
              "IEEE multiplication identity should be 1");
