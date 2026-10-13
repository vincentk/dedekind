#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.python;

using namespace dedekind::python;
using dedekind::category::Boole;
using dedekind::category::Chain;
using dedekind::category::Identity;

TEST_CASE(
    "Python facade: refl is the species' reflection, erased, and "
    "composition is the reducer's",
    "[python][jlt]") {
  REQUIRE(jlt::refl<Boole>()(true) == false);
  REQUIRE(jlt::refl<Chain<jlt::Int>>()(jlt::refl<Chain<jlt::Int>>()(3)) == 3);
  REQUIRE(jlt::compose(Identity<bool>{}, jlt::refl<Boole>())(false) == true);
  REQUIRE(jlt::compose(jlt::refl<Boole>(), jlt::refl<Boole>())(true) == true);
}
