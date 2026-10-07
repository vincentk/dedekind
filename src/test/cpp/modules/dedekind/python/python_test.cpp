#include <catch2/catch_test_macros.hpp>
#include <optional>

import dedekind.category;
import dedekind.order;
import dedekind.python;

using namespace dedekind::python;
using dedekind::category::Boole;
using dedekind::category::Chain;
using dedekind::category::Identity;
using dedekind::category::Predecessor;
using dedekind::category::Successor;
using dedekind::order::Direction;
using dedekind::order::Strictness;

TEST_CASE("Python facade: the value leaf under the structural arrows",
          "[python][lwv]") {
  constexpr auto above = lwv::ray<Direction::Upward, Strictness::Strict>;
  const auto above5 = above(5);
  REQUIRE(lwv::image(Successor<jlt::Int>{}, above5) == above(6));
  REQUIRE(lwv::preimage(Successor<jlt::Int>{}, above5) == above(4));
  REQUIRE(lwv::image(Predecessor<jlt::Int>{}, above5) ==
          lwv::preimage(Successor<jlt::Int>{}, above5));
  REQUIRE(lwv::image(Identity<jlt::Int>{}, above5) == above5);
  const auto window = lwv::restrict(above5, std::nullopt, 9);  // 5 < x < 9
  REQUIRE(lwv::is_bounded(window));
  REQUIRE(lwv::least(window) == 6);
  REQUIRE(lwv::greatest(window) == 8);
  REQUIRE(!lwv::is_bounded(above5));
  REQUIRE(lwv::least(above5) == 6);
  REQUIRE(!lwv::greatest(above5));
}

TEST_CASE(
    "Python facade: refl is the species' reflection, erased, and "
    "composition is the reducer's",
    "[python][jlt]") {
  REQUIRE(jlt::refl<Boole>()(true) == false);
  REQUIRE(jlt::refl<Chain<jlt::Int>>()(jlt::refl<Chain<jlt::Int>>()(3)) == 3);
  REQUIRE(jlt::compose(Identity<bool>{}, jlt::refl<Boole>())(false) == true);
  REQUIRE(jlt::compose(jlt::refl<Boole>(), jlt::refl<Boole>())(true) == true);
}
