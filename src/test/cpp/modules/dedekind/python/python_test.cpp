#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <optional>

import dedekind.category;
import dedekind.numbers;
import dedekind.order;
import dedekind.python;
import dedekind.sequences;

using namespace dedekind::python;
using dedekind::category::Boole;
using dedekind::category::Chain;
using dedekind::category::Identity;
using dedekind::category::Predecessor;
using dedekind::category::Successor;
using dedekind::category::Ternary;
using dedekind::numbers::collatz_step;
using dedekind::order::Direction;
using dedekind::order::reduce_meet;
using dedekind::order::Strictness;
using dedekind::sequences::iterate;

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

TEST_CASE("Python facade: the bounded ∀ over a window, in K₃",
          "[python][collatz]") {
  const auto step =
      collatz::ArrowN{std::function<collatz::Nat(collatz::Nat)>{collatz_step}};
  // [1, 10): every seed below 10 reaches 1 within 19 steps (9 takes 19).
  const auto window =
      reduce_meet(lwv::ray<Direction::Upward, Strictness::NonStrict>(1),
                  lwv::ray<Direction::Downward, Strictness::Strict>(10));
  REQUIRE(lwv::forall(window, collatz::ReachesWithin{step, 19}) ==
          Ternary::True);
  REQUIRE(lwv::forall(window, collatz::ReachesWithin{step, 18}) ==
          Ternary::Unknown);
  REQUIRE(lwv::forall(lwv::Set::empty(), collatz::ReachesWithin{step, 0}) ==
          Ternary::True);
  REQUIRE(iterate(std::size_t{6}, collatz_step).at(8) == 1u);
}
