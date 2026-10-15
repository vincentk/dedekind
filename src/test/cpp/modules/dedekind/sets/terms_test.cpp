/** @file dedekind/sets/terms_test.cpp
 *
 * What sets:terms certifies by itself: the syntax, projections and constants,
 * + - * where total (category's certificate) and == / !=.  A comparison or a
 * division needs the rule of the module that owns its certificate (order,
 * algebra): see order/terms_test.cpp.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <memory>
#include <stdexcept>
#include <utility>

import dedekind.sets;

using namespace dedekind::sets;
using P = std::pair<unsigned, unsigned>;

TEST_CASE(
    "sets:terms: + * == evaluate at a point; a constant meets the carrier",
    "[sets][terms]") {
  const auto succ = π_1 + 1 == π_2;
  CHECK(succ(P{4, 5}));
  CHECK_FALSE(succ(P{4, 6}));
  CHECK(((π_1 + 1) * 3 == π_2)(P{1, 6}));
  CHECK((π != 3)(4u));
  CHECK(slots_v<decltype(π_1 * 3 + 1)> == 0b10u);
  CHECK((𝔸<unsigned>{} | (π == 4))(4u));
}

TEST_CASE("sets:terms: a division by the constant 0 is refused when built",
          "[sets][terms][construction]") {
  // Building a term is syntax: no rule is asked until the former binds it.
  CHECK_THROWS_AS(π_1 / 0, std::domain_error);
  CHECK_THROWS_AS(π_1 % 0, std::domain_error);
  CHECK_NOTHROW(π_1 / 2);
  CHECK_THROWS_AS(
      Term<int>::bin(Arith::Mod, Term<int>::proj(0), Term<int>::constant(0)),
      std::domain_error);
}

TEST_CASE("sets:terms: without a module's rule its operators are not admitted",
          "[sets][terms][rules]") {
  // order is not imported here: the comparisons and divisions have no rule.
  STATIC_CHECK(admits(On<unsigned>{}, std::plus<>{}, false));
  STATIC_CHECK(admits(On<unsigned>{}, std::equal_to<>{}, false));
  STATIC_CHECK_FALSE(admits(On<unsigned>{}, std::less<>{}, false));
  STATIC_CHECK_FALSE(admits(On<unsigned>{}, std::modulus<>{}, true));
  STATIC_CHECK_FALSE(admits(On<unsigned>{}, std::divides<>{}, true));
  // 𝔹's rig is spelled (∨, ∧); std::plus<bool> is registered as nothing, so
  // neither + nor − is in its signature.
  STATIC_CHECK_FALSE(admits(On<bool>{}, std::plus<>{}, false));
  STATIC_CHECK_FALSE(admits(On<bool>{}, std::minus<>{}, false));
  CHECK_THROWS_AS(bind_check<unsigned>(Pred<int>::cmp(
                      Rel::Lt, Term<int>::proj(0), Term<int>::constant(1))),
                  not_admitted);
}

TEST_CASE(
    "sets:terms: an arrow without a term is applied, evaluated, never "
    "rewritten, and binds to its own carrier only",
    "[sets][terms][runtime]") {
  using T = Term<unsigned>;
  const auto twice = std::make_shared<OpaqueArrow<unsigned>>(
      "twice", [](const unsigned& x) { return 2 * x; });
  const T t = T::bin(Arith::Add, T::ap(twice, T::proj(0)), T::constant(1u));
  CHECK(term_value(t, 5u) == 11u);  // 2·5 + 1
  // The same arrow in the typed grammar: an opaque arrow is an IsArrow.
  CHECK(term_value(ap(*twice, π), 5u) == 10u);
  // Substitution reaches through the application: twice(π) at π := 3.
  CHECK(term_value(t.subst(0, T::constant(3u)), 0u) == 7u);
  // The same arrow is the same object; another arrow of the same table is not.
  const auto again = std::make_shared<OpaqueArrow<unsigned>>(
      "twice", [](const unsigned& x) { return 2 * x; });
  CHECK(same_shape(t, t));
  CHECK_FALSE(same_shape(
      t, T::bin(Arith::Add, T::ap(again, T::proj(0)), T::constant(1u))));
  // Bound to another carrier, the arrow is refused.
  CHECK_NOTHROW(bind_check<unsigned>(t));
  CHECK_THROWS_AS(bind_check<unsigned long long>(t), not_admitted);
}
