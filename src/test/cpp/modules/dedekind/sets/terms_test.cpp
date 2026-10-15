/** @file dedekind/sets/terms_test.cpp
 *
 * What sets:terms certifies by itself: the syntax, projections and constants,
 * + - * where total (category's certificate) and == / !=.  A comparison or a
 * division needs the rule of the module that owns its certificate (order,
 * algebra): see order/terms_test.cpp.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>
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
    "sets:terms: McCarthy's conditional built at run time chooses a branch "
    "by a predicate",
    "[sets][terms][runtime]") {
  using T = Term<unsigned>;
  using Pr = Pred<unsigned>;
  // cond(π == 0, π + 1, π * 3): the typed grammar's cond, with its shape a
  // value; the operators are the ones this module admits.
  const T x = T::proj(0);
  const T step = T::cond(Pr::cmp(Rel::Eq, x, T::constant(0u)),
                         T::bin(Arith::Add, x, T::constant(1u)),
                         T::bin(Arith::Mul, x, T::constant(3u)));
  CHECK(term_value(step, 0u) == 1u);
  CHECK(term_value(step, 2u) == 6u);
  // Substitution reaches the predicate and both branches.
  CHECK(term_value(step.subst(0, T::constant(2u)), 0u) == 6u);
  CHECK(same_shape(step, step));
  CHECK_FALSE(
      same_shape(step, T::cond(Pr::cmp(Rel::Eq, x, T::constant(1u)),
                               T::bin(Arith::Add, x, T::constant(1u)),
                               T::bin(Arith::Mul, x, T::constant(3u)))));
  CHECK_NOTHROW(bind_check<unsigned>(step));
}

TEST_CASE(
    "sets:terms: a graph atom built at run time names its function's "
    "term",
    "[sets][terms][runtime]") {
  using T = Term<unsigned>;
  using Pr = Pred<unsigned>;
  const T x = T::proj(1), y = T::proj(2);
  const T t = T::bin(Arith::Add, T::bin(Arith::Mul, x, T::constant(3u)),
                     T::constant(1u));
  // π_2 == t(π_1), either way round: the term is t.
  CHECK(graph_term(Pr::cmp(Rel::Eq, y, t)) != nullptr);
  CHECK(graph_term(Pr::cmp(Rel::Eq, t, y)) != nullptr);
  CHECK(term_value(graph_term(Pr::cmp(Rel::Eq, y, t))->subst(1, T::proj(0)),
                   4u) == 13u);
  // Not a graph: another comparison, a term mentioning π_2, a conjunction.
  CHECK(graph_term(Pr::cmp(Rel::Lt, y, t)) == nullptr);
  CHECK(graph_term(Pr::cmp(Rel::Eq, y, T::bin(Arith::Add, y, x))) == nullptr);
  CHECK(graph_term(Pr::conj(Pr::cmp(Rel::Eq, y, t),
                            Pr::cmp(Rel::Lt, x, T::constant(5u)))) == nullptr);
}
