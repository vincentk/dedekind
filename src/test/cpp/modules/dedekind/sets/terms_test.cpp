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
#include <string>
#include <type_traits>
#include <utility>
#include <variant>

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

TEST_CASE(
    "sets:terms: a term built at run time over two carriers has a sort "
    "per slot",
    "[sets][terms][runtime][sorts]") {
  using T = Term<unsigned>;
  using Pr = Pred<unsigned>;
  using NM = std::pair<unsigned, unsigned long long>;
  const T x = T::proj(1), y = T::proj(2);
  // Each slot compared in its own sort; a constant takes its sibling's.
  const Pr p = Pr::conj(
      Pr::cmp(Rel::Eq, T::bin(Arith::Add, x, T::constant(1u)), T::constant(4u)),
      Pr::cmp(Rel::Eq, y, T::constant(7u)));
  CHECK(p(NM{3u, 7ull}));
  CHECK_FALSE(p(NM{3u, 8ull}));
  CHECK_FALSE(p(NM{4u, 7ull}));
  CHECK_NOTHROW(bind_check<NM>(p));
  // Two carriers in one operator or comparison: refused when bound, and when
  // read; an arrow between them is applied, not implied.
  CHECK_THROWS_AS(bind_check<NM>(Pr::cmp(Rel::Eq, T::bin(Arith::Add, x, y),
                                         T::constant(0u))),
                  not_admitted);
  CHECK_THROWS_AS(bind_check<NM>(Pr::cmp(Rel::Eq, x, y)), not_admitted);
  CHECK_THROWS_AS(Pr::cmp(Rel::Eq, x, y)(NM{1u, 1ull}), not_admitted);
  // A comparison of two constants names no carrier.
  CHECK_THROWS_AS(
      bind_check<NM>(Pr::cmp(Rel::Eq, T::constant(1u), T::constant(1u))),
      not_admitted);
  // The value of a term over two carriers is the sum of their sorts.
  const auto v =
      term_value(T::bin(Arith::Add, y, T::constant(1u)), NM{3u, 7ull});
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(v)>,
                            std::variant<unsigned, unsigned long long>>);
  CHECK(std::get<unsigned long long>(v) == 8ull);
}

TEST_CASE(
    "sets:terms: ⊤ and ⊥ are predicates, and a constant converts only "
    "into the sort that reads it",
    "[sets][terms][runtime][sorts]") {
  using T = Term<unsigned>;
  using Pr = Pred<unsigned>;
  CHECK(Pr::top()(0u));
  CHECK_FALSE(Pr::bottom()(0u));
  CHECK(same_shape(Pr::top().negated(), Pr::bottom()));
  CHECK(Pr::conj(Pr::top(), Pr::cmp(Rel::Eq, T::proj(0), T::constant(3u)))(3u));
  CHECK_NOTHROW(bind_check<unsigned>(Pr::disj(Pr::bottom(), Pr::top())));
  // Constants that are strings, on a product of the word and strings: the
  // atom on the string slot binds and reads, although the word slot's reading
  // is instantiated as well; a string on the word slot is refused where read.
  using S = Term<std::string>;
  using SP = Pred<std::string>;
  using NS = std::pair<unsigned, std::string>;
  const SP ok = SP::cmp(Rel::Eq, S::proj(2), S::constant(std::string{"ok"}));
  CHECK_NOTHROW(bind_check<NS>(ok));
  CHECK(ok(NS{1u, "ok"}));
  CHECK_FALSE(ok(NS{1u, "no"}));
  CHECK_THROWS_AS(bind_check<NS>(SP::cmp(Rel::Eq, S::proj(1),
                                         S::constant(std::string{"ok"}))),
                  std::domain_error);
}

TEST_CASE(
    "sets:terms: a row is its own schema: n slots, a sort per cell, read at "
    "run "
    "time",
    "[sets][terms][rows]") {
  using R = Row<unsigned, bool>;
  using T = Term<int>;
  using Q = Pred<int>;
  // Three columns: a word, a bool, a word.
  const R rep = row_of_sorts<unsigned, bool>({0, 1, 0});
  CHECK(slot_sort<R>(1, rep) == 0);
  CHECK(slot_sort<R>(2, rep) == 1);
  CHECK(slot_sort<R>(3, rep) == 0);
  CHECK_THROWS_AS(slot_sort<R>(4, rep), not_admitted);  // past the arity
  CHECK_THROWS_AS(slot_sort<R>(0, rep), not_admitted);  // π is no column
  CHECK(one_sort<R>(rep) == no_sort);                   // two carriers
  CHECK(one_sort<R>(row_of_sorts<unsigned, bool>({0, 0})) == 0);

  // π_3 == π_1 + 1, read at rows: each slot in its column's sort.
  const Q succ = Q::cmp(Rel::Eq, T::proj(3),
                        T::bin(Arith::Add, T::proj(1), T::constant(1)));
  CHECK_NOTHROW(bind_check<R>(succ, rep));
  CHECK(succ(R{{4u, true, 5u}}));
  CHECK_FALSE(succ(R{{4u, true, 6u}}));
  // A word and a bool do not meet: π_1 == π_2 is refused at these rows.
  CHECK_THROWS_AS(bind_check<R>(Q::cmp(Rel::Eq, T::proj(1), T::proj(2)), rep),
                  not_admitted);

  // The graph of a function of the first two columns, the result the third.
  CHECK(graph_term(succ, 3) != nullptr);
  CHECK(graph_term(succ, 2) == nullptr);  // π_2 is not its result
  // A product shifts the slots: map_slots renames them all at once.
  struct Shift {
    T operator()(unsigned s) const { return T::proj(s + 1); }
  };
  CHECK(same_shape(succ.map_slots(Shift{}),
                   Q::cmp(Rel::Eq, T::proj(4),
                          T::bin(Arith::Add, T::proj(2), T::constant(1)))));
}
