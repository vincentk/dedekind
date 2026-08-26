/** @file probe_bool_collapse_test.cpp  — TEMPORARY PROBE (delete after)
 *
 * Question: can a bool complement-pair (P, ~P) type-collapse structurally
 * (P & ~P → Ø, P | ~P → UniversalSet), or does bool's finite materialization
 * (FiniteBooleanSet / BooleanEqPredicate) pre-empt the complement-pair branch?
 *
 * Two constructions:
 *   (1) scout predicate  -> BooleanEqPredicate (the path that materialised in
 *       the pruning test);
 *   (2) lambda predicate -> should bypass BooleanEqPredicate and keep the
 *       NegatedPredicate structure that the complement-pair branch recognises.
 * A failing static_assert prints the *actual* result type, which is the data.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::numbers;
using namespace dedekind::order;

TEST_CASE("probe: bool complement-pair collapse", "[probe][sets][bool]") {
  constexpr auto U = Ω<bool>;

  // (1) scout / BooleanEqPredicate path
  {
    constexpr auto P = Set{in<U> | in<U>};  // {true}
    constexpr auto nP = ~P;                 // complement
    static_assert(Ø<bool>{} == (P & nP), "(1) P & ~P collapses to Ø");
    static_assert(U == (P | nP), "(1) P | ~P collapses to the universe");
  }

  // (2) lambda predicate path (bypass BooleanEqPredicate)
  {
    constexpr auto Q = Set{in<U> | [](const bool& b) { return b; }};
    constexpr auto nQ = ~Q;
    static_assert(Ø<bool>{} == (Q & nQ), "(2) Q & ~Q collapses to Ø");
    static_assert(U == (Q | nQ), "(2) Q | ~Q collapses to the universe");
  }

  CHECK(true);
}
