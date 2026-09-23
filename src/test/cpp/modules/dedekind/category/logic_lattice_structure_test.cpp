/** @file test/cpp/modules/dedekind/category/logic_lattice_structure_test.cpp
 *
 * Downstream witnesses that the shipped logic species' De Morgan-tower position
 * (their :logic concepts) is BACKED by their carriers' position in the :total
 * lattice-ladder variety concepts.  The two partitions meet in @c category;
 * these asserts pin the bridge so the species cannot drift from the algebra
 * their carriers actually realise.
 *
 * Scope note: @c Kleene's carrier (the @c Ternary enum, i.e. @c Kleene::Ω) is
 * now registered in @c :total / @c :posetal (its order + bounds, #912), so it
 * joins the bridge below --- and, being finite, reaches the @b bounded rung
 * @c Chain<int> cannot.  The full @c IsAlgebraOnSet "palace" is still further
 * down the build chain and tracked separately.
 */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>
#include <utility>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE(
    "Logic species realise the :total lattice ladder (downstream witness)",
    "[category][logic][lattice][witness]") {
  SECTION("Boole is Boolean: bool is a Boolean algebra under (or, and, not)") {
    // The :logic species concept ...
    STATIC_REQUIRE(IsBooleanLogic<Boole>);
    // ... is backed by the :total carrier-level variety concept.
    STATIC_REQUIRE(
        IsBooleanAlgebra<Boole::Ω, std::logical_or<bool>,
                         std::logical_and<bool>, std::logical_not<bool>>);
  }

  SECTION(
      "Chain<int> is a bounded De Morgan chain: int is a distributive "
      "lattice under (Sup, Inf)") {
    STATIC_REQUIRE(IsBoundedDeMorganChain<Chain<int>>);
    // int is a distributive lattice under the value-returning Sup/Inf, but NOT
    // a bounded one (ℤ has no ⊥/⊤): no IsBoundedLattice<int, ...> here.
    STATIC_REQUIRE(IsDistributiveLattice<int, Sup, Inf>);
    // A chain wider than two grades is NOT complemented, matching the species.
    STATIC_REQUIRE(!IsBooleanLogic<Chain<int>>);
  }

  SECTION(
      "Kleene is a bounded De Morgan chain: Ternary is a distributive "
      "lattice under (Sup, Inf), and --- being finite --- a bounded one") {
    STATIC_REQUIRE(IsBoundedDeMorganChain<Kleene>);
    // Value-returning Sup/Inf (not the std::ranges::max/min niebloids, which
    // return const T& and so fail IsClosedUnder's same_as<T>): the honest
    // T×T→T ops that reach the whole ladder (#912; niebloid migration #934).
    STATIC_REQUIRE(IsDistributiveLattice<Ternary, Sup, Inf>);
    // K₃ is finite, so (unlike the unbounded Chain<int>) its carrier is a
    // BOUNDED lattice: ⊥ = False (∨-identity), ⊤ = True (∧-identity), #912.
    STATIC_REQUIRE(IsBoundedLattice<Ternary, Sup, Inf>);
    // Three grades, so NOT complemented --- matching the species (not Boolean).
    STATIC_REQUIRE(!IsBooleanLogic<Kleene>);
  }

  SECTION("value-level bridge: the species ops ARE the carrier's lattice ops") {
    // Exercises actual operations (not just concept constants): each species'
    // declared AND / OR / RFL computes exactly the carrier's :total lattice op.
    // Both operand orders and both Boolean values, so a wrong projection impl
    // (e.g. AND{return b;} / OR{return a;}) would fail rather than slip
    // through.
    for (bool a : {false, true}) {
      for (bool b : {false, true}) {
        CHECK(Boole::AND(a, b) == std::logical_and<bool>{}(a, b));
        CHECK(Boole::OR(a, b) == std::logical_or<bool>{}(a, b));
      }
      CHECK(Boole::RFL(a) == std::logical_not<bool>{}(a));
    }
    for (auto [x, y] : {std::pair{7, 3}, std::pair{3, 7}, std::pair{-5, 5}}) {
      CHECK(Chain<int>::AND(x, y) == Inf{}(x, y));
      CHECK(Chain<int>::OR(x, y) == Sup{}(x, y));
    }
    for (Ternary a : {Ternary::False, Ternary::Unknown, Ternary::True}) {
      for (Ternary b : {Ternary::False, Ternary::Unknown, Ternary::True}) {
        CHECK(Kleene::AND(a, b) == Inf{}(a, b));
        CHECK(Kleene::OR(a, b) == Sup{}(a, b));
      }
    }
    // The tower split, exercised at the value level: bool's ¬ is a genuine
    // complement (a ∧ ¬a = ⊥), int's is not (interior stays interior).
    CHECK((Boole::AND(true, Boole::RFL(true)) == Boole::False));
    CHECK((Chain<int>::AND(5, Chain<int>::RFL(5)) != Chain<int>::False));
  }
}
